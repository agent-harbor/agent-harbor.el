;;; agent-harbor-workspace.el --- Agent Harbor workspace re-rooting -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Model-C workspace re-rooting for Emacs.  The daemon owns branch metadata and
;; change-manifest computation; this module owns editor buffer state, dirty
;; buffer policy, and host-specific refresh after the selected task changes.

;;; Code:

(require 'cl-lib)
(require 'filenotify)
(require 'subr-x)
(require 'vc)
(require 'agent-harbor-models)
(require 'agent-harbor-rest)

(defgroup agent-harbor-workspace nil
  "Agent Harbor workspace re-rooting."
  :group 'agent-harbor)

(defcustom agent-harbor-on-switch-modified 'prompt
  "Policy for modified file buffers when switching task branches.

`prompt' asks for a decision per switch, `stash' stores dirty bytes through the
daemon stash endpoint, `keep' leaves dirty buffers untouched, and `kill' kills
dirty buffers after clearing their modified flag."
  :type '(choice (const :tag "Prompt" prompt)
                 (const :tag "Stash" stash)
                 (const :tag "Keep" keep)
                 (const :tag "Kill" kill))
  :group 'agent-harbor-workspace)

(defcustom agent-harbor-workspace-branch-root-function nil
  "Optional function mapping a branch alist to its local root path."
  :type 'function
  :group 'agent-harbor-workspace)

(defvar agent-harbor-workspace--current-task-id nil)
(defvar agent-harbor-workspace--current-branch-id nil)
(defvar agent-harbor-workspace--current-root nil)
(defvar agent-harbor-workspace--last-retarget nil)
(defvar agent-harbor-workspace--retarget-call-count 0)
(defvar agent-harbor-workspace--watch-cycle-count 0)
(defvar agent-harbor-workspace--stashed-paths (make-hash-table :test #'equal))
(defvar agent-harbor-workspace--managed-watch-descriptors nil)
(defvar agent-harbor-workspace-prompt-function nil
  "Optional function used to choose a modified-buffer switch action.
The function is called with no arguments and must return one of `stash',
`keep', `kill', or `abort'.")

(defvar agent-harbor-after-reroot-hook nil
  "Hook run after Agent Harbor re-roots buffers.
Each function is called with (NEW-TASK-ID MANIFEST).")

(defun agent-harbor-workspace--get (key alist)
  "Return KEY from ALIST, accepting string and symbol keys."
  (agent-harbor-models--alist-get key alist))

(defun agent-harbor-workspace--normalize-directory (path)
  "Return PATH as an expanded directory name."
  (file-name-as-directory (expand-file-name path)))

(defun agent-harbor-workspace--file-in-root-p (file root)
  "Return non-nil when FILE is under ROOT."
  (when (and file root)
    (let ((relative (file-relative-name (expand-file-name file)
                                        (agent-harbor-workspace--normalize-directory root))))
      (not (or (string-prefix-p "../" relative)
               (string= relative "..")
               (file-name-absolute-p relative))))))

(defun agent-harbor-workspace--relative-path (file root)
  "Return FILE relative to ROOT using slash separators."
  (replace-regexp-in-string
   "\\`[.]/" ""
   (file-relative-name (expand-file-name file)
                       (agent-harbor-workspace--normalize-directory root))))

(defun agent-harbor-workspace--join (root relative)
  "Return ROOT joined with RELATIVE."
  (expand-file-name relative (agent-harbor-workspace--normalize-directory root)))

(defun agent-harbor-workspace--branch-session-id (branch)
  "Return session id from BRANCH."
  (agent-harbor-workspace--get "sessionId" branch))

(defun agent-harbor-workspace--branch-updated-at (branch)
  "Return updated-at string from BRANCH."
  (or (agent-harbor-workspace--get "updatedAt" branch) ""))

(defun agent-harbor-workspace-sandboxed-p ()
  "Return non-nil when this Emacs process is in Agent Harbor sandboxed mode."
  (equal (getenv "AH_SANDBOXED") "1"))

(defun agent-harbor-workspace--latest-branch (session-ids)
  "Return latest branch alist for SESSION-IDS."
  (let (branches)
    (dolist (session-id session-ids)
      (setq branches (append branches (agent-harbor-rest-list-branches-raw session-id))))
    (car (sort branches
               (lambda (left right)
                 (string< (agent-harbor-workspace--branch-updated-at right)
                          (agent-harbor-workspace--branch-updated-at left)))))))

(defun agent-harbor-workspace-branch-root (branch)
  "Return local root path for BRANCH."
  (or (and agent-harbor-workspace-branch-root-function
           (funcall agent-harbor-workspace-branch-root-function branch))
      (agent-harbor-workspace--get "workspacePath" branch)
      (agent-harbor-workspace--get "workspace_path" branch)))

(defun agent-harbor-workspace-resolve-branch (task-id session-ids &optional branch-id)
  "Resolve TASK-ID SESSION-IDS and optional BRANCH-ID to branch metadata."
  (or (when branch-id
        (cl-loop for session-id in session-ids
                 append (agent-harbor-rest-list-branches-raw session-id)
                 into branches
                 finally return
                 (cl-find branch-id branches
                          :key (lambda (branch)
                                 (agent-harbor-workspace--get "id" branch))
                          :test #'equal)))
      (agent-harbor-workspace--latest-branch session-ids)
      (signal 'user-error (list (format "No AgentHarborFS branch found for task %s" task-id)))))

(defun agent-harbor-workspace--manifest-changes (manifest)
  "Return change entries from MANIFEST."
  (or (agent-harbor-workspace--get "changes" manifest) nil))

(defun agent-harbor-workspace--change-path (change)
  "Return path from manifest CHANGE."
  (agent-harbor-workspace--get "path" change))

(defun agent-harbor-workspace--change-kind (change)
  "Return normalized kind from manifest CHANGE."
  (let ((case-fold-search nil)
        (kind (format "%s" (agent-harbor-workspace--get "kind" change))))
    (downcase (replace-regexp-in-string
               "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" kind))))

(defun agent-harbor-workspace--path-matches-change-p (relative change)
  "Return non-nil when RELATIVE is affected by CHANGE."
  (let ((path (agent-harbor-workspace--change-path change))
        (kind (agent-harbor-workspace--change-kind change)))
    (cond
     ((null path) nil)
     ((equal kind "tree-replaced")
      (or (equal relative path)
          (string-prefix-p (file-name-as-directory path) relative)))
     (t (equal relative path)))))

(defun agent-harbor-workspace--change-for-relative (relative manifest)
  "Return manifest change affecting RELATIVE."
  (cl-find-if
   (lambda (change)
     (agent-harbor-workspace--path-matches-change-p relative change))
   (agent-harbor-workspace--manifest-changes manifest)))

(defun agent-harbor-workspace--root-file-buffers (root)
  "Return file buffers whose visited file is under ROOT."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (agent-harbor-workspace--file-in-root-p buffer-file-name root)))
   (buffer-list)))

(defun agent-harbor-workspace--modified-root-buffers (root)
  "Return modified file buffers under ROOT."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (and (buffer-modified-p)
            (agent-harbor-workspace--file-in-root-p buffer-file-name root))))
   (buffer-list)))

(defun agent-harbor-workspace--prompt-action ()
  "Prompt for a modified-buffer switch action."
  (let ((buffer (get-buffer-create "*Agent Harbor Switch Modified*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Modified buffers are open in the current Agent Harbor branch.\n\n")
        (insert "s: stash and switch\n")
        (insert "k: keep buffers\n")
        (insert "d: kill buffers\n")
        (insert "a: abort switch\n")
        (setq buffer-read-only t)))
    (display-buffer buffer)
    (if agent-harbor-workspace-prompt-function
        (funcall agent-harbor-workspace-prompt-function)
      (pcase (read-char-choice
              "Modified buffers: [s] stash and switch, [k] keep, [d] kill, [a] abort: "
              '(?s ?k ?d ?a))
        (?s 'stash)
        (?k 'keep)
        (?d 'kill)
        (_ 'abort)))))

(defun agent-harbor-workspace--policy-action (modified-buffers)
  "Return policy action for MODIFIED-BUFFERS."
  (if (null modified-buffers)
      'none
    (pcase agent-harbor-on-switch-modified
      ('prompt (agent-harbor-workspace--prompt-action))
      ('stash 'stash)
      ('keep 'keep)
      ('kill 'kill)
      (_ (signal 'user-error
                 (list (format "Unsupported agent-harbor-on-switch-modified value: %s"
                               agent-harbor-on-switch-modified)))))))

(defun agent-harbor-workspace--record-stashed-path (task-id relative)
  "Remember that TASK-ID has a stashed RELATIVE path."
  (let ((paths (copy-sequence (gethash task-id agent-harbor-workspace--stashed-paths))))
    (unless (member relative paths)
      (puthash task-id (cons relative paths) agent-harbor-workspace--stashed-paths))))

(defun agent-harbor-workspace--stash-buffer (task-id root buffer)
  "Stash BUFFER under TASK-ID using ROOT-relative path."
  (with-current-buffer buffer
    (let* ((relative (agent-harbor-workspace--relative-path buffer-file-name root))
           (bytes (encode-coding-string
                   (buffer-substring-no-properties (point-min) (point-max))
                   buffer-file-coding-system)))
      (agent-harbor-rest-stash-buffer task-id relative bytes)
      (agent-harbor-workspace--record-stashed-path task-id relative)
      relative)))

(defun agent-harbor-workspace--handle-modified-buffers (task-id root)
  "Apply modified-buffer policy for TASK-ID buffers under ROOT."
  (let* ((buffers (agent-harbor-workspace--modified-root-buffers root))
         (action (agent-harbor-workspace--policy-action buffers)))
    (pcase action
      ('none nil)
      ('abort (signal 'quit '("Agent Harbor task switch aborted")))
      ('keep nil)
      ('stash
       (dolist (buffer buffers)
         (agent-harbor-workspace--stash-buffer task-id root buffer)
         (with-current-buffer buffer
           (set-buffer-modified-p nil))))
      ('kill
       (dolist (buffer buffers)
         (with-current-buffer buffer
           (set-buffer-modified-p nil))
         (kill-buffer buffer))))))

(defun agent-harbor-workspace--retarget-buffer-file (buffer old-root new-root relative)
  "Move BUFFER's visited file from OLD-ROOT to NEW-ROOT at RELATIVE."
  (with-current-buffer buffer
    (let ((new-file (agent-harbor-workspace--join new-root relative)))
      (when (agent-harbor-workspace--file-in-root-p buffer-file-name old-root)
        (set-visited-file-name new-file t t)
        (setq default-directory
              (agent-harbor-workspace--normalize-directory new-root))))))

(defun agent-harbor-workspace--revert-buffer-from-disk (buffer)
  "Revert BUFFER from disk when its file still exists."
  (with-current-buffer buffer
    (when (and buffer-file-name (file-regular-p buffer-file-name))
      (let ((revert-without-query (list (regexp-quote buffer-file-name))))
        (revert-buffer :ignore-auto :noconfirm :preserve-modes)))))

(defun agent-harbor-workspace--retarget-and-revert-clean-buffer
    (buffer old-root new-root relative)
  "Retarget clean BUFFER to RELATIVE under NEW-ROOT and revert it.
Kill BUFFER when the new branch no longer has a regular file at RELATIVE so
old branch bytes cannot remain visible after manifest-driven re-rooting."
  (let ((new-file (agent-harbor-workspace--join new-root relative)))
    (if (file-regular-p new-file)
        (progn
          (agent-harbor-workspace--retarget-buffer-file
           buffer old-root new-root relative)
          (agent-harbor-workspace--revert-buffer-from-disk buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun agent-harbor-workspace--apply-clean-buffer-change
    (buffer old-root new-root relative change)
  "Apply manifest CHANGE to clean BUFFER from OLD-ROOT to NEW-ROOT at RELATIVE."
  (let ((kind (agent-harbor-workspace--change-kind change)))
    (pcase kind
      ("deleted"
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))
      (_
       (agent-harbor-workspace--retarget-and-revert-clean-buffer
        buffer old-root new-root relative)))))

(defun agent-harbor-workspace--retarget-unaffected-clean-buffer
    (buffer old-root new-root relative)
  "Retarget clean BUFFER from OLD-ROOT to NEW-ROOT at RELATIVE."
  (agent-harbor-workspace--retarget-buffer-file buffer old-root new-root relative))

(defun agent-harbor-workspace--apply-manifest-to-buffers (old-root new-root manifest)
  "Apply MANIFEST to clean buffers under OLD-ROOT and NEW-ROOT."
  (dolist (buffer (agent-harbor-workspace--root-file-buffers old-root))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((relative (agent-harbor-workspace--relative-path buffer-file-name old-root))
               (change (agent-harbor-workspace--change-for-relative relative manifest)))
          (when (not (buffer-modified-p))
            (if change
                (agent-harbor-workspace--apply-clean-buffer-change
                 buffer old-root new-root relative change)
              (agent-harbor-workspace--retarget-unaffected-clean-buffer
               buffer old-root new-root relative))))))))

(defun agent-harbor-workspace--restore-stash (task-id new-root)
  "Restore stashed dirty buffers for TASK-ID into NEW-ROOT."
  (dolist (relative (copy-sequence (gethash task-id agent-harbor-workspace--stashed-paths)))
    (condition-case nil
        (let ((bytes (agent-harbor-rest-get-stash-buffer task-id relative)))
          (find-file (agent-harbor-workspace--join new-root relative))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (decode-coding-string bytes buffer-file-coding-system))
            (set-buffer-modified-p t)))
      (error nil))))

(defun agent-harbor-workspace--update-default-directories (old-root new-root)
  "Replace OLD-ROOT with NEW-ROOT in buffer default directories."
  (setq-default default-directory (agent-harbor-workspace--normalize-directory new-root))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (agent-harbor-workspace--file-in-root-p default-directory old-root)
        (setq default-directory
              (agent-harbor-workspace--join
               new-root
               (agent-harbor-workspace--relative-path default-directory old-root)))))))

(defun agent-harbor-workspace--watch-path (watch)
  "Best-effort path extraction for file notification WATCH."
  (cond
   ((consp watch) (cdr watch))
   (t nil)))

(defun agent-harbor-workspace--remove-watch (watch)
  "Remove file notification WATCH, ignoring stale descriptors."
  (condition-case nil
      (file-notify-rm-watch watch)
    (error nil)))

(defun agent-harbor-workspace--cycle-file-notify-watches (old-root new-root)
  "Remove file notification watches associated with OLD-ROOT or NEW-ROOT."
  (setq agent-harbor-workspace--watch-cycle-count
        (1+ agent-harbor-workspace--watch-cycle-count))
  (dolist (watch (copy-sequence agent-harbor-workspace--managed-watch-descriptors))
    (agent-harbor-workspace--remove-watch watch))
  (setq agent-harbor-workspace--managed-watch-descriptors nil)
  (when (boundp 'file-notify-descriptors)
    (let ((descriptors (symbol-value 'file-notify-descriptors)))
      (cond
       ((hash-table-p descriptors)
        (maphash
         (lambda (watch value)
           (let ((path (or (agent-harbor-workspace--watch-path value)
                           (agent-harbor-workspace--watch-path watch))))
             (when (or (agent-harbor-workspace--file-in-root-p path old-root)
                       (agent-harbor-workspace--file-in-root-p path new-root))
               (agent-harbor-workspace--remove-watch watch))))
         descriptors))
       ((listp descriptors)
        (dolist (entry descriptors)
          (let ((watch (car entry))
                (path (agent-harbor-workspace--watch-path entry)))
            (when (or (agent-harbor-workspace--file-in-root-p path old-root)
                      (agent-harbor-workspace--file-in-root-p path new-root))
              (agent-harbor-workspace--remove-watch watch)))))))))

(defun agent-harbor-workspace-register-watch (descriptor)
  "Register file-notify DESCRIPTOR for cycling during Agent Harbor re-root."
  (push descriptor agent-harbor-workspace--managed-watch-descriptors)
  descriptor)

(defun agent-harbor-workspace--refresh-vc-and-magit (new-root)
  "Refresh VC and Magit state for NEW-ROOT."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or (agent-harbor-workspace--file-in-root-p buffer-file-name new-root)
                (agent-harbor-workspace--file-in-root-p default-directory new-root))
        (ignore-errors (vc-refresh-state)))))
  (when (fboundp 'magit-refresh)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (string-match-p "\\`magit" (symbol-name major-mode))
          (ignore-errors (magit-refresh)))))))

(defun agent-harbor-workspace--initial-reroot (task-id branch-id new-root)
  "Initialize Agent Harbor workspace state to TASK-ID BRANCH-ID NEW-ROOT."
  (setq agent-harbor-workspace--current-task-id task-id
        agent-harbor-workspace--current-branch-id branch-id
        agent-harbor-workspace--current-root
        (agent-harbor-workspace--normalize-directory new-root)
        agent-harbor-workspace--last-retarget
        `((status . "not-requested")
          (message . "initial focus only")
          (retargeted . :false)
          (fallback . :false)
          (action . "initialized-current-task")))
  (setq-default default-directory agent-harbor-workspace--current-root)
  (run-hook-with-args 'agent-harbor-after-reroot-hook task-id nil))

(defun agent-harbor-workspace--request-manifest
    (new-task-id current-branch-id target-branch-id old-root target-root)
  "Return the sandbox-diff manifest response for NEW-TASK-ID.
The retired OS-level sandbox-retarget endpoint is not part of supported editor
focus flows, including sandbox-launched editor sessions."
  (let* ((response (agent-harbor-rest-sandbox-diff
                    new-task-id current-branch-id target-branch-id))
         (status (format "%s"
                         (or (agent-harbor-workspace--get "retarget_status" response)
                             (agent-harbor-workspace--get "retargetStatus" response)
                             "not-requested")))
         (message (or (agent-harbor-workspace--get "retarget_message" response)
                      (agent-harbor-workspace--get "retargetMessage" response)
                      "sandbox-diff manifest only"))
         (retargeted (eq (agent-harbor-workspace--get "retargeted" response) t))
         (effective-root (if retargeted old-root target-root)))
    (setq agent-harbor-workspace--last-retarget
          `((status . ,status)
            (message . ,message)
            (retargeted . ,(if retargeted t :false))
            (fallback . :false)
            (action . ,(cond
                        (retargeted "retargeted")
                        (t "sandbox-diff")))
            (fromBranchId . ,current-branch-id)
            (toBranchId . ,target-branch-id)
            (oldRoot . ,old-root)
            (targetRoot . ,target-root)
            (effectiveRoot . ,effective-root)))
    (cons `(effectiveRoot . ,effective-root) response)))

;;;###autoload
(defun agent-harbor-workspace-reroot (new-task-id session-ids &optional new-branch-id)
  "Re-root Emacs workspace state to NEW-TASK-ID.
SESSION-IDS must come from daemon task/session responses.  Optional
NEW-BRANCH-ID narrows branch resolution when the sidebar already has branch
metadata."
  (let* ((branch (agent-harbor-workspace-resolve-branch
                  new-task-id session-ids new-branch-id))
         (target-branch-id (agent-harbor-workspace--get "id" branch))
         (target-root (agent-harbor-workspace-branch-root branch)))
    (unless (and target-branch-id target-root)
      (signal 'user-error (list (format "Branch metadata for task %s is incomplete" new-task-id))))
    (if (or (null agent-harbor-workspace--current-branch-id)
            (null agent-harbor-workspace--current-root))
        (agent-harbor-workspace--initial-reroot new-task-id target-branch-id target-root)
      (unless (equal target-branch-id agent-harbor-workspace--current-branch-id)
        (let* ((old-task-id agent-harbor-workspace--current-task-id)
               (old-root agent-harbor-workspace--current-root)
               (manifest (agent-harbor-workspace--request-manifest
                          new-task-id
                          agent-harbor-workspace--current-branch-id
                          target-branch-id
                          old-root
                          target-root))
               (effective-root (agent-harbor-workspace--get "effectiveRoot" manifest)))
          (agent-harbor-workspace--handle-modified-buffers old-task-id old-root)
          (agent-harbor-workspace--cycle-file-notify-watches old-root effective-root)
          (agent-harbor-workspace--apply-manifest-to-buffers
           old-root effective-root manifest)
          (agent-harbor-workspace--update-default-directories old-root effective-root)
          (setq agent-harbor-workspace--current-task-id new-task-id
                agent-harbor-workspace--current-branch-id target-branch-id
                agent-harbor-workspace--current-root
                (agent-harbor-workspace--normalize-directory effective-root))
          (agent-harbor-workspace--restore-stash new-task-id effective-root)
          (agent-harbor-workspace--refresh-vc-and-magit effective-root)
          (run-hook-with-args 'agent-harbor-after-reroot-hook new-task-id manifest))))
    `((taskId . ,agent-harbor-workspace--current-task-id)
      (branchId . ,agent-harbor-workspace--current-branch-id)
      (root . ,agent-harbor-workspace--current-root))))

(defun agent-harbor-workspace-current-state ()
  "Return current Agent Harbor workspace state."
  `((taskId . ,agent-harbor-workspace--current-task-id)
    (branchId . ,agent-harbor-workspace--current-branch-id)
    (root . ,agent-harbor-workspace--current-root)
    (sandboxed . ,(if (agent-harbor-workspace-sandboxed-p) t :false))
    (retargetCallCount . ,agent-harbor-workspace--retarget-call-count)
    (watchCycleCount . ,agent-harbor-workspace--watch-cycle-count)
    (lastRetarget . ,agent-harbor-workspace--last-retarget)
    (stashed . ,(let (items)
                  (maphash
                   (lambda (task paths)
                     (push `(,task . ,(vconcat paths)) items))
                   agent-harbor-workspace--stashed-paths)
                  items))))

(provide 'agent-harbor-workspace)

;;; agent-harbor-workspace.el ends here
