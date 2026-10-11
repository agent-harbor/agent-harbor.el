;;; agent-harbor-scm.el --- Agent Harbor SCM integration -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Magit and VC-facing views of daemon-owned Agent Harbor changes.  The daemon
;; remains the source of truth for modified-file metadata and base/head snapshot
;; hashes; Magit is only the presentation and key-dispatch surface.

;;; Code:

(require 'cl-lib)
(require 'ediff)
(require 'subr-x)
(require 'vc)
(require 'magit-section nil t)
(require 'agent-harbor-daemon)
(require 'agent-harbor-models)
(require 'agent-harbor-rest)
(require 'agent-harbor-workspace)

(declare-function magit-refresh "ext:magit")
(declare-function magit-status-setup-buffer "ext:magit")
(declare-function magit-toplevel "ext:magit")
(declare-function magit-insert-heading "ext:magit-section")
(declare-function magit-insert-section-body "ext:magit-section")
(declare-function magit-insert-section--create "ext:magit-section")
(declare-function magit-insert-section--finish "ext:magit-section")
(declare-function magit-section--set-section-properties "ext:magit-section")
(declare-function magit-section-lineage "ext:magit-section")
(declare-function magit-section-maybe-remove-heading-map "ext:magit-section")
(declare-function magit-section-maybe-remove-visibility-indicator "ext:magit-section")
(declare-function eieio-oref "eieio")
(declare-function eieio-oset "eieio")
(defvar magit-insert-section--current)
(defvar magit-insert-section--oldroot)
(defvar magit-insert-section--parent)
(defvar magit-root-section)
(defvar magit-status-mode-map)
(defvar magit-status-sections-hook)

(defun agent-harbor-scm--magit-slot (section slot)
  "Return Magit SECTION's SLOT value."
  (eieio-oref section slot))

(defun agent-harbor-scm--magit-set-slot (section slot value)
  "Set Magit SECTION's SLOT to VALUE."
  (eieio-oset section slot value))

(eval-when-compile
  ;; Keep Magit optional for minimal byte-compile runs while preserving Magit's
  ;; section semantics in the compiled code.  These fallbacks mirror the macro
  ;; expansions from magit-section.el:
  ;; https://github.com/magit/magit/blob/main/lisp/magit-section.el
  (unless (fboundp 'magit-insert-section)
    (defmacro magit-insert-section (&rest args)
      "Compile-time fallback for `magit-insert-section'."
      (declare (indent 1)
               (debug ([&optional symbolp]
                       (&or [("eval" form) &optional form form &rest form]
                            [symbolp &optional form form &rest form])
                       body)))
      (pcase-let* ((bind (and (symbolp (car args))
                              (pop args)))
                   (`((,class ,value ,hide . ,args) . ,body) args)
                   (obj (gensym "section")))
        `(let* ((,obj (magit-insert-section--create
                       ,(if (eq (car-safe class) 'eval) (cadr class) `',class)
                       ,value ,hide ,@args))
                (magit-insert-section--current ,obj)
                (magit-insert-section--oldroot
                 (or magit-insert-section--oldroot
                     (and (not magit-insert-section--parent)
                          (prog1 magit-root-section
                            (setq magit-root-section ,obj)))))
                (magit-insert-section--parent ,obj))
           (catch 'cancel-section
             ,@(if bind `((let ((,bind ,obj)) ,@body)) body)
             (magit-insert-section--finish ,obj))
           ,obj))))
  (unless (fboundp 'magit-insert-section-body)
    (defmacro magit-insert-section-body (&rest body)
      "Compile-time fallback for `magit-insert-section-body'."
      (declare (indent 0))
      (let ((f (gensym))
            (s (gensym))
            (lineage (gensym)))
        `(let ((,f (lambda () ,@body)))
           (if (agent-harbor-scm--magit-slot magit-insert-section--current 'hidden)
               (agent-harbor-scm--magit-set-slot
                magit-insert-section--current 'washer
                (let ((,s magit-insert-section--current))
                  (lambda ()
                    (let ((,lineage (magit-section-lineage ,s t)))
                      (dolist (s ,lineage)
                        (set-marker-insertion-type
                         (agent-harbor-scm--magit-slot s 'end) t))
                      (funcall ,f)
                      (dolist (s ,lineage)
                        (set-marker-insertion-type
                         (agent-harbor-scm--magit-slot s 'end) nil))
                      (magit-section--set-section-properties ,s)
                      (magit-section-maybe-remove-heading-map ,s)
                      (magit-section-maybe-remove-visibility-indicator ,s)))))
             (funcall ,f)))))))

(defgroup agent-harbor-scm nil
  "Agent Harbor source-control integration."
  :group 'agent-harbor)

(defcustom agent-harbor-scm-buffer-name "*Agent Harbor: Changes*"
  "Name of the Agent Harbor vc fallback buffer."
  :type 'string
  :group 'agent-harbor-scm)

(defvar-local agent-harbor-scm--state nil)
(defvar-local agent-harbor-scm--checkpoint-session-id nil)
(defvar-local agent-harbor-scm--checkpoint-refresh-buffer nil)
(defvar-local agent-harbor-scm--checkpoint-response nil)
(defvar agent-harbor-scm-last-checkpoint-response nil
  "Most recent checkpoint response, exposed for batch integration tests.")

(defvar agent-harbor-scm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'agent-harbor-scm-refresh)
    (define-key map (kbd "RET") #'agent-harbor-scm-open-file)
    (define-key map (kbd "d") #'agent-harbor-scm-ediff-file)
    (define-key map (kbd "c") #'agent-harbor-scm-open-checkpoint-buffer)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `agent-harbor-scm-mode'.")

(define-derived-mode agent-harbor-scm-mode special-mode "Agent Harbor Changes"
  "Major mode for Agent Harbor SCM fallback buffers."
  (setq-local truncate-lines nil))

(defvar agent-harbor-scm-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-harbor-scm-commit-submit)
    (define-key map (kbd "C-c C-k") #'agent-harbor-scm-commit-cancel)
    map)
  "Keymap for Agent Harbor checkpoint message buffers.")

(define-derived-mode agent-harbor-scm-commit-mode text-mode "Agent Harbor Commit"
  "Major mode for Agent Harbor checkpoint messages.")

(defun agent-harbor-scm--get (key alist)
  "Return KEY from ALIST, accepting string and symbol keys."
  (agent-harbor-models--alist-get key alist))

(defun agent-harbor-scm--snapshot-id (snapshot)
  "Return SNAPSHOT id."
  (agent-harbor-scm--get "id" snapshot))

(defun agent-harbor-scm--normalize-directory (path)
  "Return PATH as an expanded directory name."
  (file-name-as-directory (expand-file-name path)))

(defun agent-harbor-scm--file-in-root-p (file root)
  "Return non-nil when FILE is under ROOT."
  (when (and file root)
    (let ((relative (file-relative-name (expand-file-name file)
                                        (agent-harbor-scm--normalize-directory root))))
      (not (or (string-prefix-p "../" relative)
               (string= relative "..")
               (file-name-absolute-p relative))))))

(defun agent-harbor-scm--current-root ()
  "Return the current repository/workspace root."
  (or (and (fboundp 'magit-toplevel)
           (ignore-errors (magit-toplevel)))
      (ignore-errors (vc-root-dir))
      default-directory))

(defun agent-harbor-scm--branch-root (branch)
  "Return local workspace root from BRANCH."
  (or (agent-harbor-scm--get "workspacePath" branch)
      (agent-harbor-scm--get "workspace_path" branch)))

(defun agent-harbor-scm--branch-session-id (branch)
  "Return session id from BRANCH."
  (or (agent-harbor-scm--get "sessionId" branch)
      (agent-harbor-scm--get "session_id" branch)))

(defun agent-harbor-scm--branch-id (branch)
  "Return id from BRANCH."
  (agent-harbor-scm--get "id" branch))

(defun agent-harbor-scm--branch-updated-at (branch)
  "Return updated-at string from BRANCH."
  (or (agent-harbor-scm--get "updatedAt" branch)
      (agent-harbor-scm--get "updated_at" branch)
      ""))

(defun agent-harbor-scm--sort-latest-branch-first (branches)
  "Return BRANCHES sorted by daemon update time descending."
  (sort (copy-sequence branches)
        (lambda (left right)
          (string< (agent-harbor-scm--branch-updated-at right)
                   (agent-harbor-scm--branch-updated-at left)))))

(defun agent-harbor-scm--selected-session-id ()
  "Return the sidebar-selected session id when available."
  (and (fboundp 'agent-harbor-sidebar--selected-session-id)
       (agent-harbor-sidebar--selected-session-id)))

(defun agent-harbor-scm--selected-branch-id ()
  "Return the workspace/sidebar-selected branch id when available."
  (or (and (boundp 'agent-harbor-workspace--current-branch-id)
           agent-harbor-workspace--current-branch-id)
      (and (fboundp 'agent-harbor-sidebar--selected-branch-id)
           (agent-harbor-sidebar--selected-branch-id))))

(defun agent-harbor-scm--branch-for-root (root branches)
  "Return the daemon branch from BRANCHES that owns ROOT."
  (let ((normalized-root (and root (agent-harbor-scm--normalize-directory root))))
    (or (when-let ((selected (agent-harbor-scm--selected-branch-id)))
          (cl-find selected branches :key #'agent-harbor-scm--branch-id :test #'equal))
        (cl-find-if
         (lambda (branch)
           (when-let ((branch-root (agent-harbor-scm--branch-root branch)))
             (or (equal normalized-root (agent-harbor-scm--normalize-directory branch-root))
                 (agent-harbor-scm--file-in-root-p normalized-root branch-root)
                 (agent-harbor-scm--file-in-root-p branch-root normalized-root))))
         branches))))

(defun agent-harbor-scm--task-id-for-session (session-id)
  "Return daemon task id that owns SESSION-ID, when discoverable."
  (catch 'found
    (dolist (task (agent-harbor-rest-list-tasks-raw))
      (when-let ((task-id (agent-harbor-scm--get "id" task)))
        (dolist (session (agent-harbor-rest-task-sessions task-id))
          (when (equal (agent-harbor-scm--get "id" session) session-id)
            (throw 'found task-id)))))
    nil))

(defun agent-harbor-scm--session-state (session-id root branch)
  "Build an SCM state for SESSION-ID rooted at ROOT using BRANCH metadata."
  (let* ((files-response (agent-harbor-rest-session-files session-id))
         (items (or (agent-harbor-scm--get "items" files-response) nil))
         (base (or (agent-harbor-scm--get "baseSnapshot" files-response)
                   (agent-harbor-scm--get "base_snapshot" files-response)
                   (agent-harbor-scm--get "baseSnapshot" branch)
                   (agent-harbor-scm--get "base_snapshot" branch)))
         (snapshot (or (agent-harbor-scm--get "snapshot" files-response)
                       (agent-harbor-scm--get "headSnapshot" branch)
                       (agent-harbor-scm--get "head_snapshot" branch)))
         (branch-root (or (agent-harbor-scm--branch-root branch) root)))
    `((sessionId . ,session-id)
      (taskId . ,(agent-harbor-scm--task-id-for-session session-id))
      (branchId . ,(agent-harbor-scm--branch-id branch))
      (root . ,(and branch-root (agent-harbor-scm--normalize-directory branch-root)))
      (baseSnapshot . ,base)
      (snapshot . ,snapshot)
      (files . ,(vconcat (sort items
                               (lambda (left right)
                                 (string< (or (agent-harbor-scm--get "path" left) "")
                                          (or (agent-harbor-scm--get "path" right) "")))))))))

(defun agent-harbor-scm-current-state (&optional root)
  "Return daemon-backed SCM state for ROOT or the current repository."
  (agent-harbor-daemon-refresh)
  (let* ((root (or root (agent-harbor-scm--current-root)))
         (branches (agent-harbor-scm--sort-latest-branch-first
                    (agent-harbor-rest-list-branches-raw)))
         (branch (agent-harbor-scm--branch-for-root root branches))
         (session-id (or (and branch (agent-harbor-scm--branch-session-id branch))
                         (agent-harbor-scm--selected-session-id))))
    (when session-id
      (unless branch
        (setq branch (car (agent-harbor-scm--sort-latest-branch-first
                           (agent-harbor-rest-list-branches-raw session-id)))))
      (agent-harbor-scm--session-state session-id root branch))))

(defun agent-harbor-scm--files (state)
  "Return file metadata vector from STATE as a list."
  (append (agent-harbor-scm--get "files" state) nil))

(defun agent-harbor-scm--file-path (file)
  "Return FILE path."
  (agent-harbor-scm--get "path" file))

(defun agent-harbor-scm--file-status (file)
  "Return FILE status."
  (or (agent-harbor-scm--get "status" file) "modified"))

(defun agent-harbor-scm--insert-file-row (file state)
  "Insert FILE row carrying STATE text properties."
  (let ((start (point))
        (path (agent-harbor-scm--file-path file)))
    (insert (format "  %-10s %s\n" (agent-harbor-scm--file-status file) path))
    (add-text-properties
     start (point)
     `(agent-harbor-scm-file ,file
       agent-harbor-scm-state ,state
       mouse-face highlight
       read-only t))))

(defun agent-harbor-scm--insert-summary (state)
  "Insert branch/hash summary for STATE."
  (let ((base (agent-harbor-scm--get "baseSnapshot" state))
        (snapshot (agent-harbor-scm--get "snapshot" state)))
    (insert (format "Session: %s\n" (agent-harbor-scm--get "sessionId" state)))
    (when-let ((task-id (agent-harbor-scm--get "taskId" state)))
      (insert (format "Task: %s\n" task-id)))
    (when-let ((branch-id (agent-harbor-scm--get "branchId" state)))
      (insert (format "Branch: %s\n" branch-id)))
    (when base
      (insert (format "Base: %s  %s\n"
                      (agent-harbor-scm--snapshot-id base)
                      (or (agent-harbor-scm--get "contentId" base)
                          (agent-harbor-scm--get "content_id" base)
                          ""))))
    (when snapshot
      (insert (format "Head: %s  %s\n"
                      (agent-harbor-scm--snapshot-id snapshot)
                      (or (agent-harbor-scm--get "contentId" snapshot)
                          (agent-harbor-scm--get "content_id" snapshot)
                          ""))))))

;;;###autoload
(defun agent-harbor-scm-open ()
  "Open the Magit-free Agent Harbor changes buffer."
  (interactive)
  (let ((buffer (get-buffer-create agent-harbor-scm-buffer-name)))
    (with-current-buffer buffer
      (agent-harbor-scm-mode)
      (agent-harbor-scm-refresh))
    (pop-to-buffer buffer)))

(defun agent-harbor-scm-refresh ()
  "Refresh the Agent Harbor changes buffer."
  (interactive)
  (let ((state (agent-harbor-scm-current-state)))
    (setq agent-harbor-scm--state state)
    (let ((inhibit-read-only t)
          (point-was (point)))
      (erase-buffer)
      (insert "Agent Harbor\n\n")
      (if (not state)
          (insert "No Agent Harbor branch metadata for this workspace.\n")
        (agent-harbor-scm--insert-summary state)
        (insert "\nChanges\n")
        (if-let ((files (agent-harbor-scm--files state)))
            (dolist (file files)
              (agent-harbor-scm--insert-file-row file state))
          (insert "  No daemon-reported changes.\n"))
        (insert "\nRET open  d ediff  c checkpoint  g refresh\n"))
      (goto-char (min (point-max) point-was)))))

(defun agent-harbor-scm--row-state ()
  "Return SCM state at point."
  (or (get-text-property (point) 'agent-harbor-scm-state)
      agent-harbor-scm--state
      (agent-harbor-scm-current-state)))

(defun agent-harbor-scm--row-file ()
  "Return SCM file metadata at point."
  (get-text-property (point) 'agent-harbor-scm-file))

(defun agent-harbor-scm--workspace-file (state file)
  "Return absolute workspace file path for FILE in STATE."
  (expand-file-name (agent-harbor-scm--file-path file)
                    (agent-harbor-scm--get "root" state)))

(defun agent-harbor-scm-open-file ()
  "Open the Agent Harbor file at point."
  (interactive)
  (let ((state (agent-harbor-scm--row-state))
        (file (agent-harbor-scm--row-file)))
    (unless (and state file)
      (user-error "No Agent Harbor file at point"))
    (find-file (agent-harbor-scm--workspace-file state file))))

(defun agent-harbor-scm--buffer-from-bytes (name bytes)
  "Return a buffer named NAME containing BYTES."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (insert (decode-coding-string bytes 'utf-8 t))
      (set-buffer-modified-p nil))
    buffer))

(defun agent-harbor-scm--snapshot-content-buffer (state file snapshot-key label)
  "Return a content buffer for FILE at STATE's SNAPSHOT-KEY with LABEL."
  (let* ((session-id (agent-harbor-scm--get "sessionId" state))
         (snapshot (agent-harbor-scm--get snapshot-key state))
         (snapshot-id (agent-harbor-scm--snapshot-id snapshot))
         (path (agent-harbor-scm--file-path file))
         (bytes (agent-harbor-rest-session-file-content session-id path snapshot-id)))
    (agent-harbor-scm--buffer-from-bytes
     (format "*Agent Harbor %s: %s*" label path)
     bytes)))

(defun agent-harbor-scm-ediff-file ()
  "Ediff the file at point against the daemon base snapshot."
  (interactive)
  (let ((state (agent-harbor-scm--row-state))
        (file (agent-harbor-scm--row-file)))
    (unless (and state file)
      (user-error "No Agent Harbor file at point"))
    (let* ((base-buffer (agent-harbor-scm--snapshot-content-buffer state file "baseSnapshot" "base"))
           (workspace-file (agent-harbor-scm--workspace-file state file))
           (head-buffer (if (file-regular-p workspace-file)
                            (find-file-noselect workspace-file)
                          (agent-harbor-scm--snapshot-content-buffer state file "snapshot" "head"))))
      (ediff-buffers base-buffer head-buffer))))

(defun agent-harbor-scm-open-checkpoint-buffer ()
  "Open an Agent Harbor checkpoint message buffer."
  (interactive)
  (let* ((state (agent-harbor-scm--row-state))
         (session-id (and state (agent-harbor-scm--get "sessionId" state))))
    (unless session-id
      (user-error "No Agent Harbor session at point"))
    (let ((buffer (generate-new-buffer "*Agent Harbor Commit*")))
      (with-current-buffer buffer
        (agent-harbor-scm-commit-mode)
        (setq-local agent-harbor-scm--checkpoint-session-id session-id)
        (setq-local agent-harbor-scm--checkpoint-refresh-buffer (current-buffer))
        (insert "\n# C-c C-c creates an Agent Harbor checkpoint.\n")
        (goto-char (point-min)))
      (pop-to-buffer buffer))))

(defun agent-harbor-scm--commit-message ()
  "Return checkpoint message from the current buffer."
  (string-trim
   (string-join
    (cl-remove-if
     (lambda (line) (string-prefix-p "#" (string-trim-left line)))
     (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
    "\n")))

(defun agent-harbor-scm--refresh-displays ()
  "Refresh Agent Harbor, VC, and Magit displays."
  (when (fboundp 'vc-refresh-state)
    (ignore-errors (vc-refresh-state)))
  (when (fboundp 'magit-refresh)
    (ignore-errors (magit-refresh)))
  (when-let ((buffer (get-buffer agent-harbor-scm-buffer-name)))
    (with-current-buffer buffer
      (agent-harbor-scm-refresh))))

(defun agent-harbor-scm-commit-submit ()
  "Submit the current checkpoint message to the daemon."
  (interactive)
  (let ((session-id agent-harbor-scm--checkpoint-session-id)
        (message (agent-harbor-scm--commit-message)))
    (unless session-id
      (user-error "No Agent Harbor session for checkpoint"))
    (when (string-empty-p message)
      (user-error "Checkpoint message is empty"))
    (setq agent-harbor-scm--checkpoint-response
          (agent-harbor-rest-checkpoint-session session-id message))
    (setq agent-harbor-scm-last-checkpoint-response agent-harbor-scm--checkpoint-response)
    (let ((response agent-harbor-scm--checkpoint-response))
      (kill-buffer (current-buffer))
      (agent-harbor-scm--refresh-displays)
      (message "Agent Harbor checkpoint created for %s" session-id)
      response)))

(defun agent-harbor-scm-commit-cancel ()
  "Cancel the current checkpoint message."
  (interactive)
  (kill-buffer (current-buffer)))

(defun agent-harbor-scm-magit-insert-section ()
  "Insert the Agent Harbor section in `magit-status'."
  (when-let ((state (ignore-errors (agent-harbor-scm-current-state))))
    (when (require 'magit-section nil t)
      (magit-insert-section (agent-harbor)
        (magit-insert-heading "Agent Harbor")
        (magit-insert-section-body
          (let ((start (point)))
            (agent-harbor-scm--insert-summary state)
            (add-text-properties start (point)
                                 `(agent-harbor-scm-state ,state
                                   read-only t)))
          (if-let ((files (agent-harbor-scm--files state)))
              (dolist (file files)
                (magit-insert-section (agent-harbor-file file)
                  (agent-harbor-scm--insert-file-row file state)))
            (insert "  No daemon-reported changes.\n"))
          (insert "\n"))))))

(defun agent-harbor-scm-magit-checkpoint-dwim ()
  "Open an Agent Harbor checkpoint on rows, otherwise dispatch Magit's commit key."
  (interactive)
  (if (or (get-text-property (point) 'agent-harbor-scm-file)
          (get-text-property (point) 'agent-harbor-scm-state))
      (agent-harbor-scm-open-checkpoint-buffer)
    (let ((binding (or (and (fboundp 'magit-commit)
                            #'magit-commit)
                       (lookup-key (current-global-map) (kbd "c")))))
      (if (commandp binding)
          (call-interactively binding)
        (user-error "No Magit commit command is available")))))

(defun agent-harbor-scm--install-magit-section ()
  "Install the Agent Harbor section into Magit status buffers."
  (when (boundp 'magit-status-sections-hook)
    (let ((fn #'agent-harbor-scm-magit-insert-section)
          (before 'magit-insert-unstaged-changes))
      (add-hook 'magit-status-sections-hook fn)
      (setq magit-status-sections-hook
            (cl-remove fn magit-status-sections-hook :test #'eq))
      (let (installed reordered)
        (dolist (entry magit-status-sections-hook)
          (when (and (not installed) (eq entry before))
            (push fn reordered)
            (setq installed t))
          (push entry reordered))
        (unless installed
          (push fn reordered))
        (setq magit-status-sections-hook (nreverse reordered)))))
  (when (boundp 'magit-status-mode-map)
    (define-key magit-status-mode-map (kbd "c") #'agent-harbor-scm-magit-checkpoint-dwim)))

(with-eval-after-load 'magit
  (agent-harbor-scm--install-magit-section))

(defun agent-harbor-scm-test-state (&optional root)
  "Return current SCM state for test ROOT."
  (agent-harbor-scm-current-state root))

(provide 'agent-harbor-scm)

;;; agent-harbor-scm.el ends here
