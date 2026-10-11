;;; agent-harbor-mux.el --- Agent Harbor multiplexer dispatch -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Open the focused Agent Harbor task in a host-native editor instance.  Task,
;; session, branch, and workspace metadata are resolved from daemon REST state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agent-harbor-daemon)
(require 'agent-harbor-models)
(require 'agent-harbor-rest)
(require 'agent-harbor-sidebar)
(require 'agent-harbor-workspace)

(defgroup agent-harbor-mux nil
  "Agent Harbor multiplexer dispatch."
  :group 'agent-harbor)

(defcustom agent-harbor-mux-ah-command "ah"
  "Agent Harbor CLI executable used for multiplexer dispatch."
  :type 'string
  :group 'agent-harbor-mux)

(defcustom agent-harbor-mux-emacs-client-command "emacsclient"
  "Emacs client executable used for terminal multiplexer dispatch."
  :type 'string
  :group 'agent-harbor-mux)

(defcustom agent-harbor-mux-emacs-client-args '("-n" ".")
  "Arguments appended to `agent-harbor-mux-emacs-client-command'."
  :type '(repeat string)
  :group 'agent-harbor-mux)

(defcustom agent-harbor-mux-prefer-gui-frame t
  "When non-nil, GUI Emacs opens the selected task in a new frame."
  :type 'boolean
  :group 'agent-harbor-mux)

(define-error 'agent-harbor-mux-error "Agent Harbor multiplexer error")

(defun agent-harbor-mux--get (key alist)
  "Return KEY from ALIST, accepting string and symbol keys."
  (agent-harbor-models--alist-get key alist))

(defun agent-harbor-mux-detect-host ()
  "Return the current editor host for multiplexer dispatch."
  (cond
   ((display-graphic-p) "emacs-frame")
   ((getenv "TMUX") "tmux")
   ((or (getenv "ZELLIJ") (getenv "ZELLIJ_SESSION_NAME")) "zellij")
   ((getenv "WEZTERM_PANE") "wezterm")
   ((getenv "STY") "screen")
   ((getenv "KITTY_LISTEN_ON") "kitty")
   ((getenv "ITERM_SESSION_ID") "iterm2")
   (t "bare")))

(defun agent-harbor-mux--task-id (task)
  "Return TASK id."
  (agent-harbor-mux--get "id" task))

(defun agent-harbor-mux--task-workspace (task)
  "Return TASK workspace path."
  (or (agent-harbor-mux--get "workspace_path" task)
      (agent-harbor-mux--get "workspacePath" task)))

(defun agent-harbor-mux--selected-task ()
  "Return the selected sidebar task, refreshing from the daemon when needed."
  (agent-harbor-daemon-refresh)
  (unless (eq agent-harbor-daemon-state 'connected)
    (signal 'agent-harbor-mux-error '("Agent Harbor daemon is not connected")))
  (unless (bound-and-true-p agent-harbor-sidebar--tasks)
    (agent-harbor-sidebar-refresh))
  (or (agent-harbor-sidebar--selected-task)
      (car agent-harbor-sidebar--tasks)
      (signal 'agent-harbor-mux-error '("No Agent Harbor task is selected"))))

(defun agent-harbor-mux--session-ids (task-id)
  "Return daemon session ids for TASK-ID."
  (delq nil
        (mapcar (lambda (session)
                  (agent-harbor-mux--get "id" session))
                (agent-harbor-rest-task-sessions task-id))))

(defun agent-harbor-mux--resolve-root (task &optional branch-id)
  "Resolve TASK and optional BRANCH-ID to a local workspace root."
  (let* ((task-id (agent-harbor-mux--task-id task))
         (session-ids (agent-harbor-mux--session-ids task-id))
         (branch (and session-ids
                      (ignore-errors
                        (agent-harbor-workspace-resolve-branch
                         task-id session-ids branch-id))))
         (root (or (and branch (agent-harbor-workspace-branch-root branch))
                   (agent-harbor-mux--task-workspace task))))
    (unless (and root (file-directory-p root))
      (signal 'agent-harbor-mux-error
              (list (format "Selected task has no local branch/workspace root: %s" task-id))))
    (list :task-id task-id
          :branch-id (and branch (agent-harbor-mux--get "id" branch))
          :root (file-name-as-directory (expand-file-name root)))))

(defun agent-harbor-mux--open-gui-frame (root)
  "Open ROOT in a new GUI frame."
  (let ((default-directory root))
    (select-frame-set-input-focus
     (make-frame `((name . ,(format "Agent Harbor: %s"
                                    (file-name-nondirectory
                                     (directory-file-name root)))))))
    (dired root)))

(defun agent-harbor-mux--call-cli (host root title command args)
  "Invoke the Rust mux helper for HOST ROOT TITLE COMMAND ARGS."
  (let* ((host-arg (if (equal host "emacs-frame") "auto" host))
         (argv (append
                (list "editor-mux" "open-new"
                      "--host" host-arg
                      "--cwd" root
                      "--title" title)
                (list command)
                args))
         (buffer (generate-new-buffer " *agent-harbor-mux*"))
         (status (unwind-protect
                     (apply #'call-process agent-harbor-mux-ah-command nil buffer nil argv)
                   (unless (buffer-live-p buffer)
                     (setq buffer nil)))))
    (unwind-protect
        (unless (and (integerp status) (zerop status))
          (signal 'agent-harbor-mux-error
                  (list (format "ah editor-mux failed: %s"
                                (and buffer
                                     (with-current-buffer buffer
                                       (string-trim (buffer-string))))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;;###autoload
(defun agent-harbor-open-in-new-instance (&optional task-id)
  "Open TASK-ID, or the focused task, in a new editor instance."
  (interactive)
  (agent-harbor-sidebar-refresh)
  (when task-id
    (setq agent-harbor-sidebar--selected-task-id task-id))
  (let* ((task (agent-harbor-mux--selected-task))
         (selected-branch-id (agent-harbor-sidebar--branch-id-for-task
                              (agent-harbor-mux--task-id task)))
         (target (agent-harbor-mux--resolve-root task selected-branch-id))
         (root (plist-get target :root))
         (host (agent-harbor-mux-detect-host))
         (title (format "AH %s" (plist-get target :task-id))))
    (if (and agent-harbor-mux-prefer-gui-frame
             (display-graphic-p)
             (equal host "emacs-frame"))
        (agent-harbor-mux--open-gui-frame root)
      (agent-harbor-mux--call-cli
       host root title
       agent-harbor-mux-emacs-client-command
       agent-harbor-mux-emacs-client-args))
    `((host . ,host)
      (taskId . ,(plist-get target :task-id))
      (branchId . ,(or (plist-get target :branch-id) :false))
      (root . ,root))))

(provide 'agent-harbor-mux)

;;; agent-harbor-mux.el ends here
