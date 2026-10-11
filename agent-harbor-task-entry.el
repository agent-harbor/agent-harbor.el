;;; agent-harbor-task-entry.el --- Agent Harbor task-entry buffer -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Magit-transient-style field navigation for creating Agent Harbor tasks from
;; Emacs.  Submission uses the daemon's real POST /api/v1/tasks endpoint and
;; then hands focus back to the sidebar's REST/SSE model.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agent-harbor-rest)

(declare-function agent-harbor-sidebar-open "agent-harbor-sidebar")
(declare-function agent-harbor-sidebar-refresh "agent-harbor-sidebar")
(declare-function agent-harbor-sidebar-render "agent-harbor-sidebar")
(defvar agent-harbor-sidebar--selected-task-id)

(defgroup agent-harbor-task-entry nil
  "Agent Harbor task-entry buffer."
  :group 'agent-harbor)

(defcustom agent-harbor-task-entry-buffer-name "*Agent Harbor New Task*"
  "Name of the Agent Harbor task-entry buffer."
  :type 'string
  :group 'agent-harbor-task-entry)

(defcustom agent-harbor-task-entry-agent-type "claude"
  "Default agent type for new Agent Harbor tasks."
  :type 'string
  :group 'agent-harbor-task-entry)

(defcustom agent-harbor-task-entry-agent-version "latest"
  "Default agent version for new Agent Harbor tasks."
  :type 'string
  :group 'agent-harbor-task-entry)

(defcustom agent-harbor-task-entry-model "sonnet"
  "Default model for new Agent Harbor tasks."
  :type 'string
  :group 'agent-harbor-task-entry)

(defcustom agent-harbor-task-entry-agent-count 1
  "Default number of agent instances for new tasks."
  :type 'integer
  :group 'agent-harbor-task-entry)

(defcustom agent-harbor-task-entry-working-copy-mode "in_place"
  "Working-copy mode sent to POST /api/v1/tasks."
  :type 'string
  :group 'agent-harbor-task-entry)

(defcustom agent-harbor-task-entry-task-executor-engine "agent-harbor"
  "Task executor engine sent to POST /api/v1/tasks."
  :type 'string
  :group 'agent-harbor-task-entry)

(defcustom agent-harbor-task-entry-acp-command-binary nil
  "Optional ACP stdio command binary for ACP task launches."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'agent-harbor-task-entry)

(defcustom agent-harbor-task-entry-acp-command-args nil
  "Optional ACP stdio command arguments for ACP task launches."
  :type '(repeat string)
  :group 'agent-harbor-task-entry)

(defcustom agent-harbor-task-entry-llm-provider nil
  "Optional LLM provider alist sent to POST /api/v1/tasks.
Expected keys match the daemon contract, for example apiStyle, baseUrl,
apiKey, and model."
  :type '(choice (const :tag "Unset" nil) sexp)
  :group 'agent-harbor-task-entry)

(defvar-local agent-harbor-task-entry--fields nil)
(defvar-local agent-harbor-task-entry--last-error nil)
(defvar-local agent-harbor-task-entry--last-response nil)

(defconst agent-harbor-task-entry--field-order
  '(prompt title workspace branch agent-type model agent-count)
  "Field navigation order for the task-entry buffer.")

(defvar agent-harbor-task-entry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'agent-harbor-task-entry-next-field)
    (define-key map (kbd "<backtab>") #'agent-harbor-task-entry-previous-field)
    (define-key map (kbd "n") #'agent-harbor-task-entry-next-field)
    (define-key map (kbd "p") #'agent-harbor-task-entry-previous-field)
    (define-key map (kbd "e") #'agent-harbor-task-entry-edit-field)
    (define-key map (kbd "C-c C-c") #'agent-harbor-task-entry-submit)
    (define-key map (kbd "C-c C-k") #'agent-harbor-task-entry-cancel)
    map)
  "Keymap for `agent-harbor-task-entry-mode'.")

(define-derived-mode agent-harbor-task-entry-mode special-mode "Agent Harbor Task Entry"
  "Major mode for Agent Harbor task creation."
  (setq-local truncate-lines nil))

(defun agent-harbor-task-entry--git-output (args)
  "Return trimmed git output for ARGS in `default-directory', or nil."
  (when (executable-find "git")
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil args)))
        (when (and (integerp status) (zerop status))
          (let ((output (string-trim (buffer-string))))
            (unless (string-empty-p output) output)))))))

(defun agent-harbor-task-entry--default-workspace ()
  "Return the current git root or `default-directory'."
  (or (agent-harbor-task-entry--git-output '("rev-parse" "--show-toplevel"))
      (expand-file-name default-directory)))

(defun agent-harbor-task-entry--default-branch ()
  "Return the current git branch, if available."
  (agent-harbor-task-entry--git-output '("rev-parse" "--abbrev-ref" "HEAD")))

(defun agent-harbor-task-entry-default-fields ()
  "Return default field alist for a new task-entry buffer."
  `((prompt . "")
    (title . "")
    (workspace . ,(agent-harbor-task-entry--default-workspace))
    (branch . ,(or (agent-harbor-task-entry--default-branch) ""))
    (agent-type . ,agent-harbor-task-entry-agent-type)
    (model . ,agent-harbor-task-entry-model)
    (agent-count . ,agent-harbor-task-entry-agent-count)))

(defun agent-harbor-task-entry--field-label (field)
  "Return display label for FIELD."
  (pcase field
    ('agent-type "Agent")
    ('agent-count "Count")
    (_ (capitalize (replace-regexp-in-string "-" " " (symbol-name field))))))

(defun agent-harbor-task-entry--field-value (field)
  "Return FIELD value from the current task-entry buffer."
  (alist-get field agent-harbor-task-entry--fields))

(defun agent-harbor-task-entry--set-field-value (field value)
  "Set FIELD to VALUE in the current task-entry buffer."
  (setf (alist-get field agent-harbor-task-entry--fields) value))

(defun agent-harbor-task-entry--field-string (field)
  "Return FIELD value as a display string."
  (let ((value (agent-harbor-task-entry--field-value field)))
    (cond
     ((null value) "")
     ((stringp value) value)
     (t (format "%s" value)))))

(defun agent-harbor-task-entry--validate ()
  "Return validation error strings for current fields."
  (delq nil
        (list
         (when (string-empty-p (string-trim (agent-harbor-task-entry--field-string 'prompt)))
           "Prompt is required")
         (when (string-empty-p (string-trim (agent-harbor-task-entry--field-string 'workspace)))
           "Workspace is required")
         (when (string-empty-p (string-trim (agent-harbor-task-entry--field-string 'agent-type)))
           "Agent is required")
         (when (string-empty-p (string-trim (agent-harbor-task-entry--field-string 'model)))
           "Model is required")
         (when (< (string-to-number (agent-harbor-task-entry--field-string 'agent-count)) 1)
           "Agent count must be at least 1"))))

(defun agent-harbor-task-entry--title (prompt)
  "Return task title from explicit title or PROMPT."
  (let ((title (string-trim (agent-harbor-task-entry--field-string 'title))))
    (if (not (string-empty-p title))
        title
      (let ((first-line (car (split-string (string-trim prompt) "\r?\n"))))
        (if (> (length first-line) 80)
            (concat (substring first-line 0 80) "...")
          first-line)))))

(defun agent-harbor-task-entry-build-payload ()
  "Build the daemon POST /api/v1/tasks payload for current fields."
  (let ((errors (agent-harbor-task-entry--validate)))
    (when errors
      (signal 'user-error (list (string-join errors "; ")))))
  (let* ((prompt (string-trim (agent-harbor-task-entry--field-string 'prompt)))
         (title (agent-harbor-task-entry--title prompt))
         (branch (string-trim (agent-harbor-task-entry--field-string 'branch)))
         (agent-type (string-trim (agent-harbor-task-entry--field-string 'agent-type)))
         (model (string-trim (agent-harbor-task-entry--field-string 'model)))
         (agent `((type . ,agent-type)
                  (version . ,agent-harbor-task-entry-agent-version)
                  (count . ,(max 1 (string-to-number
                                    (agent-harbor-task-entry--field-string 'agent-count))))
                  (settings . ((model . ,model)))))
         (repo `((mode . "none"))))
    (when (not (string-empty-p branch))
      (setq repo (append repo `((branch . ,branch)))))
    (when (and (string-equal (downcase agent-type) "acp")
               agent-harbor-task-entry-acp-command-binary)
      (setq agent
            (append agent
                    `((acpStdioLaunchCommand
                       . ((binary . ,agent-harbor-task-entry-acp-command-binary)
                          (args . ,(vconcat agent-harbor-task-entry-acp-command-args))))))))
    (let ((payload
           `((prompt . ,prompt)
             (repo . ,repo)
             (runtime . ((type . "local")))
             (workspace_path . ,(string-trim
                                 (agent-harbor-task-entry--field-string 'workspace)))
             (working_copy_mode . ,agent-harbor-task-entry-working-copy-mode)
             (agents . ,(vector agent))
             (output . ((format . "stream-json")
                        (flavor . "ah")
                        (nonInteractive . t)))
             (taskExecutor . ((engine . ,agent-harbor-task-entry-task-executor-engine)))
             (labels . ((agentHarbor.source . "emacs")
                        (agentHarborTaskTitle . ,title)
                        (agentHarbor.taskTitle . ,title))))))
      (when agent-harbor-task-entry-llm-provider
        (let ((provider (copy-sequence agent-harbor-task-entry-llm-provider)))
          (setf (alist-get 'model provider nil nil #'equal) model)
          (setf (alist-get 'llmProvider payload) provider)))
      payload)))

(defun agent-harbor-task-entry--insert-field (field)
  "Insert FIELD row with navigation properties."
  (let ((start (point)))
    (insert (format "%-10s %s\n"
                    (concat (agent-harbor-task-entry--field-label field) ":")
                    (agent-harbor-task-entry--field-string field)))
    (add-text-properties start (point)
                         `(agent-harbor-task-entry-field ,field
                           read-only t
                           mouse-face highlight))))

(defun agent-harbor-task-entry-render ()
  "Render the current task-entry fields."
  (let ((inhibit-read-only t)
        (point-was (point)))
    (erase-buffer)
    (insert "Agent Harbor New Task\n\n")
    (dolist (field agent-harbor-task-entry--field-order)
      (agent-harbor-task-entry--insert-field field))
    (insert "\n")
    (let ((errors (agent-harbor-task-entry--validate)))
      (when errors
        (insert (format "Validation: %s\n" (string-join errors "; ")))))
    (when agent-harbor-task-entry--last-error
      (insert (format "Error: %s\n" agent-harbor-task-entry--last-error)))
    (when agent-harbor-task-entry--last-response
      (insert (format "Created: %s\n"
                      (or (alist-get 'task_id agent-harbor-task-entry--last-response)
                          (alist-get "task_id" agent-harbor-task-entry--last-response nil nil #'equal)))))
    (goto-char (min (point-max) point-was))))

(defun agent-harbor-task-entry-open ()
  "Open the Agent Harbor task-entry buffer."
  (interactive)
  (let ((buffer (get-buffer-create agent-harbor-task-entry-buffer-name)))
    (with-current-buffer buffer
      (agent-harbor-task-entry-mode)
      (setq agent-harbor-task-entry--fields (agent-harbor-task-entry-default-fields)
            agent-harbor-task-entry--last-error nil
            agent-harbor-task-entry--last-response nil)
      (agent-harbor-task-entry-render))
    (display-buffer buffer)
    buffer))

(defun agent-harbor-task-entry-current-field ()
  "Return the field at point, or the first field."
  (or (get-text-property (point) 'agent-harbor-task-entry-field)
      (car agent-harbor-task-entry--field-order)))

(defun agent-harbor-task-entry--move-field (direction)
  "Move to the next field in DIRECTION."
  (let* ((current (agent-harbor-task-entry-current-field))
         (fields agent-harbor-task-entry--field-order)
         (index (or (cl-position current fields) 0))
         (next (nth (mod (+ index direction) (length fields)) fields)))
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (eq (get-text-property (point) 'agent-harbor-task-entry-field) next)))
      (goto-char (or (next-single-property-change
                      (point) 'agent-harbor-task-entry-field nil (point-max))
                     (point-max))))
    (beginning-of-line)
    next))

(defun agent-harbor-task-entry-next-field ()
  "Move point to the next task-entry field."
  (interactive)
  (agent-harbor-task-entry--move-field 1))

(defun agent-harbor-task-entry-previous-field ()
  "Move point to the previous task-entry field."
  (interactive)
  (agent-harbor-task-entry--move-field -1))

(defun agent-harbor-task-entry-edit-field ()
  "Edit the field at point using the minibuffer."
  (interactive)
  (let* ((field (agent-harbor-task-entry-current-field))
         (old (agent-harbor-task-entry--field-string field))
         (value (read-string (format "%s: " (agent-harbor-task-entry--field-label field)) old)))
    (agent-harbor-task-entry--set-field-value
     field
     (if (eq field 'agent-count) (max 1 (string-to-number value)) value))
    (agent-harbor-task-entry-render)))

(defun agent-harbor-task-entry-set-field (field value)
  "Set FIELD to VALUE in the current task-entry buffer and re-render.
This exercises the same buffer-local model used by interactive editing."
  (unless (memq field agent-harbor-task-entry--field-order)
    (error "Unknown Agent Harbor task-entry field: %s" field))
  (agent-harbor-task-entry--set-field-value field value)
  (agent-harbor-task-entry-render))

(defun agent-harbor-task-entry-submit ()
  "Submit the current task-entry form through POST /api/v1/tasks."
  (interactive)
  (setq agent-harbor-task-entry--last-error nil)
  (condition-case err
      (let* ((response (agent-harbor-rest-create-task
                        (agent-harbor-task-entry-build-payload)))
             (task-id (or (alist-get 'task_id response)
                          (alist-get "task_id" response nil nil #'equal)))
             (session-ids (or (alist-get 'session_ids response)
                              (alist-get "session_ids" response nil nil #'equal))))
        (setq agent-harbor-task-entry--last-response response)
        (unless task-id
          (error "Task creation response did not include task_id"))
        (when (boundp 'agent-harbor-sidebar--selected-task-id)
          (setq agent-harbor-sidebar--selected-task-id task-id))
        (when (fboundp 'agent-harbor-sidebar-open)
          (agent-harbor-sidebar-open)
          (agent-harbor-sidebar-refresh)
          (setq agent-harbor-sidebar--selected-task-id task-id)
          (agent-harbor-sidebar-render))
        (message "Agent Harbor task created: %s" task-id)
        `((taskId . ,task-id)
          (sessionIds . ,(vconcat session-ids))
          (response . ,response)))
    (error
     (setq agent-harbor-task-entry--last-error (error-message-string err))
     (agent-harbor-task-entry-render)
     (signal (car err) (cdr err)))))

(defun agent-harbor-task-entry-cancel ()
  "Close the task-entry buffer."
  (interactive)
  (quit-window t))

(defun agent-harbor-task-entry-test-state ()
  "Return structured state for task-entry page-object tests."
  (let ((buffer (get-buffer agent-harbor-task-entry-buffer-name)))
    (if (not (buffer-live-p buffer))
        '((visible . :false)
          (fields . nil)
          (lastError . nil)
          (lastResponse . nil)
          (submitting . :false))
      (with-current-buffer buffer
        `((visible . ,(if (get-buffer-window buffer t) t :false))
          (fields . ,agent-harbor-task-entry--fields)
          (lastError . ,agent-harbor-task-entry--last-error)
          (lastResponse . ,agent-harbor-task-entry--last-response)
          (submitting . :false))))))

(provide 'agent-harbor-task-entry)

;;; agent-harbor-task-entry.el ends here
