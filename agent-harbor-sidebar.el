;;; agent-harbor-sidebar.el --- Agent Harbor side window -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Dedicated read-only side window for Agent Harbor.  The sidebar state is a
;; projection of the daemon REST/SSE surface so tests can inspect the rendered
;; model without taking a separate fake path.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)
(require 'agent-harbor-daemon)
(require 'agent-harbor-rest)
(require 'agent-harbor-reply)
(require 'agent-harbor-sse)
(require 'agent-harbor-task-entry)
(require 'agent-harbor-timeline)
(require 'agent-harbor-workspace)

(declare-function agent-harbor--install-header-line "agent-harbor")
(declare-function agent-harbor-header-line "agent-harbor")
(declare-function agent-harbor-open-documentation "agent-harbor")
(declare-function agent-harbor-open-in-new-instance "agent-harbor-mux")
(defvar agent-harbor-buffer-name)

(defgroup agent-harbor-sidebar nil
  "Agent Harbor side-window rendering."
  :group 'agent-harbor)

(defcustom agent-harbor-sidebar-width 82
  "Width in columns for the Agent Harbor side window."
  :type 'integer
  :group 'agent-harbor-sidebar)

(defcustom agent-harbor-sidebar-refresh-interval 0.5
  "Polling interval in seconds for task/session/sidebar refresh."
  :type 'number
  :group 'agent-harbor-sidebar)

(defcustom agent-harbor-cross-task-files 'preview
  "How Agent Harbor opens files from non-focused task timeline cards."
  :type '(choice (const :tag "Read-only preview" preview)
                 (const :tag "Switch to task branch" switch)
                 (const :tag "Open in new frame" new-frame))
  :group 'agent-harbor-sidebar)

(defcustom agent-harbor-share-task-default-ttl "24h"
  "Default TTL for `agent-harbor-share-task' URLs."
  :type 'string
  :group 'agent-harbor-sidebar)

(defvar agent-harbor-sidebar--tasks nil)
(defvar agent-harbor-sidebar--task-sessions (make-hash-table :test #'equal))
(defvar agent-harbor-sidebar--task-branches (make-hash-table :test #'equal))
(defvar agent-harbor-sidebar--all-branches nil
  "Cached full daemon branch list (every branch, not just the focused task's).
Used to normalize a cross-task `agentharborfs://branches/<name>/...'
preview URI to its canonical `agentharborfs://<id>/...' form without a
render-time REST call.")
(defvar agent-harbor-sidebar--full-sessions (make-hash-table :test #'equal))
(defvar agent-harbor-sidebar--session-events (make-hash-table :test #'equal))
(defvar agent-harbor-sidebar--session-history-counts (make-hash-table :test #'equal))
(defvar agent-harbor-sidebar--task-diff-files (make-hash-table :test #'equal))
(defvar agent-harbor-sidebar--task-feedback (make-hash-table :test #'equal))
(defvar agent-harbor-sidebar--sse-clients (make-hash-table :test #'equal))
(defvar agent-harbor-sidebar--sse-event-count 0)
(defvar agent-harbor-sidebar--selected-task-id nil)
(defvar agent-harbor-sidebar--selected-stream-session-id nil)
(defvar agent-harbor-sidebar--accordion-collapsed-p nil)
(defvar agent-harbor-sidebar--last-error nil)
(defvar agent-harbor-sidebar--refresh-timer nil)
(defvar agent-harbor-sidebar-last-share nil)

(defvar agent-harbor-sidebar--inflight-count 0
  "Number of asynchronous daemon requests currently outstanding.")
(defvar agent-harbor-sidebar--refresh-active nil
  "Non-nil while an async refresh sweep is in progress.
Acts as the re-entrancy guard so overlapping timer-driven refreshes do
not duplicate in-flight work or double-count requests.")
(defvar agent-harbor-sidebar--last-refresh-time nil
  "`float-time' at which the last refresh sweep fully completed.")
(defvar agent-harbor-sidebar--tasks-loaded nil
  "Non-nil once a task list has been confirmed from the daemon at least once.")
(defvar agent-harbor-sidebar--diff-loaded (make-hash-table :test #'equal)
  "Per-task-id flag: non-nil once the modified-file set has been confirmed.")
(defvar agent-harbor-sidebar--history-loaded (make-hash-table :test #'equal)
  "Per-session-id flag: non-nil once focused-task history has arrived.")
(defvar agent-harbor-sidebar--spinner-frames
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Braille spinner frames cycled while requests are in flight.")
(defvar agent-harbor-sidebar--spinner-index 0
  "Current index into `agent-harbor-sidebar--spinner-frames'.")
(defvar agent-harbor-sidebar--live-session-ids nil
  "Session ids seen during the current async refresh sweep.
Used to stop stale SSE clients once the sweep settles.")
(defvar-local agent-harbor-preview nil)
(defvar-local agent-harbor-preview-branch-id nil)
(defvar-local agent-harbor-preview-path nil)

(defvar agent-harbor-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map agent-harbor-timeline-mode-map)
    (define-key map (kbd "RET") #'agent-harbor-sidebar-select-task)
    (define-key map [mouse-1] #'agent-harbor-sidebar-mouse-1)
    (define-key map (kbd "TAB") #'agent-harbor-sidebar-tab)
    (define-key map (kbd "+") #'agent-harbor-sidebar-new-task)
    (define-key map (kbd "N") #'agent-harbor-sidebar-new-task)
    (define-key map (kbd "O") #'agent-harbor-open-in-new-instance)
    (define-key map (kbd "s") #'agent-harbor-share-task)
    (define-key map (kbd "C-c C-r") #'agent-harbor-reply-read-and-submit)
    (define-key map (kbd "C-c C-m") #'agent-harbor-reply-open-popup)
    (define-key map (kbd "C-c C-a") #'agent-harbor-reply-quick-approve)
    (define-key map (kbd "C-c C-x") #'agent-harbor-reply-quick-reject)
    (define-key map (kbd "n") #'agent-harbor-timeline-next-card)
    (define-key map (kbd "p") #'agent-harbor-timeline-previous-card)
    (define-key map (kbd "f") #'agent-harbor-timeline-toggle-auto-follow)
    map)
  "Keymap for `agent-harbor-sidebar-mode'.")

(define-derived-mode agent-harbor-sidebar-mode agent-harbor-timeline-mode "Agent Harbor"
  "Major mode for the Agent Harbor sidebar."
  (setq-local truncate-lines nil))

(defun agent-harbor-sidebar--get (key alist)
  "Return KEY from ALIST, accepting string and symbol keys."
  (agent-harbor-models--alist-get key alist))

(defun agent-harbor-sidebar--alist (value)
  "Return VALUE when it is an alist-like object."
  (and (listp value) value))

(defun agent-harbor-sidebar--task-id (task)
  "Return TASK id."
  (agent-harbor-sidebar--get "id" task))

(defun agent-harbor-sidebar--session-id (session)
  "Return SESSION id."
  (agent-harbor-sidebar--get "id" session))

(defun agent-harbor-sidebar--failure-status-p (status)
  "Return non-nil when STATUS is a failure-like terminal state."
  (member status '("failed" "cancelled" "stopped")))

(defun agent-harbor-sidebar--terminal-status-p (status)
  "Return non-nil when STATUS is terminal."
  (or (equal status "completed")
      (agent-harbor-sidebar--failure-status-p status)))

(defun agent-harbor-sidebar--task-status (task)
  "Return TASK status string."
  (let* ((task-id (agent-harbor-sidebar--task-id task))
         (sessions (and task-id (agent-harbor-sidebar--task-sessions-for task-id)))
         (session-statuses (mapcar #'agent-harbor-sidebar--session-status sessions))
         (event-status (and task-id
                            (agent-harbor-sidebar--latest-event-status-for-task task-id)))
         (task-status (or (agent-harbor-sidebar--get "status" task) "unknown")))
    (cond
     ((agent-harbor-sidebar--failure-status-p event-status) event-status)
     ((cl-some #'agent-harbor-sidebar--failure-status-p
               session-statuses)
      (car (cl-remove-if-not
            #'agent-harbor-sidebar--failure-status-p
            session-statuses)))
     ((agent-harbor-sidebar--terminal-status-p task-status) task-status)
     ;; A session's own status event may report progress, but it may not finish
     ;; a task the daemon still calls unfinished. `--latest-event-status-for-task'
     ;; folds the status events of EVERY session bound to the task, so in the
     ;; window between two loop-driver phases the newest one is the phase that
     ;; just ended — and returning it here reported the whole task completed
     ;; while its supervisor was still blocked on an unanswered feedback
     ;; request. Failure statuses are already handled above and still win.
     ((and event-status (not (equal event-status "completed"))) event-status)
     ((cl-some (lambda (status) (member status '("running" "provisioning")))
               session-statuses)
      "running")
     ;; A task is NOT promoted to "completed" just because every session this
     ;; sidebar currently knows about is completed. The daemon owns a task's
     ;; lifecycle, and for a loop-driver task that aggregate is wrong in an
     ;; ordinary, recurring window: the driver runs one phase session at a time,
     ;; so between two phases every session it has started is completed and the
     ;; next one does not exist yet. Reading that as completion reported a
     ;; finished task while its supervisor was still blocked on an unanswered
     ;; feedback request, overriding a daemon that correctly said "running".
     ;; Trust the daemon's status; it already derives completion from sessions
     ;; where that is sound. See
     ;; `issues/2026-10-07-a-live-loop-driver-task-is-reported-completed-between-phases.md'.
     ((and (equal task-status "pending")
           (member "completed" session-statuses))
      "completed")
     (t task-status))))

(defun agent-harbor-sidebar--session-status (session)
  "Return SESSION status string."
  (or (agent-harbor-sidebar--get "status" session) "unknown"))

(defun agent-harbor-sidebar--event-status (event)
  "Return status payload from EVENT when it is a status event.

Intentionally does NOT unwrap sub_agent envelopes. The task's overall
status is the OUTER task's status, owned by the manager session that
hosts the task in the daemon. Child-session status transitions (a
sub-agent flipping from running to completed and back) arrive as
sub_agent envelopes on the parent's history; folding those into
`agent-harbor-sidebar--task-status' would make the focused-task status
oscillate whenever a late child status event lands, even after the
manager's terminal status has settled. The card-text and modified-files
paths still unwrap envelopes so the timeline surface mirrors all
activity."
  (when (equal (agent-harbor-timeline--text-field "type" event) "status")
    (agent-harbor-timeline--text-field "status" event)))

(defun agent-harbor-sidebar--latest-event-status-for-task (task-id)
  "Return the latest timeline status for TASK-ID."
  (let ((latest nil)
        (latest-timestamp nil))
    (dolist (session (agent-harbor-sidebar--task-sessions-for task-id))
      (when-let ((session-id (agent-harbor-sidebar--session-id session)))
        (dolist (event (gethash session-id agent-harbor-sidebar--session-events))
          (when-let ((status (agent-harbor-sidebar--event-status event)))
            (let ((timestamp (agent-harbor-timeline-event-timestamp event)))
              (when (or (null latest-timestamp)
                        (>= timestamp latest-timestamp))
                (setq latest status
                      latest-timestamp timestamp)))))))
    latest))

(defun agent-harbor-sidebar--labels-from-full-session (session-id)
  "Return labels alist for SESSION-ID from the full session list."
  (let* ((session (gethash session-id agent-harbor-sidebar--full-sessions))
         (task (agent-harbor-sidebar--alist (agent-harbor-sidebar--get "task" session))))
    (agent-harbor-sidebar--alist (agent-harbor-sidebar--get "labels" task))))

(defun agent-harbor-sidebar--label (labels key)
  "Return KEY from LABELS."
  (or (agent-harbor-sidebar--get key labels)
      (agent-harbor-sidebar--get (replace-regexp-in-string "\\." "" key) labels)))

(defun agent-harbor-sidebar--task-sessions-for (task-id)
  "Return session summaries for TASK-ID."
  (or (gethash task-id agent-harbor-sidebar--task-sessions) nil))

(defun agent-harbor-sidebar--session-ids-for-task (task-id)
  "Return session ids for TASK-ID."
  (delq nil
        (mapcar #'agent-harbor-sidebar--session-id
                (agent-harbor-sidebar--task-sessions-for task-id))))

(defun agent-harbor-sidebar--sanitize-single-line (text)
  "Return only the first non-empty line of TEXT.

Daemon-supplied task titles can carry multi-line prompts (loop-driver and
manager-harness sub-tasks embed `# Implementation Task\\n\\n## …' as the
title fallback).  Accordion rows are single-line by design, so we collapse
to the first line to keep the buffer write well-formed."
  (when text
    (or (and (stringp text)
             (string-match "[^\r\n]+" text)
             (match-string 0 text))
        text)))

(defun agent-harbor-sidebar--task-title (task)
  "Return display title for TASK."
  (let* ((task-id (agent-harbor-sidebar--task-id task))
         (sessions (agent-harbor-sidebar--task-sessions-for task-id))
         (first-session-id (and sessions
                                (agent-harbor-sidebar--session-id (car sessions))))
         ;; Prefer labels from the full session list, but fall back to the
         ;; per-task session summary's own labels.  The full session list is
         ;; bounded by the daemon, so a task that has accumulated many rows may
         ;; have its title-bearing session paged out of it; the per-task
         ;; `/sessions' summary is unbounded and carries the same labels.
         (labels (or (and first-session-id
                          (agent-harbor-sidebar--labels-from-full-session
                           first-session-id))
                     (and sessions
                          (agent-harbor-sidebar--alist
                           (agent-harbor-sidebar--get "labels" (car sessions)))))))
    (agent-harbor-sidebar--sanitize-single-line
     (or (and labels (agent-harbor-sidebar--label labels "agentHarbor.taskTitle"))
         (and labels (agent-harbor-sidebar--label labels "agentHarborTaskTitle"))
         (agent-harbor-sidebar--get "prompt" task)
         task-id))))

(defun agent-harbor-sidebar--branch-id (session)
  "Return branch id for SESSION summary or full session."
  (or (agent-harbor-sidebar--get "agentharborfs_branch_id" session)
      (agent-harbor-sidebar--get "agentharborfsBranchId" session)))

(defun agent-harbor-sidebar--branch-metadata-id (branch)
  "Return daemon branch metadata id for BRANCH."
  (agent-harbor-sidebar--get "id" branch))

(defun agent-harbor-sidebar--workspace-branch-id-for-task (task-id)
  "Return current workspace branch id when it belongs to TASK-ID."
  (let ((state (agent-harbor-workspace-current-state)))
    (when (equal (agent-harbor-sidebar--get "taskId" state) task-id)
      (agent-harbor-sidebar--get "branchId" state))))

(defun agent-harbor-sidebar--branch-ids-for-task (task-id)
  "Return branch ids associated with TASK-ID."
  (cl-remove-duplicates
   (delq nil
         (append
          (mapcar #'agent-harbor-sidebar--branch-id
                  (agent-harbor-sidebar--task-sessions-for task-id))
          (mapcar #'agent-harbor-sidebar--branch-metadata-id
                  (gethash task-id agent-harbor-sidebar--task-branches))
          (list (agent-harbor-sidebar--workspace-branch-id-for-task task-id))))
   :test #'equal))

(defun agent-harbor-sidebar--branch-name (branch)
  "Return human-readable daemon BRANCH name."
  (agent-harbor-sidebar--get "name" branch))

(defun agent-harbor-sidebar--branch-workspace-path (branch)
  "Return daemon workspace path for BRANCH."
  (or (agent-harbor-sidebar--get "workspacePath" branch)
      (agent-harbor-sidebar--get "workspace_path" branch)))

(defun agent-harbor-sidebar--selected-task ()
  "Return the selected task alist."
  (or (cl-find agent-harbor-sidebar--selected-task-id
               agent-harbor-sidebar--tasks
               :key #'agent-harbor-sidebar--task-id
               :test #'equal)
      (car agent-harbor-sidebar--tasks)))

(defun agent-harbor-sidebar--selected-task-id ()
  "Return the selected task id."
  (when-let ((task (agent-harbor-sidebar--selected-task)))
    (agent-harbor-sidebar--task-id task)))

(defun agent-harbor-sidebar--selected-sessions ()
  "Return sessions for the selected task."
  (when-let ((task-id (agent-harbor-sidebar--selected-task-id)))
    (agent-harbor-sidebar--task-sessions-for task-id)))

(defun agent-harbor-sidebar--session-role (session)
  "Return SESSION role string when present."
  (agent-harbor-sidebar--get "role" session))

(defun agent-harbor-sidebar--startup-session-p (session)
  "Return non-nil when SESSION is a non-executable startup anchor."
  (equal (agent-harbor-sidebar--session-role session) "startup"))

(defun agent-harbor-sidebar--normalized-session-role (session)
  "Return SESSION role normalized for priority comparisons."
  (let ((role (agent-harbor-sidebar--session-role session)))
    (if (stringp role)
        (downcase (replace-regexp-in-string "[-. ]" "_" role))
      "")))

(defun agent-harbor-sidebar--session-role-priority (session)
  "Return automatic stream-selection priority for SESSION role."
  (let ((role (agent-harbor-sidebar--normalized-session-role session)))
    (cond
     ((member role '("implementation" "implementer")) 1)
     ((member role '("manager" "manager_agent")) 2)
     ((equal role "coordinator") 3)
     ((or (equal role "reviewer") (string-prefix-p "reviewer_" role)) 4)
     ((equal role "supervisor") 5)
     (t 100))))

(defun agent-harbor-sidebar--preferred-role-session (sessions &optional predicate)
  "Return best SESSION by role among SESSIONS satisfying PREDICATE."
  (let ((best nil)
        (best-priority nil))
    (dolist (session sessions best)
      (when (or (null predicate) (funcall predicate session))
        (let ((priority (agent-harbor-sidebar--session-role-priority session)))
          (when (or (null best) (< priority best-priority))
            (setq best session)
            (setq best-priority priority)))))))

(defun agent-harbor-sidebar--session-has-timeline-p (session)
  "Return non-nil when SESSION has cached timeline history or events."
  (when-let ((session-id (agent-harbor-sidebar--session-id session)))
    (or (gethash session-id agent-harbor-sidebar--session-events)
        (> (or (gethash session-id agent-harbor-sidebar--session-history-counts) 0) 0))))

(defun agent-harbor-sidebar--selected-session-id ()
  "Return selected session id for the selected task."
  (when-let ((sessions (agent-harbor-sidebar--selected-sessions)))
    (let* ((session-ids (mapcar #'agent-harbor-sidebar--session-id sessions))
           (non-startup-sessions
            (cl-remove-if #'agent-harbor-sidebar--startup-session-p sessions)))
      (or (and (member agent-harbor-sidebar--selected-stream-session-id session-ids)
               agent-harbor-sidebar--selected-stream-session-id)
          (agent-harbor-sidebar--session-id
           (or (cl-find-if (lambda (session)
                             (and (member (agent-harbor-sidebar--session-status session)
                                          '("running" "queued"))
                                  (agent-harbor-sidebar--session-has-timeline-p session)))
                           non-startup-sessions)
               (cl-find-if (lambda (session)
                             (member (agent-harbor-sidebar--session-status session)
                                     '("running" "queued")))
                           non-startup-sessions)
               (agent-harbor-sidebar--preferred-role-session
                non-startup-sessions
                #'agent-harbor-sidebar--session-has-timeline-p)
               (cl-find-if #'agent-harbor-sidebar--session-has-timeline-p
                           non-startup-sessions)
               (agent-harbor-sidebar--preferred-role-session non-startup-sessions)
               (car (last non-startup-sessions))
               (cl-find-if (lambda (session)
                             (member (agent-harbor-sidebar--session-status session)
                                     '("running" "queued")))
                           sessions)
               (car (last sessions))
               (car sessions)))))))

(defun agent-harbor-sidebar--selected-branch-id ()
  "Return branch metadata for the selected task when available."
  (or (cl-loop for session in (agent-harbor-sidebar--selected-sessions)
               for branch = (agent-harbor-sidebar--branch-id session)
               when branch return branch)
      (when-let* ((session-id (agent-harbor-sidebar--selected-session-id))
                  (session (gethash session-id agent-harbor-sidebar--full-sessions)))
        (agent-harbor-sidebar--branch-id session))
      (when-let ((task-id (agent-harbor-sidebar--selected-task-id)))
        (car (last (agent-harbor-sidebar--branch-ids-for-task task-id))))))

(defun agent-harbor-sidebar--branch-id-for-task (task-id)
  "Return best branch id for TASK-ID from daemon session metadata."
  (or (cl-loop for session in (agent-harbor-sidebar--task-sessions-for task-id)
               for branch = (agent-harbor-sidebar--branch-id session)
               when branch return branch)
      (when-let ((branches (gethash task-id agent-harbor-sidebar--task-branches)))
        (agent-harbor-sidebar--branch-metadata-id (car (last branches))))
      (cl-loop for session-id in (agent-harbor-sidebar--session-ids-for-task task-id)
               for session = (gethash session-id agent-harbor-sidebar--full-sessions)
               for branch = (and session (agent-harbor-sidebar--branch-id session))
               when branch return branch)
      ;; Non-focused tasks skip the per-task `/branches' fetch (their
      ;; `--task-branches' stays empty), so fall back to the shared full
      ;; branch list, matching a branch to one of the task's session ids.
      ;; This lets a cross-task preview card resolve the foreign task's
      ;; branch id without paying for that task's branch fan-out.
      (let ((session-ids (agent-harbor-sidebar--session-ids-for-task task-id)))
        (cl-loop for branch in agent-harbor-sidebar--all-branches
                 for sid = (or (agent-harbor-models--alist-get "sessionId" branch)
                               (agent-harbor-models--alist-get "session_id" branch))
                 when (and sid (member sid session-ids))
                 return (agent-harbor-sidebar--branch-metadata-id branch)))
      (agent-harbor-sidebar--workspace-branch-id-for-task task-id)))

(defun agent-harbor-sidebar--task-id-for-session (session-id)
  "Return the task id that owns SESSION-ID."
  (cl-loop for task in agent-harbor-sidebar--tasks
           for task-id = (agent-harbor-sidebar--task-id task)
           when (member session-id (agent-harbor-sidebar--session-ids-for-task task-id))
           return task-id))

(defun agent-harbor-sidebar--branch-for-id-or-name (branch-ref)
  "Return daemon branch metadata matching BRANCH-REF id or name."
  (cl-find-if
   (lambda (branch)
     (or (equal (agent-harbor-sidebar--get "id" branch) branch-ref)
         (equal (agent-harbor-sidebar--branch-name branch) branch-ref)))
   (agent-harbor-rest-list-branches-raw)))

(defun agent-harbor-sidebar--agentharborfs-uri-from-text (text)
  "Return an agentharborfs URI embedded in TEXT, if any."
  (when (and text (string-match "\\bagentharborfs://[^[:space:])>'\"]+" text))
    (match-string 0 text)))

(defun agent-harbor-sidebar--agentharborfs-preview (uri)
  "Parse URI into a preview alist."
  (when (and uri (string-prefix-p "agentharborfs://" uri))
    (let* ((without-scheme (substring uri (length "agentharborfs://")))
           (parts (split-string without-scheme "/" t))
           (branch-ref (car parts))
           (path-parts (cdr parts)))
      (when (equal branch-ref "branches")
        (setq branch-ref (car path-parts)
              path-parts (cdr path-parts)))
      (when (and branch-ref path-parts)
        `((uri . ,uri)
          (branchRef . ,(url-unhex-string branch-ref))
          (path . ,(mapconcat #'url-unhex-string path-parts "/")))))))

(defun agent-harbor-sidebar--cached-branch-for-id-or-name (branch-ref)
  "Return the cached branch metadata matching BRANCH-REF id or name.
Resolves against `agent-harbor-sidebar--all-branches' (populated by refresh) so
this can run during render without a REST round-trip."
  (cl-find-if
   (lambda (branch)
     (or (equal (agent-harbor-sidebar--get "id" branch) branch-ref)
         (equal (agent-harbor-sidebar--branch-name branch) branch-ref)))
   agent-harbor-sidebar--all-branches))

(defun agent-harbor-sidebar--normalize-agentharborfs-uri (uri)
  "Rewrite an agentharborfs branch-name URI to its canonical branch-id form.
A timeline card's text may reference a branch by NAME
\(agentharborfs://branches/<name>/...), e.g. a cross-task preview of
another task's
branch.  Previews are keyed by branch id, so resolve <name> to its id via the
cached branch list and rebuild the URI.  Returns URI unchanged when the branch
is already an id or cannot be resolved yet."
  (let ((parsed (agent-harbor-sidebar--agentharborfs-preview uri)))
    (if (not parsed)
        uri
      (let* ((branch-ref (alist-get 'branchRef parsed))
             (path (alist-get 'path parsed))
             (branch (and branch-ref
                          (agent-harbor-sidebar--cached-branch-for-id-or-name branch-ref)))
             (branch-id (and branch (agent-harbor-sidebar--get "id" branch))))
        (if (and branch-id path (not (equal branch-id branch-ref)))
            (concat "agentharborfs://"
                    (url-hexify-string branch-id)
                    "/"
                    (mapconcat #'url-hexify-string (split-string path "/" t) "/"))
          uri)))))

(defun agent-harbor-sidebar--preview-uri-for-card (card branch-id)
  "Return an agentharborfs preview URI for CARD in BRANCH-ID."
  (or (when-let ((uri (agent-harbor-sidebar--agentharborfs-uri-from-text
                       (or (agent-harbor-sidebar--get "text" card) ""))))
        (agent-harbor-sidebar--normalize-agentharborfs-uri uri))
      (when (and (equal (agent-harbor-sidebar--get "type" card) "edited")
                 branch-id
                 (agent-harbor-sidebar--get "filePath" card))
        (concat "agentharborfs://"
                (url-hexify-string branch-id)
                "/"
                (url-hexify-string (agent-harbor-sidebar--get "filePath" card))))))

(defun agent-harbor-sidebar--card-with-preview (card session task-id)
  "Attach preview metadata to CARD for SESSION and TASK-ID."
  (let ((branch-id (or (agent-harbor-sidebar--branch-id session)
                       (agent-harbor-sidebar--branch-id-for-task task-id))))
    (if-let ((uri (agent-harbor-sidebar--preview-uri-for-card card branch-id)))
      (append `((previewUri . ,uri)
                (sourceTaskId . ,task-id)
                (branchId . ,branch-id))
              card)
      card)))

(defun agent-harbor-sidebar--feedback-created-at (feedback)
  "Return FEEDBACK created timestamp string."
  (or (agent-harbor-sidebar--get "createdAt" feedback)
      (agent-harbor-sidebar--get "created_at" feedback)
      ""))

(defun agent-harbor-sidebar--pending-feedback-for-task (task-id)
  "Return the oldest pending feedback request for TASK-ID."
  (car
   (sort
    (cl-remove-if-not
     (lambda (feedback)
       (equal (agent-harbor-sidebar--get "status" feedback) "pending"))
     (copy-sequence (or (gethash task-id agent-harbor-sidebar--task-feedback) nil)))
    (lambda (left right)
      (string< (agent-harbor-sidebar--feedback-created-at left)
               (agent-harbor-sidebar--feedback-created-at right))))))

(defun agent-harbor-sidebar--selected-pending-feedback ()
  "Return pending feedback for the selected task."
  (when-let ((task-id (agent-harbor-sidebar--selected-task-id)))
    (agent-harbor-sidebar--pending-feedback-for-task task-id)))

(defun agent-harbor-sidebar--pending-feedback-task-ids ()
  "Return task ids that have pending feedback."
  (let (task-ids)
    (dolist (task agent-harbor-sidebar--tasks)
      (let ((task-id (agent-harbor-sidebar--task-id task)))
        (when (and task-id (agent-harbor-sidebar--pending-feedback-for-task task-id))
          (push task-id task-ids))))
    (nreverse task-ids)))

(defun agent-harbor-sidebar--event-key (session-id event)
  "Return stable event key for SESSION-ID EVENT.

For sub_agent envelopes we fingerprint on the inner event so the parent
session's history merge dedupes by the actual fan-out payload — otherwise
the same inner activity would deduplicate by the wrapper's identical
\"sub_agent\" type signature."
  (let* ((effective (agent-harbor-timeline-effective-event event))
         (event-type (agent-harbor-timeline--event-type effective))
         (timestamp (number-to-string (agent-harbor-timeline-event-timestamp event)))
         (field (lambda (key)
                  (or (agent-harbor-timeline--text-field key effective) "")))
         (fields
          (pcase event-type
            ("status" (list (funcall field "status")))
            ("workspace" (list (funcall field "status")
                               (funcall field "mount_path")
                               (funcall field "working_copy_mode")
                               (funcall field "provider")
                               (funcall field "error")))
            ("log" (list (funcall field "level")
                         (funcall field "message")
                         (funcall field "tool_execution_id")))
            ("thought" (list (funcall field "thought")
                             (funcall field "reasoning")))
            ("tool_use" (list (funcall field "tool_name")
                              (funcall field "tool_args")
                              (funcall field "tool_execution_id")
                              (funcall field "status")))
            ("tool_result" (list (funcall field "tool_name")
                                 (funcall field "tool_output")
                                 (funcall field "tool_execution_id")
                                 (funcall field "status")
                                 (funcall field "exit_code")))
            ("file_edit" (list (funcall field "file_path")
                               (funcall field "lines_added")
                               (funcall field "lines_removed")
                               (funcall field "description")))
            ("file_read" (list (funcall field "file_path")
                               (funcall field "start_line")
                               (funcall field "end_line")))
            ("diff" (list (funcall field "path")
                          (funcall field "tool_execution_id")
                          (funcall field "oldText")
                          (funcall field "newText")
                          (funcall field "fullDiffUrl")))
            ("user_input" (list (funcall field "content")
                                (funcall field "input_type")
                                (funcall field "user_id")
                                (funcall field "user_name")))
            (_ (list (agent-harbor-timeline-event-text effective))))))
    (mapconcat #'identity
               (list (or session-id "")
                     timestamp
                     event-type
                     (mapconcat #'identity fields "\x1f"))
               "\x1e")))

(defun agent-harbor-sidebar--merge-events (session-id events)
  "Merge chronological EVENTS into SESSION-ID's event list."
  (let ((combined (copy-sequence (or (gethash session-id agent-harbor-sidebar--session-events)
                                     nil)))
        (seen (make-hash-table :test #'equal))
        (index 0))
    (dolist (event combined)
      (setq index (1+ index))
      (puthash (agent-harbor-sidebar--event-key session-id event) t seen))
    (dolist (event events)
      (let ((key (agent-harbor-sidebar--event-key session-id event)))
        (unless (gethash key seen)
          (setq index (1+ index))
          (puthash key t seen)
          (setq combined (append combined (list event))))))
    (puthash session-id
             (sort combined
                   (lambda (left right)
                     (< (agent-harbor-timeline-event-timestamp left)
                        (agent-harbor-timeline-event-timestamp right))))
             agent-harbor-sidebar--session-events)))

(defun agent-harbor-sidebar--history-events (history)
  "Return chronological events from daemon HISTORY response."
  (nreverse (copy-sequence
             (or (agent-harbor-sidebar--get "events" history) nil))))

(defun agent-harbor-sidebar--start-sse (session-id)
  "Start SSE for SESSION-ID if it is not already connected."
  (when (and agent-harbor-daemon-endpoint
             (not (gethash session-id agent-harbor-sidebar--sse-clients)))
    (let* ((url (concat (agent-harbor-rest-api-base-url agent-harbor-daemon-endpoint)
                        "/sessions/" (url-hexify-string session-id) "/events"))
           (client
            (agent-harbor-sse-open
             url
             (lambda (frame)
               (let ((data (agent-harbor-sidebar--get "data" frame)))
                 (when (listp data)
                   (setq agent-harbor-sidebar--sse-event-count
                         (1+ agent-harbor-sidebar--sse-event-count))
                   (agent-harbor-sidebar--merge-events session-id (list data))
                   (agent-harbor-sidebar-render))))
             (lambda (_event) nil))))
      (puthash session-id client agent-harbor-sidebar--sse-clients))))

(defun agent-harbor-sidebar--stop-stale-sse (live-session-ids)
  "Stop SSE clients whose sessions are not in LIVE-SESSION-IDS."
  (maphash
   (lambda (session-id client)
     (unless (member session-id live-session-ids)
       (agent-harbor-sse-close client)
       (remhash session-id agent-harbor-sidebar--sse-clients)))
   agent-harbor-sidebar--sse-clients))

(defun agent-harbor-sidebar--api-url (&rest segments)
  "Join SEGMENTS onto the discovered daemon API base URL."
  (apply #'concat
         (agent-harbor-rest-api-base-url agent-harbor-daemon-endpoint)
         segments))

(defun agent-harbor-sidebar--inflight-inc ()
  "Register one outstanding async daemon request."
  (setq agent-harbor-sidebar--inflight-count
        (1+ agent-harbor-sidebar--inflight-count)))

(defun agent-harbor-sidebar--inflight-dec ()
  "Mark one async daemon request as resolved and finalize the sweep when idle."
  (setq agent-harbor-sidebar--inflight-count
        (max 0 (1- agent-harbor-sidebar--inflight-count)))
  (when (and agent-harbor-sidebar--refresh-active
             (zerop agent-harbor-sidebar--inflight-count))
    (setq agent-harbor-sidebar--refresh-active nil
          agent-harbor-sidebar--last-refresh-time (float-time))
    (agent-harbor-sidebar--stop-stale-sse
     agent-harbor-sidebar--live-session-ids))
  (agent-harbor-sidebar-render))

(defvar agent-harbor-sidebar--live-session-ids nil
  "Session ids seen during the current async refresh sweep.
Used to stop stale SSE clients once the sweep settles.")

(defun agent-harbor-sidebar--async-get (url on-success)
  "Issue an async GET to URL, invoking ON-SUCCESS with the parsed JSON.
Increments the in-flight counter immediately and decrements it (with a
re-render) when the request resolves, whether or not it succeeded.
Failures are swallowed at the leaf so a single slow/missing endpoint
does not strand the whole sweep, matching the sync code's per-call
`condition-case' guards."
  (agent-harbor-sidebar--inflight-inc)
  (agent-harbor-rest-get-json-async
   url
   (lambda (json)
     (condition-case nil
         (funcall on-success json)
       (error nil))
     (agent-harbor-sidebar--inflight-dec))
   (lambda (_message)
     (agent-harbor-sidebar--inflight-dec))))

(defun agent-harbor-sidebar--store-diff-files (task-id diff)
  "Store modified files for TASK-ID parsed from daemon DIFF response.

The daemon serializes `TaskDiffResponse' via Serde with
`rename_all = \"camelCase\"', so the JSON field for untracked entries is
`untrackedFiles', not `untracked_files'. `agent-harbor-sidebar--get'
accepts both string and symbol keys, but the camelCase-vs-snake_case mix
is real: query both spellings to stay compatible across the daemon's
serialization tweaks. We also coerce the untracked payload to a list
because `json-parse-string' returns either a list (plz backend) or a
vector (url.el backend) depending on the available HTTP client."
  (let* ((untracked (or (agent-harbor-sidebar--get "untrackedFiles" diff)
                        (agent-harbor-sidebar--get "untracked_files" diff)
                        nil))
         (stat (or (agent-harbor-sidebar--get "stat" diff) ""))
         (stat-files
          (delq nil
                (mapcar
                 (lambda (line)
                   (when (string-match "\\`[[:space:]]*\\([^|]+?\\)[[:space:]]*|" line)
                     (string-trim (match-string 1 line))))
                 (split-string stat "\n" t))))
         (untracked-list (cond
                          ((listp untracked) untracked)
                          ((vectorp untracked) (append untracked nil))
                          (t nil)))
         (files (append untracked-list stat-files)))
    (puthash task-id (delete-dups files) agent-harbor-sidebar--task-diff-files)
    (puthash task-id t agent-harbor-sidebar--diff-loaded)))

(defun agent-harbor-sidebar--refresh-diff-files (task-id)
  "Asynchronously refresh modified files for TASK-ID from the diff endpoint."
  (agent-harbor-sidebar--async-get
   (agent-harbor-sidebar--api-url "/tasks/" (url-hexify-string task-id) "/diff")
   (lambda (diff)
     (agent-harbor-sidebar--store-diff-files task-id diff))))

(defun agent-harbor-sidebar--refresh-feedback (task-id)
  "Asynchronously refresh feedback requests for TASK-ID."
  (agent-harbor-sidebar--async-get
   (agent-harbor-sidebar--api-url "/tasks/" (url-hexify-string task-id) "/feedback")
   (lambda (json)
     (puthash task-id
              (or (agent-harbor-sidebar--get "items" json) nil)
              agent-harbor-sidebar--task-feedback))))

(defun agent-harbor-sidebar--shallow-refresh-task (task-id)
  "Populate session-id metadata for TASK-ID with one REST round-trip.

Every accordion row needs at minimum the list of session ids that back
the task, otherwise `agent-harbor-sidebar-select-task' cannot resolve
`session-ids' for the switch target and the
`agent-harbor-workspace-reroot' call is silently skipped (its
`(when session-ids ...)' guard fires).  That guard is the spec contract
from Emacs-Extension-PRD step 1: capture outgoing-branch buffer state
and run the modified-buffer policy (prompt / stash / keep / kill).
Skipping it means a dirty-buffer switch never opens the policy dialog,
which is exactly the regression
`scenario_emacs_branch_switch_modified_prompt' guards.

This call is one cheap GET per task (a session-summary list, no per-event
fan-out); the heavy work (history, branches, diff, feedback, SSE) is
deferred to `agent-harbor-sidebar--deep-refresh-task' for the focused
task only."
  (when task-id
    (let ((sessions (condition-case nil
                        (agent-harbor-rest-task-sessions task-id)
                      (error nil))))
      (puthash task-id sessions agent-harbor-sidebar--task-sessions)
      sessions)))

(defun agent-harbor-sidebar--deep-refresh-sessions (task-id sessions)
  "Asynchronously deep-refresh SESSIONS belonging to TASK-ID.

Pulls the task's branches, diff, feedback, and each session's history
into the local store via non-blocking GETs.  Each leaf updates the local
store from its callback exactly as the former synchronous path did and
triggers a re-render, so the e2e poll loops observe the same converged
state once the requests resolve during `accept-process-output'."
  (puthash task-id sessions agent-harbor-sidebar--task-sessions)
  (let ((session-ids (delq nil (mapcar #'agent-harbor-sidebar--session-id sessions))))
    (agent-harbor-sidebar--refresh-diff-files task-id)
    (agent-harbor-sidebar--refresh-feedback task-id)
    ;; AgentHarborFS branches: one `/branches' GET returns every branch; filter to
    ;; this task's sessions and set the task entry in a single shot so a
    ;; mid-sweep state read never observes the focused task with no branches.
    (when session-ids
      (agent-harbor-sidebar--async-get
       (agent-harbor-sidebar--api-url "/branches")
       (lambda (json)
         (let ((branches (or (agent-harbor-sidebar--get "branches" json) nil)))
           ;; The /branches GET already returns every branch; cache the full
           ;; list so cross-task preview URIs (which reference another task's
           ;; branch) can be normalized name -> id without a render-time fetch.
           (setq agent-harbor-sidebar--all-branches branches)
           (puthash task-id
                    (cl-remove-if-not
                     (lambda (branch)
                       (member (or (agent-harbor-models--alist-get "sessionId" branch)
                                   (agent-harbor-models--alist-get "session_id" branch))
                               session-ids))
                     branches)
                    agent-harbor-sidebar--task-branches)))))
    (dolist (session sessions)
      (when-let ((session-id (agent-harbor-sidebar--session-id session)))
        (push session-id agent-harbor-sidebar--live-session-ids)
        ;; Session history.
        (agent-harbor-sidebar--async-get
         (agent-harbor-sidebar--api-url "/sessions/" (url-hexify-string session-id)
                                        "/events/history?limit=200")
         (lambda (history)
           (puthash session-id
                    (or (agent-harbor-sidebar--get "total_count" history) 0)
                    agent-harbor-sidebar--session-history-counts)
           (puthash session-id t agent-harbor-sidebar--history-loaded)
           (agent-harbor-sidebar--merge-events
            session-id (agent-harbor-sidebar--history-events history))))
        (agent-harbor-sidebar--start-sse session-id)))))

(defun agent-harbor-sidebar--deep-refresh-task (task-id)
  "Asynchronously deep-refresh the focused TASK-ID.

Fetches the task's session summary list, then chains the per-session
branch / history fan-out plus the task-level diff / feedback fetches.
Expensive (5-7 REST round-trips per call), so the sweep only runs it for
the focused task to keep the panel responsive on daemons that host many
task records."
  (when task-id
    (agent-harbor-sidebar--async-get
     (agent-harbor-sidebar--api-url "/tasks/" (url-hexify-string task-id) "/sessions")
     (lambda (json)
       (let ((sessions (or (agent-harbor-sidebar--get "sessions" json) nil)))
         (agent-harbor-sidebar--deep-refresh-sessions task-id sessions))))))

(defun agent-harbor-sidebar--preview-refresh-task (task-id)
  "Asynchronously load a non-focused TASK-ID for cross-task previews.

A focused task can preview another task's branch through the
agentharborfs scheme
\(see `agent-harbor-cross-task-files').  `agent-harbor-sidebar--timeline-cards'
surfaces such a foreign task's preview-bearing cards (an `edited' card resolves
to `agentharborfs://<branch-id>/<path>') into the focused timeline, but
only once that
task's session ids and per-session history have been pulled into the local
store.  The focused-only `agent-harbor-sidebar--deep-refresh-task' never touches
non-focused rows, so without this pass the cross-task preview card never
materialises.

This is deliberately a trimmed deep-refresh: one `/sessions' summary GET plus a
single history GET per session, and nothing else.  It omits the per-task
`/branches', diff, feedback, and SSE fan-out (branch ids resolve from the shared
`agent-harbor-sidebar--all-branches' cache instead), so the cost stays at the
order of one round-trip per non-focused session rather than the 5-7 the focused
deep-refresh pays."
  (when task-id
    (agent-harbor-sidebar--async-get
     (agent-harbor-sidebar--api-url "/tasks/" (url-hexify-string task-id) "/sessions")
     (lambda (json)
       (let ((sessions (or (agent-harbor-sidebar--get "sessions" json) nil)))
         (puthash task-id sessions agent-harbor-sidebar--task-sessions)
         (dolist (session sessions)
           (when-let ((session-id (agent-harbor-sidebar--session-id session)))
             (agent-harbor-sidebar--async-get
              (agent-harbor-sidebar--api-url
               "/sessions/" (url-hexify-string session-id)
               "/events/history?limit=200")
              (lambda (history)
                (puthash session-id
                         (or (agent-harbor-sidebar--get "total_count" history) 0)
                         agent-harbor-sidebar--session-history-counts)
                (puthash session-id t agent-harbor-sidebar--history-loaded)
                (agent-harbor-sidebar--merge-events
                 session-id
                 (agent-harbor-sidebar--history-events history)))))))))))

(defun agent-harbor-sidebar-refresh ()
  "Refresh Agent Harbor sidebar model from the daemon asynchronously.

The HTTP work is non-blocking: this function performs the synchronous
daemon discovery handshake (a fast local PID-file probe), then kicks off
the task / session / focused-task requests through
`agent-harbor-rest-get-json-async' and returns immediately.  Panel state
is updated and the buffer re-rendered from each request's callback as the
responses arrive, so the editor main loop stays responsive even when the
daemon is slow.

`agent-harbor-sidebar--refresh-active' is the re-entrancy guard: while a
sweep is outstanding, a fresh timer- or poll-driven call only re-renders
(picking up whatever has landed) instead of launching a duplicate,
double-counted set of requests.  The e2e harnesses drive this in a poll
loop and yield to the event loop via `accept-process-output'/`sleep-for'
between calls, during which the in-flight callbacks fire and converge the
state stores to the same shape the former synchronous path produced.

Only the currently focused task is deep-refreshed; non-focused accordion
rows render from the `list_tasks' header (status, title, session count)
alone.  `agent-harbor-sidebar-select-task' shallow-refreshes its target
on demand so switching to any row still resolves the session ids that
drive `agent-harbor-workspace-reroot' (the spec's modified-buffer-policy +
sandbox-diff switch contract) without paying that cost for rows the user
never visits."
  (interactive)
  ;; Daemon discovery is a cheap local handshake; keep it synchronous so the
  ;; connected/disconnected gate below is decided before issuing REST work.
  (condition-case err
      (agent-harbor-daemon-refresh)
    (error
     (setq agent-harbor-sidebar--last-error (error-message-string err))))
  (cond
   ((not (eq agent-harbor-daemon-state 'connected))
    (agent-harbor-sidebar-render))
   (agent-harbor-sidebar--refresh-active
    ;; A sweep is still settling; just re-render with whatever has arrived.
    (agent-harbor-sidebar-render))
   (t
    (setq agent-harbor-sidebar--last-error nil
          agent-harbor-sidebar--refresh-active t
          agent-harbor-sidebar--live-session-ids nil)
    ;; Hold the inflight count above zero across the whole sweep setup so an
    ;; early-resolving leaf cannot finalize the sweep before later requests
    ;; are even issued.
    (agent-harbor-sidebar--inflight-inc)
    ;; The stores are NOT cleared up-front: each focused-task entry is
    ;; overwritten atomically when its fresh response lands, and the full
    ;; session map is re-keyed in place.  Clearing here instead opened an empty
    ;; window during which a state read (the e2e poll loops read between
    ;; refreshes) would observe a focused task with no sessions / branch, which
    ;; the synchronous path — repopulating within a single call — never exposed.
    ;;
    ;; The focused task id usually carries over from the previous sweep, so kick
    ;; off its deep-refresh immediately and in parallel with the `/tasks' list
    ;; fetch rather than chaining behind it.  This halves the sweep's critical
    ;; path, which matters under heavy load where each round-trip nears the 5 s
    ;; ceiling: a faster sweep lets the focused session's event count keep
    ;; advancing between the e2e poll loop's stability ticks instead of
    ;; plateauing while a single long sweep is still in flight.
    (let ((primed-focus agent-harbor-sidebar--selected-task-id))
      (when primed-focus
        (agent-harbor-sidebar--deep-refresh-task primed-focus))
      (agent-harbor-sidebar--async-get
       (agent-harbor-sidebar--api-url "/tasks")
       (lambda (json)
         (setq agent-harbor-sidebar--tasks
               (or (agent-harbor-sidebar--get "tasks" json) nil)
               agent-harbor-sidebar--tasks-loaded t)
         ;; Only auto-focus the first task when nothing is selected yet (initial
         ;; open). Do NOT steal focus from an explicit selection just because it
         ;; is momentarily absent from an in-flight `/tasks' response — a
         ;; just-created task (set as selected by the task-entry submit) may not
         ;; appear in a list fetched a beat earlier, and drifting focus to the
         ;; first row (a loop-driver sub-agent task) here was the
         ;; new-task-flow regression.
         (when (null agent-harbor-sidebar--selected-task-id)
           (setq agent-harbor-sidebar--selected-task-id
                 (agent-harbor-sidebar--task-id (car agent-harbor-sidebar--tasks))))
         ;; Deep-refresh the focused task unless we already primed the same one
         ;; above (avoid a duplicate fan-out); a selection change still fans out.
         (when (and agent-harbor-sidebar--selected-task-id
                    (not (equal agent-harbor-sidebar--selected-task-id primed-focus)))
           (agent-harbor-sidebar--deep-refresh-task
            agent-harbor-sidebar--selected-task-id))
         ;; Lightly load non-focused tasks so their cross-task preview cards
         ;; (another task's branch referenced via the agentharborfs scheme) can
         ;; surface in the focused timeline.  This is a trimmed pass (sessions
         ;; + history only) — see `agent-harbor-sidebar--preview-refresh-task'.
         (dolist (task agent-harbor-sidebar--tasks)
           (let ((tid (agent-harbor-sidebar--task-id task)))
             (when (and tid
                        (not (equal tid agent-harbor-sidebar--selected-task-id)))
               (agent-harbor-sidebar--preview-refresh-task tid)))))))
    (agent-harbor-sidebar--async-get
     (agent-harbor-sidebar--api-url "/sessions?perPage=100")
     (lambda (json)
       (dolist (session (or (agent-harbor-sidebar--get "items" json) nil))
         (when-let ((session-id (agent-harbor-sidebar--session-id session)))
           (puthash session-id session agent-harbor-sidebar--full-sessions)))))
    ;; Release the setup hold; if everything above already resolved, this
    ;; finalizes the sweep.
    (agent-harbor-sidebar--inflight-dec))))

(defun agent-harbor-sidebar--timeline-session-events ()
  "Return selected timeline session events."
  (when-let ((session-id (agent-harbor-sidebar--selected-session-id)))
    (list (cons session-id (or (gethash session-id agent-harbor-sidebar--session-events) nil)))))

(defun agent-harbor-sidebar--selected-task-session-events ()
  "Return all session event streams for the selected task."
  (delq nil
        (mapcar
         (lambda (session)
           (when-let ((session-id (agent-harbor-sidebar--session-id session)))
             (cons session-id
                   (or (gethash session-id agent-harbor-sidebar--session-events) nil))))
         (agent-harbor-sidebar--selected-sessions))))

(defun agent-harbor-sidebar--channel-routing-indicator ()
  "Return the latest user presence route rendered for the focused task.

Presence-change / notification-route signals can arrive as log /
tool_output payloads inside a sub_agent envelope (the child session's
own message).  Unwrap before extracting the indicator text so the focused
task header surfaces the live route."
  (let (route)
    (dolist (session (agent-harbor-sidebar--selected-sessions))
      (when-let ((session-id (agent-harbor-sidebar--session-id session)))
        (dolist (event (or (gethash session-id agent-harbor-sidebar--session-events) nil))
          (let* ((effective (agent-harbor-timeline-effective-event event))
                 (message (or (agent-harbor-timeline--text-field "message" effective)
                              (agent-harbor-timeline--text-field "tool_output" effective)
                              (agent-harbor-timeline-event-text effective))))
            (when (and (stringp message)
                       (string-match
                        "presence change:.*active_channel: \"\\([^\"]+\\)\"" message))
              (setq route (match-string 1 message)))
            (when (and (stringp message)
                       (string-match "notification_route=\\([[:alnum:]_-]+\\)" message))
              (setq route (match-string 1 message)))))))
    (when route
      (format "Notifications routed to: %s" (capitalize route)))))

(defun agent-harbor-sidebar--timeline-cards ()
  "Return rendered timeline cards for the selected task."
  (let ((selected-id (agent-harbor-sidebar--selected-task-id))
        cards)
    (dolist (task agent-harbor-sidebar--tasks)
      (let* ((task-id (agent-harbor-sidebar--task-id task))
             (selected (equal task-id selected-id)))
        (dolist (session (agent-harbor-sidebar--task-sessions-for task-id))
          (let* ((session-id (agent-harbor-sidebar--session-id session))
                 (render-session (or (not selected)
                                     (equal session-id
                                            (agent-harbor-sidebar--selected-session-id))))
                 (session-cards
                  (and render-session
                       (agent-harbor-timeline-cards
                        (list (cons session-id
                                    (or (gethash session-id agent-harbor-sidebar--session-events)
                                        nil)))))))
            (dolist (card session-cards)
              (let ((with-preview
                     (agent-harbor-sidebar--card-with-preview card session task-id)))
                (when (or selected
                          (agent-harbor-sidebar--get "previewUri" with-preview))
                  (push with-preview cards))))))))
    (sort (nreverse cards)
          (lambda (left right)
            (< (or (agent-harbor-sidebar--get "timestamp" left) 0)
               (or (agent-harbor-sidebar--get "timestamp" right) 0))))))

(defun agent-harbor-sidebar--modified-files ()
  "Return modified files for the selected task.

Unwrap sub_agent envelopes so a child session's file_edit / diff events
still register as modified files under the parent task. Unlike the
timeline card list, this scans every selected-task session rather than
only the selected stream so the task summary does not drop files edited
by implementation or supervisor fan-out sessions.

We `copy-sequence' the diff-file list before splicing because `append'
shares its last argument's tail with the result, and the subsequent
`delete-dups' / `delq' / `sort' calls below mutate the spine in place.
Without the copy, those mutations would corrupt the cached value stored
in `agent-harbor-sidebar--task-diff-files', shrinking it on every
side-panel render."
  (let ((files nil))
    (dolist (entry (agent-harbor-sidebar--selected-task-session-events))
      (dolist (event (cdr entry))
        (let ((effective (agent-harbor-timeline-effective-event event)))
          (when (member (agent-harbor-timeline--event-type effective) '("file_edit" "diff"))
            (when-let ((file (agent-harbor-timeline--event-file-path effective)))
              (push file files))))))
    (when-let ((task-id (agent-harbor-sidebar--selected-task-id)))
      (let ((diff-files (gethash task-id agent-harbor-sidebar--task-diff-files)))
        (setq files (append files (copy-sequence diff-files)))))
    (sort (delete-dups (delq nil files)) #'string<)))

(defun agent-harbor-sidebar--insert-task-row (task selected)
  "Insert TASK row, marking SELECTED when non-nil."
  (let* ((task-id (agent-harbor-sidebar--task-id task))
         (pending-feedback (agent-harbor-sidebar--pending-feedback-for-task task-id))
         (sessions (agent-harbor-sidebar--task-sessions-for task-id))
         (session-ids (mapcar #'agent-harbor-sidebar--session-id sessions))
         (branches (delete-dups
                    (delq nil (mapcar #'agent-harbor-sidebar--branch-id sessions))))
         (start (point)))
    (insert (format "%s %s  %s\n"
                    (if selected "*" " ")
                    (agent-harbor-sidebar--task-status task)
                    (agent-harbor-sidebar--task-title task)))
    (insert (format "    task: %s\n" task-id))
    (when session-ids
      (insert (format "    sessions: %s\n" (string-join session-ids ", "))))
    (when branches
      (insert (format "    branches: %s\n" (string-join branches ", "))))
    (add-text-properties start (point)
                         `(agent-harbor-task-id ,task-id
                           agent-harbor-feedback-pending ,(and pending-feedback t)
                           read-only t
                           ,@(when pending-feedback
                               `(face agent-harbor-feedback-task-row))))))

(defun agent-harbor-sidebar--insert-action-row ()
  "Insert the sidebar action row."
  (let ((start (point)))
    (insert "[New Task]")
    (add-text-properties start (point)
                         '(agent-harbor-action new-task
                           mouse-face highlight
                           read-only t))
    (insert " ")
    (setq start (point))
    (insert "[Docs]\n")
    (add-text-properties start (point)
                         '(agent-harbor-action docs
                           mouse-face highlight
                           read-only t))))

(defun agent-harbor-sidebar--insert-focused-action-row (task-id)
  "Insert focused task actions for TASK-ID."
  (let ((start (point)))
    (insert "Actions: [Share Task] [Reply] [New Task]\n")
    (add-text-properties start (point)
                         `(agent-harbor-task-id ,task-id
                           agent-harbor-action share-task
                           mouse-face highlight
                           read-only t))))

(defun agent-harbor-sidebar--insert-feedback-banner (task-id feedback)
  "Insert pending feedback banner for TASK-ID and FEEDBACK."
  (let ((start (point)))
    (insert (format "Feedback required: %s\n"
                    (or (agent-harbor-sidebar--get "question" feedback)
                        "Response required")))
    (insert (format "Feedback: %s  Task: %s\n"
                    (agent-harbor-sidebar--get "id" feedback)
                    task-id))
    (when (equal (agent-harbor-sidebar--get "type" feedback) "confirmation")
      (insert "Actions: [Approve] [Reject]\n"))
    (add-text-properties start (point)
                         `(face agent-harbor-feedback-banner
                           agent-harbor-feedback-id ,(agent-harbor-sidebar--get "id" feedback)
                           agent-harbor-task-id ,task-id
                           read-only t))))

(defun agent-harbor-sidebar--spinner-frame ()
  "Return the current animated spinner frame string."
  (aref agent-harbor-sidebar--spinner-frames
        (mod agent-harbor-sidebar--spinner-index
             (length agent-harbor-sidebar--spinner-frames))))

(defun agent-harbor-sidebar--seconds-ago (time)
  "Return whole seconds elapsed since TIME, or nil when TIME is nil."
  (when time
    (max 0 (floor (- (float-time) time)))))

(defun agent-harbor-sidebar--retry-seconds ()
  "Return the seconds until the next daemon discovery retry."
  (floor (max 1 (or (and (boundp 'agent-harbor-daemon-poll-interval)
                         agent-harbor-daemon-poll-interval)
                    1))))

(defun agent-harbor-sidebar--daemon-progress-line ()
  "Return the Balanced daemon status line text.

While requests are in flight the line shows an animated spinner; when
idle and connected it reports how long ago the last refresh completed;
when disconnected it surfaces the retry/backoff cadence."
  (pcase agent-harbor-daemon-state
    ('connected
     (if (> agent-harbor-sidebar--inflight-count 0)
         (format "Daemon: connected   %s refreshing…"
                 (agent-harbor-sidebar--spinner-frame))
       (let ((ago (agent-harbor-sidebar--seconds-ago
                   agent-harbor-sidebar--last-refresh-time)))
         (if ago
             (format "Daemon: connected · updated %ds ago" ago)
           "Daemon: connected"))))
    ('starting
     (format "Daemon: connecting   %s" (agent-harbor-sidebar--spinner-frame)))
    (_
     (format "Daemon: unreachable — retrying in %ds"
             (agent-harbor-sidebar--retry-seconds)))))

(defun agent-harbor-sidebar--submitting-p ()
  "Return non-nil when a task-entry or reply submission is in progress."
  (or (let ((entry (agent-harbor-task-entry-test-state)))
        (eq (alist-get 'submitting entry) t))
      (let ((reply (agent-harbor-reply-test-state)))
        (eq (alist-get 'submitting reply) t))))

(defun agent-harbor-sidebar-render ()
  "Render the Agent Harbor sidebar from current model state."
  ;; Advance the animated spinner on every render while requests are in flight
  ;; so the in-flight indicator visibly cycles even during a long sweep.
  (when (> agent-harbor-sidebar--inflight-count 0)
    (setq agent-harbor-sidebar--spinner-index
          (1+ agent-harbor-sidebar--spinner-index)))
  (when-let ((buffer (get-buffer agent-harbor-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (point-was (point)))
        (erase-buffer)
        (agent-harbor-sidebar-mode)
        (agent-harbor--install-header-line)
        (insert "Agent Harbor\n\n")
        (insert (format "Daemon: %s\n"
                        (substring-no-properties (agent-harbor-header-line))))
        (insert (format "%s\n" (agent-harbor-sidebar--daemon-progress-line)))
        (when agent-harbor-sidebar--last-error
          (insert (format "Error: %s\n" agent-harbor-sidebar--last-error)))
        (when (agent-harbor-sidebar--submitting-p)
          (insert "Submitting…\n"))
        (insert "\n")
        (agent-harbor-sidebar--insert-action-row)
        (insert "\n")
        (let ((task-count (length agent-harbor-sidebar--tasks)))
          (insert (format "%s Active Tasks (%d)\n"
                          (if agent-harbor-sidebar--accordion-collapsed-p "[+]" "[-]")
                          task-count)))
        (if agent-harbor-sidebar--accordion-collapsed-p
            (when-let ((task (agent-harbor-sidebar--selected-task)))
              (insert (format "  %s\n" (agent-harbor-sidebar--task-title task))))
          (if agent-harbor-sidebar--tasks
              (dolist (task agent-harbor-sidebar--tasks)
                (agent-harbor-sidebar--insert-task-row
                 task
                 (equal (agent-harbor-sidebar--task-id task)
                        (agent-harbor-sidebar--selected-task-id))))
            (insert (if agent-harbor-sidebar--tasks-loaded
                        "  No active tasks.\n"
                      "  Loading tasks…\n"))))
        (insert "\n")
        (if-let ((task (agent-harbor-sidebar--selected-task)))
            (progn
              (insert (format "Focused: %s\n" (agent-harbor-sidebar--task-title task)))
              (insert (format "Status: %s\n" (agent-harbor-sidebar--task-status task)))
              (insert (format "Task: %s\n" (agent-harbor-sidebar--task-id task)))
              (when-let ((session-id (agent-harbor-sidebar--selected-session-id)))
                (insert (format "Session: %s\n" session-id)))
              (when-let ((branch (agent-harbor-sidebar--selected-branch-id)))
                (insert (format "Branch: %s\n" branch)))
              (agent-harbor-sidebar--insert-focused-action-row
               (agent-harbor-sidebar--task-id task)))
          (insert "Focused: none\n"))
        (when-let* ((task-id (agent-harbor-sidebar--selected-task-id))
                    (feedback (agent-harbor-sidebar--pending-feedback-for-task task-id)))
          (insert "\n")
          (agent-harbor-sidebar--insert-feedback-banner task-id feedback))
        (insert "\nModified Files\n")
        (let* ((task-id (agent-harbor-sidebar--selected-task-id))
               (diff-known (and task-id
                                (gethash task-id agent-harbor-sidebar--diff-loaded)))
               (files (agent-harbor-sidebar--modified-files)))
          (cond
           (files
            (dolist (file files) (insert (format "  %s\n" file))))
           (diff-known (insert "  None\n"))
           (t (insert "  Loading…\n"))))
        (insert "\n")
        (let ((cards (agent-harbor-sidebar--timeline-cards))
              (session-id (agent-harbor-sidebar--selected-session-id)))
          (if (and (null cards)
                   session-id
                   (not (gethash session-id agent-harbor-sidebar--history-loaded)))
              (insert "Loading activity…\n")
            (agent-harbor-timeline-insert cards)))
        (agent-harbor-reply-render-overlay
         (agent-harbor-sidebar--selected-task-id)
         (agent-harbor-sidebar--selected-pending-feedback))
        (goto-char (min (point-max) point-was))))))

(defun agent-harbor-sidebar-open ()
  "Open the Agent Harbor buffer in a side window."
  (let ((buffer (get-buffer-create agent-harbor-buffer-name)))
    (with-current-buffer buffer
      (agent-harbor-sidebar-mode)
      (agent-harbor--install-header-line))
    (display-buffer-in-side-window
     buffer
     `((side . right)
       (slot . 0)
       (window-width . ,agent-harbor-sidebar-width)
       (dedicated . t)))
    (agent-harbor-daemon-start-monitor)
    (agent-harbor-sidebar-start-refresh)
    (agent-harbor-sidebar-refresh)
    buffer))

(defun agent-harbor-sidebar-start-refresh ()
  "Start the sidebar refresh timer."
  (unless (timerp agent-harbor-sidebar--refresh-timer)
    (setq agent-harbor-sidebar--refresh-timer
          (run-at-time agent-harbor-sidebar-refresh-interval
                       agent-harbor-sidebar-refresh-interval
                       #'agent-harbor-sidebar-refresh))))

(defun agent-harbor-sidebar-stop-refresh ()
  "Stop the sidebar refresh timer and SSE clients."
  (when (timerp agent-harbor-sidebar--refresh-timer)
    (cancel-timer agent-harbor-sidebar--refresh-timer)
    (setq agent-harbor-sidebar--refresh-timer nil))
  (maphash (lambda (_session-id client) (agent-harbor-sse-close client))
           agent-harbor-sidebar--sse-clients)
  (clrhash agent-harbor-sidebar--sse-clients))

(defun agent-harbor-sidebar-toggle-accordion ()
  "Toggle the Active Tasks accordion."
  (interactive)
  (setq agent-harbor-sidebar--accordion-collapsed-p
        (not agent-harbor-sidebar--accordion-collapsed-p))
  (agent-harbor-sidebar-render))

(defun agent-harbor-sidebar--completion-candidates ()
  "Return completing-read candidates for current tasks."
  (mapcar (lambda (task)
            (cons (format "%s  %s"
                          (agent-harbor-sidebar--task-title task)
                          (agent-harbor-sidebar--task-id task))
                  (agent-harbor-sidebar--task-id task)))
          agent-harbor-sidebar--tasks))

(defun agent-harbor-sidebar--run-action-at-point ()
  "Run a sidebar action at point, returning non-nil when handled."
  (pcase (get-text-property (point) 'agent-harbor-action)
    ('new-task
     (agent-harbor-sidebar-new-task)
     t)
    ('share-task
     (agent-harbor-share-task
      (get-text-property (point) 'agent-harbor-task-id))
     t)
    ('docs
     (agent-harbor-open-documentation)
     t)
    (_ nil)))

(defun agent-harbor-sidebar-select-task ()
  "Select and focus a task through `completing-read'."
  (interactive)
  (unless (or (agent-harbor-sidebar-open-card-at-point)
              (agent-harbor-sidebar--run-action-at-point))
    (let* ((candidates (agent-harbor-sidebar--completion-candidates))
           (choice (completing-read "Agent Harbor task: "
                                    (mapcar #'car candidates)
                                    nil t nil nil (caar candidates)))
           (task-id (cdr (assoc choice candidates))))
      (unless task-id
        (setq task-id choice))
      (when (and task-id (not (equal task-id agent-harbor-sidebar--selected-task-id)))
        ;; `agent-harbor-sidebar-refresh' no longer shallow-refreshes every row
        ;; (that was an N+1 over all task records), so populate the target's
        ;; session-id mapping on demand here — the switch contract below needs it
        ;; to drive `agent-harbor-workspace-reroot'.
        (unless (gethash task-id agent-harbor-sidebar--task-sessions)
          (agent-harbor-sidebar--shallow-refresh-task task-id))
        (let ((session-ids (agent-harbor-sidebar--session-ids-for-task task-id))
              (branch-id (agent-harbor-sidebar--branch-id-for-task task-id)))
          (when session-ids
            (condition-case err
                (agent-harbor-workspace-reroot task-id session-ids branch-id)
              (user-error
               (unless (string-match-p "No AgentHarborFS branch found" (error-message-string err))
                 (signal (car err) (cdr err))))))))
      (setq agent-harbor-sidebar--selected-task-id task-id
            agent-harbor-sidebar--accordion-collapsed-p nil)
      (agent-harbor-sidebar-render)
      task-id)))

(defun agent-harbor-sidebar-mouse-1 (event)
  "Open a preview card or select a task at mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (or (agent-harbor-sidebar-open-card-at-point)
      (agent-harbor-sidebar--run-action-at-point)
      (agent-harbor-sidebar-select-task)))

(defun agent-harbor-sidebar--preview-banner (branch-id path)
  "Return the read-only preview banner for BRANCH-ID PATH."
  (format "AgentHarborFS preview is read-only: %s/%s" branch-id path))

(defun agent-harbor-sidebar--open-preview-buffer (file root branch-id path)
  "Open FILE as a read-only preview rooted at ROOT for BRANCH-ID PATH."
  (let* ((default-directory (file-name-as-directory root))
         (buffer (find-file-noselect file t)))
    (with-current-buffer buffer
      (setq-local default-directory (file-name-as-directory root))
      (setq-local buffer-read-only t)
      (setq-local agent-harbor-preview t)
      (setq-local agent-harbor-preview-branch-id branch-id)
      (setq-local agent-harbor-preview-path path)
      (setq-local header-line-format
                  (propertize (agent-harbor-sidebar--preview-banner branch-id path)
                              'face 'agent-harbor-feedback-banner)))
    (pop-to-buffer buffer)
    buffer))

(defun agent-harbor-sidebar--clear-preview-buffer-state (buffer)
  "Clear Agent Harbor preview metadata from BUFFER before normal editing."
  (with-current-buffer buffer
    (let ((preview-banner
           (and (stringp header-line-format)
                (string-prefix-p "AgentHarborFS preview is read-only:"
                                 (substring-no-properties header-line-format)))))
      (setq-local buffer-read-only nil)
      (dolist (variable '(agent-harbor-preview
                          agent-harbor-preview-branch-id
                          agent-harbor-preview-path))
        (when (local-variable-p variable)
          (kill-local-variable variable)))
      (when preview-banner
        (kill-local-variable 'header-line-format)))))

(defun agent-harbor-sidebar--open-normal-file (file root)
  "Open FILE normally with `default-directory' rooted at ROOT.

A cross-task switch/new-frame open can run right after the read-only
preview warmup left a buffer visiting FILE.  A plain `find-file-noselect'
would then notice the warmup buffer's visited-modtime no longer matches
the on-disk file (the intervening reroot can rewrite the workspace file)
and pop the interactive \"changed on disk.  Reread?\" `yes-or-no-p',
which deadlocks a batch Emacs on stdin.  Reuse the existing buffer and
reload it from disk non-interactively instead, so the writable open is
deterministic in both interactive and batch contexts."
  (let* ((default-directory (file-name-as-directory root))
         (existing (find-buffer-visiting file))
         (buffer (or existing (find-file-noselect file t))))
    (when existing
      (with-current-buffer existing
        (let ((inhibit-read-only t))
          ;; ignore-auto + noconfirm + preserve-modes: re-read the current
          ;; on-disk bytes without prompting and without losing the buffer's
          ;; major mode.
          (revert-buffer t t t))))
    (with-current-buffer buffer
      (setq-local default-directory (file-name-as-directory root)))
    (agent-harbor-sidebar--clear-preview-buffer-state buffer)
    (pop-to-buffer buffer)
    buffer))

(defun agent-harbor-sidebar--reroot-to-cross-task (task-id branch-id)
  "Re-root the current editor context to TASK-ID BRANCH-ID."
  (unless task-id
    (user-error "Unable to resolve source task for AgentHarborFS preview"))
  (agent-harbor-workspace-reroot
   task-id
   (agent-harbor-sidebar--session-ids-for-task task-id)
   branch-id)
  (setq agent-harbor-sidebar--selected-task-id task-id
        agent-harbor-sidebar--accordion-collapsed-p nil)
  (agent-harbor-sidebar-render))

(defun agent-harbor-sidebar--make-cross-task-frame ()
  "Create and select a frame for a cross-task open."
  (condition-case err
      (let ((frame (make-frame)))
        (select-frame-set-input-focus frame)
        frame)
    (error
     (if noninteractive
         (progn
           (message "Unable to create cross-task frame in batch Emacs: %s"
                    (error-message-string err))
           (selected-frame))
       (signal (car err) (cdr err))))))

(defun agent-harbor-sidebar--open-card-preview (card)
  "Open the AgentHarborFS preview represented by CARD."
  (let* ((parsed (agent-harbor-sidebar--agentharborfs-preview
                  (agent-harbor-sidebar--get "previewUri" card)))
         (branch-ref (agent-harbor-sidebar--get "branchRef" parsed))
         (path (agent-harbor-sidebar--get "path" parsed))
         (branch (and branch-ref
                      (agent-harbor-sidebar--branch-for-id-or-name branch-ref)))
         (branch-id (agent-harbor-sidebar--get "id" branch))
         (root (agent-harbor-sidebar--branch-workspace-path branch))
         (source-task-id (or (agent-harbor-sidebar--get "sourceTaskId" card)
                             (and branch-id
                                  (cl-loop for task in agent-harbor-sidebar--tasks
                                           for task-id = (agent-harbor-sidebar--task-id task)
                                           when (equal branch-id
                                                       (agent-harbor-sidebar--branch-id-for-task
                                                        task-id))
                                           return task-id))))
         (file (and root path (expand-file-name path root))))
    (unless (and branch-id root path file)
      (user-error "Unable to resolve AgentHarborFS preview for %s"
                  (agent-harbor-sidebar--get "previewUri" card)))
    (pcase agent-harbor-cross-task-files
      ('preview
       (agent-harbor-sidebar--open-preview-buffer file root branch-id path))
      ('switch
       (agent-harbor-sidebar--reroot-to-cross-task source-task-id branch-id)
       (agent-harbor-sidebar--open-normal-file file root))
      ('new-frame
       (agent-harbor-sidebar--make-cross-task-frame)
       (agent-harbor-sidebar--reroot-to-cross-task source-task-id branch-id)
       (agent-harbor-sidebar--open-normal-file file root))
      (_
       (user-error "unsupported agent-harbor-cross-task-files mode: %s"
                   agent-harbor-cross-task-files)))))

(defun agent-harbor-sidebar-open-card-at-point ()
  "Open the preview card at point when one is available."
  (interactive)
  (when-let* ((card (get-text-property (point) 'agent-harbor-card-data))
              (_ (agent-harbor-sidebar--get "previewUri" card)))
    (agent-harbor-sidebar--open-card-preview card)
    t))

(defun agent-harbor-sidebar-new-task ()
  "Open the Agent Harbor task-entry buffer from the sidebar."
  (interactive)
  (agent-harbor-task-entry-open))

(defun agent-harbor-share-task (&optional task-id ttl)
  "Create and copy a scoped share URL for TASK-ID using TTL."
  (interactive)
  (let* ((task-id (or task-id
                      (get-text-property (point) 'agent-harbor-task-id)
                      (agent-harbor-sidebar--selected-task-id)))
         (branch-id (and task-id (agent-harbor-sidebar--branch-id-for-task task-id)))
         (ttl (or ttl agent-harbor-share-task-default-ttl)))
    (unless task-id
      (user-error "No Agent Harbor task is selected"))
    (unless branch-id
      (user-error "Task %s does not have an AgentHarborFS branch to share" task-id))
    (let* ((share (agent-harbor-rest-create-agentharborfs-share branch-id "branch" ttl))
           (url (agent-harbor-sidebar--get "url" share)))
      (unless (and (stringp url) (not (string-empty-p url)))
        (user-error "Share response did not include a URL"))
      (kill-new url)
      (setq agent-harbor-sidebar-last-share
            `((taskId . ,task-id)
              (branchId . ,branch-id)
              (scope . "branch")
              (ttl . ,ttl)
              (url . ,url)
              (response . ,share)))
      (message "Agent Harbor share URL copied")
      url)))

(defun agent-harbor-sidebar-test-select-task (task-id)
  "Select TASK-ID by exercising the same completion function as RET."
  (let* ((candidate (car (rassoc task-id (agent-harbor-sidebar--completion-candidates)))))
    (unless candidate
      (error "No task candidate for %s" task-id))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) candidate)))
      (agent-harbor-sidebar-select-task))))

(defun agent-harbor-sidebar-focus-task (task-id)
  "Focus TASK-ID in the Agent Harbor sidebar and refresh its metadata."
  (setq agent-harbor-sidebar--selected-task-id task-id
        agent-harbor-sidebar--selected-stream-session-id nil
        agent-harbor-sidebar--accordion-collapsed-p nil)
  (agent-harbor-sidebar-refresh)
  (agent-harbor-sidebar-render)
  (agent-harbor-sidebar--selected-task-id))

(defun agent-harbor-sidebar-select-stream (session-id)
  "Select SESSION-ID as the focused activity stream for the current task."
  (interactive
   (list (completing-read "Session: "
                          (agent-harbor-sidebar--session-ids-for-task
                           (agent-harbor-sidebar--selected-task-id))
                          nil t)))
  (unless (member session-id
                  (agent-harbor-sidebar--session-ids-for-task
                   (agent-harbor-sidebar--selected-task-id)))
    (signal 'user-error (list (format "Session %s is not part of the selected task" session-id))))
  (setq agent-harbor-sidebar--selected-stream-session-id session-id)
  (agent-harbor-sidebar-render)
  session-id)

(defun agent-harbor-sidebar-tab ()
  "Toggle task accordion at its header, otherwise toggle a timeline card."
  (interactive)
  (if (get-text-property (point) 'agent-harbor-card-id)
      (agent-harbor-timeline-toggle-card)
    (agent-harbor-sidebar-toggle-accordion)))

(defun agent-harbor-test-side-panel-state ()
  "Return structured Lisp data for automated Agent Harbor sidebar tests."
  (let* ((task (agent-harbor-sidebar--selected-task))
         (task-id (agent-harbor-sidebar--selected-task-id))
         (sessions (agent-harbor-sidebar--selected-sessions))
         (cards (agent-harbor-sidebar--timeline-cards))
         (history-total 0))
    (when-let ((session-id (agent-harbor-sidebar--selected-session-id)))
      (setq history-total
            (or (gethash session-id agent-harbor-sidebar--session-history-counts)
                0)))
    `((daemon . ((state . ,(symbol-name agent-harbor-daemon-state))
                 (endpoint . ,agent-harbor-daemon-endpoint)
                 (error . ,agent-harbor-daemon-last-error)))
      (header . ((newTaskButtonVisible . t)))
      (progress . ((inFlight . ,agent-harbor-sidebar--inflight-count)
                   (refreshing . ,(if (> agent-harbor-sidebar--inflight-count 0)
                                      t :false))
                   (lastRefreshSecondsAgo
                    . ,(or (agent-harbor-sidebar--seconds-ago
                            agent-harbor-sidebar--last-refresh-time)
                           :null))
                   (daemonLine . ,(agent-harbor-sidebar--daemon-progress-line))
                   (tasksLoaded . ,(if agent-harbor-sidebar--tasks-loaded t :false))
                   (diffLoaded . ,(if (when-let ((tid (agent-harbor-sidebar--selected-task-id)))
                                        (gethash tid agent-harbor-sidebar--diff-loaded))
                                      t :false))
                   (historyLoaded . ,(if (when-let ((sid (agent-harbor-sidebar--selected-session-id)))
                                           (gethash sid agent-harbor-sidebar--history-loaded))
                                         t :false))
                   (submitting . ,(if (agent-harbor-sidebar--submitting-p) t :false))))
      (tasks . ,(vconcat
                 (mapcar (lambda (task)
                           `((id . ,(agent-harbor-sidebar--task-id task))
                             (status . ,(agent-harbor-sidebar--task-status task))
                             (title . ,(agent-harbor-sidebar--task-title task))
                             (sessionIds . ,(vconcat
                                             (mapcar #'agent-harbor-sidebar--session-id
                                                     (agent-harbor-sidebar--task-sessions-for
                                                      (agent-harbor-sidebar--task-id task)))))
                             (branchIds . ,(vconcat
                                            (agent-harbor-sidebar--branch-ids-for-task
                                             (agent-harbor-sidebar--task-id task))))))
                         agent-harbor-sidebar--tasks)))
      (activeTasks . ((filter . nil)
                      (count . ,(length agent-harbor-sidebar--tasks))
                      (dropdownAvailable . ,(if agent-harbor-sidebar--tasks t :false))))
      (focusedTask . ((title . ,(and task (agent-harbor-sidebar--task-title task)))
                      (status . ,(and task (agent-harbor-sidebar--task-status task)))
                      (taskId . ,task-id)
                      (agentCount . ,(length sessions))
                      (streamOptions . ,(vconcat
                                         (mapcar #'agent-harbor-sidebar--session-id sessions)))
                      (actionRow . ,(vconcat '("share" "reply" "new-task")))
                      (branchId . ,(agent-harbor-sidebar--selected-branch-id))))
      (selectedTaskId . ,task-id)
      (selectedSessionId . ,(agent-harbor-sidebar--selected-session-id))
      (selectedBranchId . ,(agent-harbor-sidebar--selected-branch-id))
      (lastShare . ,agent-harbor-sidebar-last-share)
      (focusedTitle . ,(and task (agent-harbor-sidebar--task-title task)))
      (focusedStatus . ,(and task (agent-harbor-sidebar--task-status task)))
      (pendingFeedbackTaskIds . ,(vconcat (agent-harbor-sidebar--pending-feedback-task-ids)))
      (feedbackBanner . ,(when-let ((feedback (agent-harbor-sidebar--selected-pending-feedback)))
                           `((feedbackId . ,(agent-harbor-sidebar--get "id" feedback))
                             (question . ,(agent-harbor-sidebar--get "question" feedback))
                             (type . ,(agent-harbor-sidebar--get "type" feedback)))))
      (replyRows . ,(if (agent-harbor-sidebar--selected-pending-feedback) 1 0))
      (quickActionIds . ,(vconcat
                          (when (equal (agent-harbor-sidebar--get
                                        "type"
                                        (agent-harbor-sidebar--selected-pending-feedback))
                                       "confirmation")
                            '("reply-quick-approve" "reply-quick-reject"))))
      (replyOverlay . ,(agent-harbor-reply-test-state))
      (taskEntry . ,(agent-harbor-task-entry-test-state))
      (notifications . ((channelRoutingIndicator . ,(agent-harbor-sidebar--channel-routing-indicator))
                        (bannerText . ,(or agent-harbor-daemon-last-error
                                           (when-let ((feedback (agent-harbor-sidebar--selected-pending-feedback)))
                                             (agent-harbor-sidebar--get "question" feedback))))))
      (accordionCollapsed . ,(and agent-harbor-sidebar--accordion-collapsed-p t))
      (timelineEventCount . ,history-total)
      (sseEventCount . ,agent-harbor-sidebar--sse-event-count)
      (timeline . ((autoFollow . ,(if (bound-and-true-p agent-harbor-timeline-auto-follow)
                                      t
                                    :false))
                   (searchQuery . nil)))
      (timelineCards . ,(vconcat cards))
      (modifiedFiles . ,(vconcat (agent-harbor-sidebar--modified-files)))
      (daemonDiffModifiedFiles . ,(vconcat
                                   (when task-id
                                     (gethash task-id agent-harbor-sidebar--task-diff-files))))
      (daemonDiff . ((ok . ,(if (and task-id
                                      (gethash task-id agent-harbor-sidebar--task-diff-files))
                                 t
                               :false))
                     (error . nil)
                     (modifiedFiles . ,(vconcat
                                        (when task-id
                                          (gethash task-id agent-harbor-sidebar--task-diff-files)))))))))

(defun agent-harbor-test-cross-task-preview-state ()
  "Return state from the current cross-task file buffer."
  (let ((buffer (current-buffer)))
    `((bufferName . ,(buffer-name buffer))
      (fileName . ,buffer-file-name)
      (defaultDirectory . ,default-directory)
      (readOnly . ,(if buffer-read-only t :json-false))
      (modified . ,(if (buffer-modified-p) t :json-false))
      (preview . ,(if (and (boundp 'agent-harbor-preview)
                           agent-harbor-preview)
                      t
                    :json-false))
      (headerLine . ,(if (stringp header-line-format)
                         (substring-no-properties header-line-format)
                       (format-mode-line header-line-format)))
      (branchId . ,(and (boundp 'agent-harbor-preview-branch-id)
                        agent-harbor-preview-branch-id))
      (path . ,(and (boundp 'agent-harbor-preview-path)
                    agent-harbor-preview-path))
      (workspace . ,(agent-harbor-workspace-current-state))
      (frameCount . ,(length (frame-list)))
      (selectedFrameDefaultDirectory . ,(with-selected-frame (selected-frame)
                                          default-directory))
      (content . ,(buffer-substring-no-properties (point-min) (point-max))))))

(provide 'agent-harbor-sidebar)

;;; agent-harbor-sidebar.el ends here
