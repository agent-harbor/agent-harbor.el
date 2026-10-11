;;; agent-harbor-uri.el --- Agent Harbor URI dispatch -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Local handling for task routes and `agent-harbor://fs/...` imports.
;; Explicit HTTP hosts stay loopback-only; rendezvous host ids are forwarded
;; to the daemon for M39 resolution.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'agent-harbor-daemon)
(require 'agent-harbor-models)
(require 'agent-harbor-rest)
(require 'agent-harbor-sidebar)
(require 'agent-harbor-workspace)

(declare-function org-link-set-parameters "ol" (type &rest parameters))

(defvar agent-harbor-uri-last-result nil
  "Last successful Agent Harbor URI dispatch result.")

(defvar agent-harbor-uri-last-error nil
  "Last Agent Harbor URI dispatch error message.")

(defun agent-harbor-uri--segment (value label)
  "Validate VALUE as a single path segment named LABEL."
  (unless (and (stringp value) (not (string-empty-p (string-trim value))))
    (user-error "agent-harbor URI is missing %s" label))
  (when (string-match-p "[/\\]" value)
    (user-error "agent-harbor URI %s must be a single path segment" label))
  value)

(defun agent-harbor-uri--query-pairs (query)
  "Return parsed QUERY pairs."
  (and query (url-parse-query-string query)))

(defun agent-harbor-uri--query-value (query key)
  "Return first QUERY value for KEY."
  (when-let ((pair (assoc key (agent-harbor-uri--query-pairs query))))
    (cadr pair)))

(defun agent-harbor-uri--loopback-host-p (host)
  "Return non-nil when HOST is an explicit loopback HTTP URL."
  (let* ((parsed (url-generic-parse-url host))
         (scheme (url-type parsed))
         (host-name (downcase (or (url-host parsed) ""))))
    (and (member scheme '("http" "https"))
         (or (equal host-name "localhost")
             (equal host-name "127.0.0.1")
             (equal host-name "::1")
             (string-prefix-p "127." host-name)))))

(defun agent-harbor-uri--validate-import-host (host)
  "Validate HOST as either a rendezvous id or explicit loopback URL."
  (let* ((parsed (url-generic-parse-url host))
         (scheme (url-type parsed)))
    (when scheme
      (unless (member scheme '("http" "https"))
        (user-error "unsupported Agent Harbor import host %S: explicit URLs must use HTTP or HTTPS" host))
      (unless (agent-harbor-uri--loopback-host-p host)
        (user-error
         "unsupported Agent Harbor import host %S: direct HTTP imports must use a loopback URL; use a rendezvous host id for cross-machine imports"
         host)))))

(defun agent-harbor-uri-parse (uri)
  "Parse and validate URI into a route alist."
  (let* ((parsed (url-generic-parse-url uri))
         (scheme (url-type parsed)))
    (unless (equal scheme "agent-harbor")
      (user-error "unsupported URI scheme: %s" (or scheme "")))
    (when (or (url-user parsed) (url-password parsed))
      (user-error "agent-harbor URI credentials are not supported"))
    (let* ((filename (or (url-filename parsed) ""))
           (path-query (split-string filename "\\?" t))
           (raw-path (car path-query))
           (query (cadr path-query)))
      (when-let ((host (agent-harbor-uri--query-value query "host")))
        (agent-harbor-uri--validate-import-host host))
      (let* ((parts (mapcar #'url-unhex-string
                             (delq nil
                                   (append (list (url-host parsed))
                                           (split-string (or raw-path "") "/" t))))))
        (pcase parts
          (`("task" ,task-id)
           `((kind . task) (taskId . ,(agent-harbor-uri--segment task-id "task id"))))
          (`("fs" "branch" ,branch-id)
           `((kind . fs-branch)
             (branchId . ,(agent-harbor-uri--segment branch-id "branch id"))
             (host . ,(agent-harbor-uri--query-value query "host"))
             (token . ,(agent-harbor-uri--query-value query "token"))))
          (`("fs" "snapshot" ,snapshot-id)
           `((kind . fs-snapshot)
             (snapshotId . ,(agent-harbor-uri--segment snapshot-id "snapshot id"))
             (host . ,(agent-harbor-uri--query-value query "host"))
             (token . ,(agent-harbor-uri--query-value query "token"))))
          (_ (user-error "unsupported agent-harbor URI path: %s" (string-join parts "/"))))))))

(defun agent-harbor-uri--get (key alist)
  "Return KEY from ALIST."
  (agent-harbor-models--alist-get key alist))

(defun agent-harbor-uri--branch-session-id (branch)
  "Return session id for BRANCH."
  (or (agent-harbor-uri--get "sessionId" branch)
      (agent-harbor-uri--get "session_id" branch)))

(defun agent-harbor-uri--branch-root (branch)
  "Return local workspace path for BRANCH."
  (or (agent-harbor-uri--get "workspacePath" branch)
      (agent-harbor-uri--get "workspace_path" branch)))

(defun agent-harbor-uri--snapshot-root (snapshot)
  "Return local workspace path for SNAPSHOT."
  (or (agent-harbor-uri--get "workspacePath" snapshot)
      (agent-harbor-uri--get "workspace_path" snapshot)))

(defun agent-harbor-uri--ensure-daemon ()
  "Ensure daemon discovery has populated a REST endpoint."
  (unless (and agent-harbor-daemon-endpoint
               (eq agent-harbor-daemon-state 'connected))
    (agent-harbor-daemon-refresh))
  (unless agent-harbor-daemon-endpoint
    (user-error "Agent Harbor daemon is not connected: %s"
                (or agent-harbor-daemon-last-error "unknown error"))))

(defun agent-harbor-uri--branch-updated-at (branch)
  "Return updated timestamp for BRANCH."
  (or (agent-harbor-uri--get "updatedAt" branch)
      (agent-harbor-uri--get "updated_at" branch)
      ""))

(defun agent-harbor-uri--session-id (session)
  "Return id for SESSION."
  (agent-harbor-uri--get "id" session))

(defun agent-harbor-uri--session-branch-id (session)
  "Return branch id for SESSION."
  (or (agent-harbor-uri--get "agentharborfsBranchId" session)
      (agent-harbor-uri--get "agentharborfs_branch_id" session)))

(defun agent-harbor-uri--branches-for-sessions (session-ids)
  "Return branches associated with SESSION-IDS."
  (cl-loop for session-id in session-ids
           append (agent-harbor-rest-list-branches-raw session-id)))

(defun agent-harbor-uri--newest-branch (branches)
  "Return newest branch from BRANCHES."
  (car
   (sort (copy-sequence branches)
         (lambda (left right)
           (string< (agent-harbor-uri--branch-updated-at right)
                    (agent-harbor-uri--branch-updated-at left))))))

(defun agent-harbor-uri--task-branch (task-id &optional sessions)
  "Return (BRANCH . SESSION-ID) for TASK-ID.

When SESSIONS is non-nil, use it instead of fetching task sessions."
  (let* ((sessions (or sessions (agent-harbor-rest-task-sessions task-id)))
         (session-ids (delq nil (mapcar #'agent-harbor-uri--session-id sessions)))
         (branches (agent-harbor-uri--branches-for-sessions session-ids)))
    (or
     (catch 'found
       (dolist (session sessions)
         (when-let ((branch-id (agent-harbor-uri--session-branch-id session)))
           (when-let ((branch (cl-find branch-id branches
                                       :key (lambda (candidate)
                                              (agent-harbor-uri--get "id" candidate))
                                       :test #'equal)))
             (throw 'found (cons branch (agent-harbor-uri--session-id session)))))))
     (when-let ((branch (agent-harbor-uri--newest-branch branches)))
       (cons branch (agent-harbor-uri--branch-session-id branch))))))

(defun agent-harbor-uri--focus-task-session (task-id session-id sessions)
  "Focus TASK-ID in the sidebar and select SESSION-ID from SESSIONS."
  (agent-harbor-sidebar-open)
  (puthash task-id sessions agent-harbor-sidebar--task-sessions)
  (agent-harbor-sidebar-focus-task task-id)
  (when (and session-id
             (member session-id
                     (delq nil (mapcar #'agent-harbor-uri--session-id sessions))))
    (setq agent-harbor-sidebar--selected-stream-session-id session-id)
    (agent-harbor-sidebar-render)))

(defun agent-harbor-uri--open-directory (root)
  "Open ROOT with the local editor's normal directory surface."
  (unless (and root (file-directory-p root))
    (user-error "Agent Harbor local path is not a directory: %s" root))
  (dired root)
  root)

(defun agent-harbor-uri--import-source-url (route)
  "Return daemon import source URL for fs ROUTE."
  (let* ((kind (agent-harbor-uri--get "kind" route))
         (source-kind (if (eq kind 'fs-branch) "branch" "snapshot"))
         (source-id (or (agent-harbor-uri--get "branchId" route)
                        (agent-harbor-uri--get "snapshotId" route)))
         (host (or (agent-harbor-uri--get "host" route)
                   agent-harbor-daemon-endpoint)))
    (agent-harbor-uri--validate-import-host host)
    (concat "agent-harbor://fs/" source-kind "/" (url-hexify-string source-id)
            "?" (agent-harbor-rest--query-string `(("host" . ,host))))))

(defun agent-harbor-uri--branch-status-evidence (branch-id)
  "Return lazy-fetch evidence for imported BRANCH-ID when available."
  (condition-case err
      (let ((status (agent-harbor-rest-branch-status-raw branch-id)))
        (or (agent-harbor-uri--get "import" status)
            (agent-harbor-uri--get "fetch" status)))
    (error `((error . ,(error-message-string err))))))

(defun agent-harbor-uri--import-fs-route (route)
  "Import fs ROUTE through the daemon and open its returned mount path."
  (let* ((source-url (agent-harbor-uri--import-source-url route))
         (token (agent-harbor-uri--get "token" route))
         (_ (message "Fetching %s..."
                     (if (eq (agent-harbor-uri--get "kind" route) 'fs-branch)
                         "branch"
                       "snapshot")))
         (import (agent-harbor-rest-create-agentharborfs-import source-url token nil "full-ahr"))
         (import-id (or (agent-harbor-uri--get "importId" import)
                        (agent-harbor-uri--get "import_id" import)))
         (branch-id (or (agent-harbor-uri--get "branchId" import)
                        (agent-harbor-uri--get "branch_id" import)))
         (mount-path (or (agent-harbor-uri--get "mountPath" import)
                         (agent-harbor-uri--get "mount_path" import)))
         (source-url-result (or (agent-harbor-uri--get "sourceUrl" import)
                                (agent-harbor-uri--get "source_url" import)
                                source-url))
         (evidence (and branch-id (agent-harbor-uri--branch-status-evidence branch-id))))
    (agent-harbor-uri--open-directory mount-path)
    `((kind . ,(agent-harbor-uri--get "kind" route))
      (branchId . ,branch-id)
      (snapshotId . ,(agent-harbor-uri--get "snapshotId" route))
      (importId . ,import-id)
      (mountPath . ,mount-path)
      (root . ,mount-path)
      (sourceUrl . ,source-url-result)
      (lazyFetchEvidence . ,evidence))))

(defun agent-harbor-uri--open-route (route)
  "Open validated ROUTE."
  (agent-harbor-uri--ensure-daemon)
  (pcase (agent-harbor-uri--get "kind" route)
    ('task
     (let* ((task-id (agent-harbor-uri--get "taskId" route))
            (sessions (agent-harbor-rest-task-sessions task-id))
            (target (agent-harbor-uri--task-branch task-id sessions))
            (branch (car-safe target))
            (session-id (cdr-safe target))
            (branch-id (and branch (agent-harbor-uri--get "id" branch)))
            (root (and branch (agent-harbor-uri--branch-root branch))))
       (unless (and branch-id root)
         (user-error "task %s does not have a local AgentHarborFS branch path" task-id))
       (agent-harbor-uri--focus-task-session task-id session-id sessions)
       (agent-harbor-workspace-reroot task-id (mapcar #'agent-harbor-uri--session-id
                                                      sessions)
                                      branch-id)
       `((kind . task) (taskId . ,task-id) (sessionId . ,session-id)
         (branchId . ,branch-id) (root . ,root))))
    ('fs-branch
     (agent-harbor-uri--import-fs-route route))
    ('fs-snapshot
     (agent-harbor-uri--import-fs-route route))
    (_ (user-error "unsupported Agent Harbor URI route: %S" route))))

;;;###autoload
(defun agent-harbor-uri-open (uri)
  "Open local Agent Harbor URI."
  (interactive "sAgent Harbor URI: ")
  (condition-case err
      (setq agent-harbor-uri-last-result
            (agent-harbor-uri--open-route (agent-harbor-uri-parse uri))
            agent-harbor-uri-last-error nil)
    (error
     (setq agent-harbor-uri-last-error (error-message-string err))
     (signal (car err) (cdr err)))))

;;;###autoload
(defun agent-harbor-dispatch-url (uri)
  "Dispatch URI for `emacsclient -e' callers."
  (agent-harbor-uri-open uri))

(defun agent-harbor-uri-open-org-link (path _)
  "Open Org ah: PATH."
  (agent-harbor-uri-open (concat "agent-harbor://" path)))

(when (and (require 'ol nil t)
           (fboundp 'org-link-set-parameters))
  (org-link-set-parameters "ah" :follow #'agent-harbor-uri-open-org-link))

(provide 'agent-harbor-uri)

;;; agent-harbor-uri.el ends here
