;;; agent-harbor-models.el --- Agent Harbor REST model helpers -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Small, deterministic mirrors of the shared REST Task, Session, and Activity
;; shapes used by the editor MVP.  Later milestones can replace this file with
;; generated output without changing callers.

;;; Code:

(require 'cl-lib)

(cl-defstruct (agent-harbor-task
               (:constructor agent-harbor-task-create))
  id prompt loop-type loop-state iteration status workspace-path session-count
  created-at updated-at links phase executive-summary elapsed-ms)

(cl-defstruct (agent-harbor-session
               (:constructor agent-harbor-session-create))
  id tenant-id project-id task agent runtime workspace vcs sandbox llm-provider
  status started-at ended-at parent-session-ids continues-from links)

(cl-defstruct (agent-harbor-activity
               (:constructor agent-harbor-activity-create))
  id type timestamp session-id task-id summary payload)

(defun agent-harbor-models--alist-get (key alist)
  "Return KEY from ALIST, accepting string or symbol keys."
  (or (alist-get key alist nil nil #'equal)
      (alist-get (intern key) alist nil nil #'eq)))

(defun agent-harbor-models--vector-or-list-to-list (value)
  "Return VALUE as a list when VALUE is a vector or list."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   ((null value) nil)
   (t (list value))))

(defun agent-harbor-models-task-from-json (json)
  "Map a daemon task JSON alist into an `agent-harbor-task'."
  (agent-harbor-task-create
   :id (agent-harbor-models--alist-get "id" json)
   :prompt (agent-harbor-models--alist-get "prompt" json)
   :loop-type (agent-harbor-models--alist-get "loop_type" json)
   :loop-state (agent-harbor-models--alist-get "loop_state" json)
   :iteration (agent-harbor-models--alist-get "iteration" json)
   :status (agent-harbor-models--alist-get "status" json)
   :workspace-path (agent-harbor-models--alist-get "workspace_path" json)
   :session-count (agent-harbor-models--alist-get "session_count" json)
   :created-at (agent-harbor-models--alist-get "created_at" json)
   :updated-at (agent-harbor-models--alist-get "updated_at" json)
   :links (agent-harbor-models--alist-get "links" json)
   :phase (agent-harbor-models--alist-get "phase" json)
   :executive-summary (agent-harbor-models--alist-get "executive_summary" json)
   :elapsed-ms (agent-harbor-models--alist-get "elapsed_ms" json)))

(defun agent-harbor-models-session-from-json (json)
  "Map a daemon session JSON alist into an `agent-harbor-session'."
  (agent-harbor-session-create
   :id (agent-harbor-models--alist-get "id" json)
   :tenant-id (agent-harbor-models--alist-get "tenant_id" json)
   :project-id (agent-harbor-models--alist-get "project_id" json)
   :task (agent-harbor-models--alist-get "task" json)
   :agent (agent-harbor-models--alist-get "agent" json)
   :runtime (agent-harbor-models--alist-get "runtime" json)
   :workspace (agent-harbor-models--alist-get "workspace" json)
   :vcs (agent-harbor-models--alist-get "vcs" json)
   :sandbox (agent-harbor-models--alist-get "sandbox" json)
   :llm-provider (agent-harbor-models--alist-get "llmProvider" json)
   :status (agent-harbor-models--alist-get "status" json)
   :started-at (agent-harbor-models--alist-get "started_at" json)
   :ended-at (agent-harbor-models--alist-get "ended_at" json)
   :parent-session-ids
   (agent-harbor-models--vector-or-list-to-list
    (agent-harbor-models--alist-get "parentSessionIds" json))
   :continues-from (agent-harbor-models--alist-get "continuesFrom" json)
   :links (agent-harbor-models--alist-get "links" json)))

(defun agent-harbor-models-activity-from-json (json)
  "Map a daemon activity/event JSON alist into an `agent-harbor-activity'."
  (agent-harbor-activity-create
   :id (agent-harbor-models--alist-get "id" json)
   :type (agent-harbor-models--alist-get "type" json)
   :timestamp (agent-harbor-models--alist-get "timestamp" json)
   :session-id (agent-harbor-models--alist-get "session_id" json)
   :task-id (agent-harbor-models--alist-get "task_id" json)
   :summary (agent-harbor-models--alist-get "summary" json)
   :payload json))

(provide 'agent-harbor-models)

;;; agent-harbor-models.el ends here
