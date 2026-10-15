;;; agent-harbor.el --- Agent Harbor editor integration -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;; Version: 0.6.11
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/agent-harbor/agent-harbor
;; Keywords: tools, ai, convenience
;;; Commentary:

;; Emacs package scaffold for Agent Harbor.  M9 provides daemon discovery, REST
;; plumbing, SSE plumbing, model mirrors, and a placeholder Agent Harbor buffer.

;;; Code:

(require 'agent-harbor-daemon)
(require 'agent-harbor-branchfs)
(require 'agent-harbor-events)
(require 'agent-harbor-faces)
(require 'agent-harbor-models)
(require 'agent-harbor-mux)
(require 'agent-harbor-reply)
(require 'agent-harbor-rest)
(require 'agent-harbor-scm)
(require 'agent-harbor-sidebar)
(require 'agent-harbor-sse)
(require 'agent-harbor-task-entry)
(require 'agent-harbor-task-prompt)
(require 'agent-harbor-workspace)
(require 'agent-harbor-uri)
(require 'agent-harbor-version)
(require 'agent-harbor-voice)
(require 'browse-url)

(defgroup agent-harbor nil
  "Agent Harbor editor integration."
  :link '(url-link :tag "Documentation" "https://docs.agent-harbor.com/editor-support/emacs")
  :group 'tools)

(defcustom agent-harbor-buffer-name "*Agent Harbor*"
  "Name of the Agent Harbor buffer."
  :type 'string
  :group 'agent-harbor)

(defcustom agent-harbor-documentation-url
  "https://docs.agent-harbor.com/editor-support/emacs"
  "Documentation URL opened by `agent-harbor-open-documentation'."
  :type 'string
  :group 'agent-harbor)

(defvar agent-harbor-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'agent-harbor-task-entry-open)
    (define-key map (kbd "O") #'agent-harbor-open-in-new-instance)
    (define-key map (kbd "?") #'agent-harbor-open-documentation)
    map)
  "Prefix keymap for Agent Harbor commands.")

(define-key global-map (kbd "C-c h") agent-harbor-command-map)

(defun agent-harbor-header-line ()
  "Return the Agent Harbor header-line indicator."
  (let ((suffix (if (agent-harbor-workspace-sandboxed-p) " sandboxed" "")))
    (pcase agent-harbor-daemon-state
      ('connected
       (propertize (concat "AH ● connected" suffix)
                   'face 'agent-harbor-header-connected))
      ('starting
       (propertize (concat "AH ● starting" suffix)
                   'face 'agent-harbor-header-starting))
      (_
       (propertize (concat "AH ● disconnected" suffix)
                   'face 'agent-harbor-header-disconnected)))))

(defun agent-harbor--install-header-line ()
  "Install the Agent Harbor header line in the current buffer."
  (setq-local header-line-format '((:eval (agent-harbor-header-line)))))

;;;###autoload
(defun agent-harbor ()
  "Open the Agent Harbor side window."
  (interactive)
  (agent-harbor-sidebar-open))

;;;###autoload
(defun agent-harbor-open-documentation ()
  "Open the Agent Harbor Emacs documentation."
  (interactive)
  (browse-url agent-harbor-documentation-url))

(provide 'agent-harbor)

;;; agent-harbor.el ends here
