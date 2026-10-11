;;; agent-harbor-faces.el --- Agent Harbor faces -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Every face the Agent Harbor buffers use, defined in one place and inheriting
;; from the standard faces (`success', `warning', `error', `shadow') so that a
;; user's theme governs the colours rather than this package.

;;; Code:

(defface agent-harbor-header-connected
  '((t :inherit success :weight bold))
  "Face for a connected Agent Harbor daemon indicator."
  :group 'agent-harbor)

(defface agent-harbor-header-starting
  '((t :inherit warning :weight bold))
  "Face for a starting Agent Harbor daemon indicator."
  :group 'agent-harbor)

(defface agent-harbor-header-disconnected
  '((t :inherit error :weight bold))
  "Face for a disconnected Agent Harbor daemon indicator."
  :group 'agent-harbor)

(defface agent-harbor-feedback-banner
  '((t :inherit warning :weight bold))
  "Face for pending Agent Harbor feedback banner text."
  :group 'agent-harbor)

(defface agent-harbor-feedback-task-row
  '((t :inherit warning :weight bold :box t))
  "Face for task rows that have pending Agent Harbor feedback."
  :group 'agent-harbor)

(defface agent-harbor-feedback-action
  '((t :inherit link :weight bold))
  "Face for Agent Harbor feedback quick-action text."
  :group 'agent-harbor)

(provide 'agent-harbor-faces)

;;; agent-harbor-faces.el ends here
