;;; tools/agent-harbor/config.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

(use-package! agent-harbor
  :commands (agent-harbor)
  :init
  (map! :leader
        :desc "Agent Harbor" "o h" #'agent-harbor))
