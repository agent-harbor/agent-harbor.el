;;; packages.el --- Agent Harbor layer packages file for Spacemacs. -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

(defconst agent-harbor-packages
  ;; The recipe names the published mirror, agent-harbor/agent-harbor.el, which
  ;; is where the package is installable from today.  It can be reduced to a
  ;; bare `agent-harbor' once the MELPA submission in packaging/melpa/ is
  ;; merged, at which point Spacemacs resolves the name through ELPA.
  '((agent-harbor :location (recipe :fetcher github
                                    :repo "agent-harbor/agent-harbor.el")))
  "The list of Lisp packages required by the Agent Harbor layer.")

(defun agent-harbor/init-agent-harbor ()
  "Initialize the Agent Harbor package."
  (use-package agent-harbor
    :defer t
    :commands (agent-harbor)
    :init
    (spacemacs/set-leader-keys "o h" #'agent-harbor)))
