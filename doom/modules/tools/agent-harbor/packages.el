;;; tools/agent-harbor/packages.el -*- no-byte-compile: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;; The explicit recipe names the published mirror,
;; agent-harbor/agent-harbor.el, which is where the package is installable from
;; today. It can be reduced to a bare `(package! agent-harbor)' once the MELPA
;; submission in packaging/melpa/ is merged, at which point Doom resolves the
;; name on its own.
(package! agent-harbor
  :recipe (:host github :repo "agent-harbor/agent-harbor.el"))
