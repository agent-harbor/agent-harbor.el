;;; agent-harbor-version.el --- Version identity and self-diagnosis -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Which version of the package this is, which `ah' releases it works with,
;; and `agent-harbor-doctor' to report on the difference.
;;
;; The package and the `ah' binary are released from one monorepo tag and
;; carry the same version number, but a user installs them separately -- the
;; package from MELPA or straight.el, `ah' from Homebrew, a release tarball or
;; Nix -- so the two drift.  The failure that drift produces is silent: `/'
;; and `@' completion in a task prompt returns nothing, because
;; `ah autocomplete --json' is missing or too old.  Declaring the floor here,
;; and reporting it in `agent-harbor-doctor', is what makes that
;; diagnosable.

;;; Code:

(require 'subr-x)

;; Required lazily in `agent-harbor-doctor': loading the daemon at top level
;; would drag in the REST and SSE stack before the doctor can report anything.
(declare-function agent-harbor-daemon-status "agent-harbor-daemon" ())

(defconst agent-harbor-version "0.6.8"
  "Version of this Agent Harbor package.

A placeholder in the monorepo, kept in step with the `Version:\' header of
agent-harbor.el, and rewritten in both places when the package is published
to the agent-harbor/agent-harbor.el mirror -- which is where a released
version number exists at all.  A monorepo checkout reports `0.0.0-dev',
and `agent-harbor-doctor' says so rather than claiming a release it is
not.")

(defconst agent-harbor-ah-minimum-version "0.6.8"
  "Oldest `ah' release this package can be used with.

Not generated.  It is the oldest release providing every CLI surface the
package calls, and it moves by hand when the package starts depending on
something newer.  Raising it breaks users on an older `ah', so it carries
the reason:

  0.6.8  `ah autocomplete --json', which backs `/' and `@' completion in
         `agent-harbor-task-prompt-mode'.")

(defun agent-harbor-version-development-p ()
  "Return non-nil when this is not a published release.

True for a monorepo checkout (`0.0.0-dev\') and for a dev-channel publish
(`0.6.8snapshot\'), which uses a suffix `version-to-list\' accepts because
package.el calls it on the `Version:\' header."
  (or (string-suffix-p "-dev" agent-harbor-version)
      (string-match-p "snapshot" agent-harbor-version)))

(defun agent-harbor-version-parse (text)
  "Parse MAJOR.MINOR.PATCH out of TEXT as a list of three integers.
Any prerelease or build suffix is ignored.  Return nil when TEXT holds no
numeric triple, which callers must treat as \"unknown\", never as \"too
old\"."
  (when (and (stringp text)
             (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" text))
    (list (string-to-number (match-string 1 text))
          (string-to-number (match-string 2 text))
          (string-to-number (match-string 3 text)))))

(defun agent-harbor-version-compare (a b)
  "Compare parsed versions A and B.  Return -1, 0 or 1."
  (catch 'result
    (dotimes (index 3)
      (let ((left (nth index a))
            (right (nth index b)))
        (unless (= left right)
          (throw 'result (if (< left right) -1 1)))))
    0))

(defun agent-harbor-version-ah-satisfied-p (reported)
  "Does REPORTED satisfy `agent-harbor-ah-minimum-version'?
Return t, nil, or the symbol `unknown' when REPORTED carries no version
number to compare."
  (let ((got (agent-harbor-version-parse reported))
        (want (agent-harbor-version-parse agent-harbor-ah-minimum-version)))
    (cond
     ((not (and got want)) 'unknown)
     ((>= (agent-harbor-version-compare got want) 0) t)
     (t nil))))

(defun agent-harbor-version--ah-reported (executable)
  "Run EXECUTABLE --version and return its first line.
Return a cons of nil and a reason string on failure."
  (let ((resolved (executable-find executable)))
    (if (not resolved)
        (cons nil (format "%S is not on `exec-path'" executable))
      (with-temp-buffer
        (let ((code (call-process resolved nil t nil "--version")))
          (if (zerop code)
              (cons (car (split-string (string-trim (buffer-string)) "\n")) nil)
            (cons nil (format "%s --version exited with %s: %s"
                              resolved code
                              (string-trim (buffer-string))))))))))

;;;###autoload
(defun agent-harbor-doctor ()
  "Report on this package, the `ah' binary, and the daemon.

The Emacs counterpart of `:checkhealth agent-harbor' in the Neovim plugin.
Checks the Emacs version against the floor, this package's version, whether
`ah' is on `exec-path' and new enough, whether `ah autocomplete' exists,
and the daemon connection state."
  (interactive)
  (let ((executable (if (boundp 'agent-harbor-daemon-executable)
                        (symbol-value 'agent-harbor-daemon-executable)
                      "ah"))
        (lines '()))
    (push (if (version< emacs-version "29.1")
              (format "FAIL  Emacs %s is older than the required 29.1" emacs-version)
            (format "ok    Emacs %s" emacs-version))
          lines)
    (push (if (agent-harbor-version-development-p)
              (format "warn  package %s (unreleased monorepo checkout)" agent-harbor-version)
            (format "ok    package %s" agent-harbor-version))
          lines)
    (pcase-let ((`(,reported . ,reason) (agent-harbor-version--ah-reported executable)))
      (if (not reported)
          (progn
            (push (format "FAIL  ah not usable: %s" reason) lines)
            (push (concat "      install it from https://docs.agent-harbor.com/installation,"
                          " or set `agent-harbor-daemon-executable'")
                  lines))
        (push (format "ok    ah at %s -- %s" (executable-find executable) reported) lines)
        (pcase (agent-harbor-version-ah-satisfied-p reported)
          ('unknown
           (push (format "warn  no version number in %S; needs ah %s or newer"
                         reported agent-harbor-ah-minimum-version)
                 lines))
          ('nil
           (push (format "FAIL  %s is older than the required ah %s; `/' and `@' completion will return nothing"
                         reported agent-harbor-ah-minimum-version)
                 lines))
          (_
           (push (format "ok    satisfies the ah >= %s requirement"
                         agent-harbor-ah-minimum-version)
                 lines)))
        (push (if (zerop (call-process (executable-find executable) nil nil nil
                                       "autocomplete" "--help"))
                  "ok    `ah autocomplete' is available"
                "FAIL  `ah autocomplete' is missing; task-prompt completion depends on it")
              lines)))
    ;; A disconnected daemon is a warning, not a failure: the package starts
    ;; it on demand, so that is the normal state before the first task.
    (push (if (require 'agent-harbor-daemon nil t)
              (let ((status (agent-harbor-daemon-status)))
                (if (string-prefix-p "Agent Harbor daemon connected" status)
                    (format "ok    %s" status)
                  (format "warn  %s" status)))
            "warn  agent-harbor-daemon is not loadable")
          lines)
    (let ((report (string-join (nreverse lines) "\n")))
      (with-current-buffer (get-buffer-create "*agent-harbor-doctor*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert report "\n")
          (goto-char (point-min)))
        (special-mode)
        (display-buffer (current-buffer)))
      report)))

(provide 'agent-harbor-version)

;;; agent-harbor-version.el ends here
