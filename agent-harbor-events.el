;;; agent-harbor-events.el --- Agent Harbor editor event helpers -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; A small fan-out sink: callers register callbacks under an event name and
;; emit by name, so the daemon, SSE and REST layers can notify the sidebar and
;; the timeline without any of them holding a reference to the others.

;;; Code:

(require 'cl-lib)

(cl-defstruct (agent-harbor-event-sink
               (:constructor agent-harbor-event-sink-create))
  callbacks)

(defun agent-harbor-events-make-sink ()
  "Create an event sink."
  (agent-harbor-event-sink-create :callbacks nil))

(defun agent-harbor-events-subscribe (sink callback)
  "Subscribe CALLBACK to SINK and return an unsubscribe function."
  (setf (agent-harbor-event-sink-callbacks sink)
        (cons callback (agent-harbor-event-sink-callbacks sink)))
  (lambda ()
    (setf (agent-harbor-event-sink-callbacks sink)
          (delq callback (agent-harbor-event-sink-callbacks sink)))))

(defun agent-harbor-events-emit (sink event)
  "Emit EVENT to all callbacks in SINK."
  (dolist (callback (agent-harbor-event-sink-callbacks sink))
    (funcall callback event)))

(provide 'agent-harbor-events)

;;; agent-harbor-events.el ends here
