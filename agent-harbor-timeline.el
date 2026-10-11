;;; agent-harbor-timeline.el --- Agent Harbor timeline rendering -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Read-only activity card rendering for the Emacs side window.  The renderer
;; uses magit-section when it is already available, and otherwise uses stable
;; text properties so batch and minimal installations see the same structure.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'agent-harbor-models)

(defvar agent-harbor-timeline--have-magit-section (require 'magit-section nil t))
(defvar-local agent-harbor-timeline-auto-follow t
  "Whether the Agent Harbor timeline follows appended cards.")
(defvar-local agent-harbor-timeline-fold-state (make-hash-table :test #'equal)
  "Expanded state by timeline card id.")

(defvar agent-harbor-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'agent-harbor-timeline-next-card)
    (define-key map (kbd "p") #'agent-harbor-timeline-previous-card)
    (define-key map (kbd "TAB") #'agent-harbor-timeline-toggle-card)
    (define-key map (kbd "f") #'agent-harbor-timeline-toggle-auto-follow)
    map)
  "Keymap for Agent Harbor timeline buffers.")

(define-derived-mode agent-harbor-timeline-mode special-mode "Agent Harbor Timeline"
  "Major mode for read-only Agent Harbor timeline buffers."
  (setq-local truncate-lines nil))

(defun agent-harbor-timeline--get (key alist)
  "Return KEY from ALIST, accepting string and symbol keys."
  (agent-harbor-models--alist-get key alist))

(defun agent-harbor-timeline--bytes-to-string (value)
  "Decode daemon byte-array VALUE when possible."
  (cond
   ((and (listp value)
         value
         (cl-every (lambda (item)
                     (and (integerp item) (<= 0 item) (<= item 255)))
                   value))
    (decode-coding-string (apply #'unibyte-string value) 'utf-8 t))
   ((stringp value) value)
   (t value)))

(defun agent-harbor-timeline--text-field (key event)
  "Return decoded text field KEY from EVENT."
  (let ((value (agent-harbor-timeline--get key event)))
    (cond
     ((null value) nil)
     ((stringp value) value)
     ((listp value)
      (let ((decoded (agent-harbor-timeline--bytes-to-string value)))
        (if (stringp decoded) decoded (json-encode value))))
     (t (format "%s" value)))))

(defun agent-harbor-timeline-unwrap-sub-agent (event)
  "Return the inner SessionEvent wrapped inside a sub_agent envelope EVENT.

Sub-agent envelopes are fan-out wrappers the daemon places into a parent
session's history when a child session emits an event.  The wrapped inner
event may arrive as either a decoded JSON object or a JSON byte vector in
the `innerEvent' field.  This helper returns decoded objects directly, or
round-trips serialized payloads through `agent-harbor-timeline--bytes-to-string`
and `json-read-from-string' to recover the original event, returning nil if
the field is missing or malformed."
  (let* ((raw (or (agent-harbor-timeline--get "innerEvent" event)
                  (agent-harbor-timeline--get "inner_event" event)))
         (decoded (cond
                   ((null raw) nil)
                   ((stringp raw) raw)
                   ((listp raw) (agent-harbor-timeline--bytes-to-string raw))
                   (t nil))))
    (cond
     ((and (listp decoded) (not (stringp decoded))) decoded)
     ((and (stringp decoded) (not (string-empty-p decoded)))
      (condition-case nil
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-key-type 'symbol))
            (json-read-from-string decoded))
        (error nil)))
     (t nil))))

(defun agent-harbor-timeline-effective-event (event)
  "Return the effective event for rendering EVENT.

For sub_agent envelopes (fan-out from a child session into the parent
task's timeline), this returns the innermost SessionEvent so the rest of the
rendering pipeline acts on the actual agent output (\"Review approved\",
\"Supervisor marked …\", file_edit, tool_use, …).  For non-sub_agent
events it returns EVENT unchanged."
  (let ((current event)
        (remaining 16))
    (while (and (> remaining 0)
                (equal (agent-harbor-timeline--text-field "type" current)
                       "sub_agent"))
      (let ((inner (agent-harbor-timeline-unwrap-sub-agent current)))
        (if inner
            (setq current inner
                  remaining (1- remaining))
          (setq remaining 0))))
    current))

(defun agent-harbor-timeline--event-type (event)
  "Return EVENT type as a string."
  (or (agent-harbor-timeline--text-field "type"
                                          (agent-harbor-timeline-effective-event event))
      "event"))

(defun agent-harbor-timeline-card-type (event)
  "Return the display card type for daemon EVENT."
  (pcase (agent-harbor-timeline--event-type event)
    ("file_edit" "edited")
    ("file_read" "read")
    ("user_input" "user")
    ("turn_complete" "completed")
    (type type)))

(defun agent-harbor-timeline--event-file-path (event)
  "Return a daemon file path from EVENT when present."
  (let ((effective (agent-harbor-timeline-effective-event event)))
    (or (agent-harbor-timeline--text-field "file_path" effective)
        (agent-harbor-timeline--text-field "path" effective))))

(defun agent-harbor-timeline-event-timestamp (event)
  "Return EVENT timestamp or zero."
  (let ((timestamp (agent-harbor-timeline--get "timestamp" event)))
    (cond
     ((numberp timestamp) timestamp)
     ((stringp timestamp) (string-to-number timestamp))
     (t 0))))

(defun agent-harbor-timeline-card-id (session-id event index)
  "Return deterministic card id for SESSION-ID EVENT at INDEX."
  (format "%s:%s:%s:%s"
          session-id
          (agent-harbor-timeline-event-timestamp event)
          (agent-harbor-timeline--event-type event)
          index))

(defun agent-harbor-timeline-event-text (event)
  "Return readable timeline text for daemon EVENT.

Sub-agent envelopes are unwrapped via
`agent-harbor-timeline-effective-event' so the rendered card text reflects
the inner agent output (\"Review approved\", \"Supervisor marked …\",
…) rather than an opaque \"sub_agent\" placeholder.  This is how the
editor side panel surfaces content from manager → reviewer → supervisor
sequential sub-agents — each child session id is not directly attached
to the parent task in the REST `/api/v1/tasks/{id}/sessions' listing."
  (let* ((effective (agent-harbor-timeline-effective-event event))
         (type (or (agent-harbor-timeline--text-field "type" effective) "event")))
    (string-trim
     (cond
      ((equal type "status")
       (format "Status: %s" (or (agent-harbor-timeline--text-field "status" effective) "unknown")))
      ((equal type "thought")
       (or (agent-harbor-timeline--text-field "thought" effective)
           (agent-harbor-timeline--text-field "reasoning" effective)
           "Agent reasoning"))
      ((equal type "tool_use")
       (string-join
        (delq nil (list "Tool started:"
                        (agent-harbor-timeline--text-field "tool_name" effective)
                        (agent-harbor-timeline--text-field "tool_args" effective)))
        " "))
      ((equal type "tool_result")
       (string-join
        (delq nil (list "Tool finished:"
                        (agent-harbor-timeline--text-field "tool_name" effective)
                        (agent-harbor-timeline--text-field "tool_output" effective)))
        " "))
      ((equal type "file_edit")
       (string-join
        (delq nil (list "Edited"
                        (agent-harbor-timeline--text-field "file_path" effective)
                        (agent-harbor-timeline--text-field "description" effective)))
        " "))
      ((equal type "file_read")
       (format "Read %s" (or (agent-harbor-timeline--text-field "file_path" effective) "file")))
      ((equal type "user_input")
       (or (agent-harbor-timeline--text-field "content" effective) "User input"))
      ((equal type "log")
       (or (agent-harbor-timeline--text-field "message" effective) "Log entry"))
      ((equal type "error")
       (format "Error: %s" (or (agent-harbor-timeline--text-field "message" effective) "unknown")))
      ((equal type "orchestration")
       (or (agent-harbor-timeline--text-field "description" effective)
           (agent-harbor-timeline--text-field "kind" effective)
           "Orchestration"))
      ((equal type "delegation")
       (or (agent-harbor-timeline--text-field "description" effective)
           (agent-harbor-timeline--text-field "kind" effective)
           "Delegation"))
      ((equal type "turn_complete")
       (format "Turn complete: %s"
               (or (agent-harbor-timeline--text-field "stop_reason" effective) "complete")))
      (t (json-encode effective))))))

(defun agent-harbor-timeline-cards (session-events)
  "Return timeline cards from SESSION-EVENTS.
SESSION-EVENTS is a list of (SESSION-ID . EVENTS) pairs."
  (let (cards)
    (dolist (entry session-events)
      (let ((session-id (car entry))
            (index 0))
        (dolist (event (cdr entry))
          (setq index (1+ index))
          (push `((id . ,(agent-harbor-timeline-card-id session-id event index))
                  (sessionId . ,session-id)
                  (type . ,(agent-harbor-timeline-card-type event))
                  (eventType . ,(agent-harbor-timeline--event-type event))
                  (timestamp . ,(agent-harbor-timeline-event-timestamp event))
                  (filePath . ,(agent-harbor-timeline--event-file-path event))
                  (text . ,(agent-harbor-timeline-event-text event)))
                cards))))
    (sort (nreverse cards)
          (lambda (left right)
            (let ((left-time (agent-harbor-timeline--get "timestamp" left))
                  (right-time (agent-harbor-timeline--get "timestamp" right)))
              (if (= left-time right-time)
                  (string< (agent-harbor-timeline--get "id" left)
                           (agent-harbor-timeline--get "id" right))
                (< left-time right-time)))))))

(defun agent-harbor-timeline--insert-fallback-card (card expanded)
  "Insert CARD using deterministic fallback rendering.
EXPANDED non-nil shows the full text payload."
  (let* ((id (agent-harbor-timeline--get "id" card))
         (type (agent-harbor-timeline--get "type" card))
         (session-id (agent-harbor-timeline--get "sessionId" card))
         (text (agent-harbor-timeline--get "text" card))
         (start (point)))
    (insert (format "%s %s  %s\n"
                    (if expanded "v" ">")
                    type
                    session-id))
    (when expanded
      (insert (format "  %s\n" text)))
    (add-text-properties
     start (point)
     `(agent-harbor-card-id ,id
       agent-harbor-card t
       agent-harbor-card-data ,card
       ,@(when (agent-harbor-timeline--get "previewUri" card)
           '(mouse-face highlight))
       read-only t))))

(defun agent-harbor-timeline-insert (cards)
  "Insert CARDS into the current buffer."
  (insert "Activity Timeline\n")
  (if (null cards)
      (insert "  No activity yet.\n")
    (dolist (card cards)
      (let* ((id (agent-harbor-timeline--get "id" card))
             (expanded (not (eq (gethash id agent-harbor-timeline-fold-state) 'collapsed))))
        (if agent-harbor-timeline--have-magit-section
            ;; Keep the Magit path intentionally shallow; the fallback text
            ;; remains the canonical shape used by tests and non-Magit installs.
            (agent-harbor-timeline--insert-fallback-card card expanded)
          (agent-harbor-timeline--insert-fallback-card card expanded)))))
  (when agent-harbor-timeline-auto-follow
    (goto-char (point-max))))

(defun agent-harbor-timeline--goto-card (direction)
  "Move to the next timeline card in DIRECTION."
  (let ((prop 'agent-harbor-card))
    (if (> direction 0)
        (goto-char (or (next-single-property-change (point) prop nil (point-max))
                       (point-max)))
      (goto-char (or (previous-single-property-change (point) prop nil (point-min))
                     (point-min))))
    (when (and (> direction 0) (not (get-text-property (point) prop)))
      (goto-char (or (next-single-property-change (point) prop nil (point-max)) (point))))
    (beginning-of-line)))

(defun agent-harbor-timeline-next-card ()
  "Move point to the next timeline card."
  (interactive)
  (agent-harbor-timeline--goto-card 1))

(defun agent-harbor-timeline-previous-card ()
  "Move point to the previous timeline card."
  (interactive)
  (agent-harbor-timeline--goto-card -1))

(defun agent-harbor-timeline-toggle-card ()
  "Toggle the timeline card at point."
  (interactive)
  (let ((id (get-text-property (point) 'agent-harbor-card-id)))
    (when id
      (puthash id
               (if (eq (gethash id agent-harbor-timeline-fold-state) 'collapsed)
                   'expanded
                 'collapsed)
               agent-harbor-timeline-fold-state)
      (when (fboundp 'agent-harbor-sidebar-render)
        (agent-harbor-sidebar-render)))))

(defun agent-harbor-timeline-toggle-auto-follow ()
  "Toggle timeline auto-follow."
  (interactive)
  (setq agent-harbor-timeline-auto-follow (not agent-harbor-timeline-auto-follow))
  (message "Agent Harbor auto-follow %s"
           (if agent-harbor-timeline-auto-follow "enabled" "disabled")))

(provide 'agent-harbor-timeline)

;;; agent-harbor-timeline.el ends here
