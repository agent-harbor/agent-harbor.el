;;; agent-harbor-reply.el --- Agent Harbor feedback reply UI -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Reply input for pending Agent Harbor feedback requests.  The side window
;; uses an overlay-backed bottom input affordance, while longer answers can be
;; composed in a dedicated *Agent Harbor Reply* buffer.  All submissions go
;; through the daemon's POST /api/v1/tasks/<id>/feedback/<fid>/respond endpoint.

;;; Code:

(require 'subr-x)
(require 'agent-harbor-models)
(require 'agent-harbor-rest)
(require 'agent-harbor-faces)

(declare-function agent-harbor-sidebar-refresh "agent-harbor-sidebar")
(declare-function agent-harbor-sidebar-render "agent-harbor-sidebar")
(declare-function agent-harbor-sidebar--selected-task-id "agent-harbor-sidebar")
(declare-function agent-harbor-sidebar--pending-feedback-for-task "agent-harbor-sidebar")

(defgroup agent-harbor-reply nil
  "Agent Harbor feedback reply UI."
  :group 'agent-harbor)

(defcustom agent-harbor-reply-buffer-name "*Agent Harbor Reply*"
  "Name of the multi-line Agent Harbor reply buffer."
  :type 'string
  :group 'agent-harbor-reply)

(defvar-local agent-harbor-reply--overlay nil)
(defvar-local agent-harbor-reply--task-id nil)
(defvar-local agent-harbor-reply--feedback nil)
(defvar-local agent-harbor-reply--last-error nil)

(defvar agent-harbor-reply-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-harbor-reply-popup-submit)
    (define-key map (kbd "C-c C-k") #'agent-harbor-reply-popup-cancel)
    map)
  "Keymap for `agent-harbor-reply-buffer-mode'.")

(define-derived-mode agent-harbor-reply-buffer-mode text-mode "Agent Harbor Reply"
  "Major mode for composing a longer Agent Harbor feedback response.")

(defun agent-harbor-reply--get (key alist)
  "Return KEY from ALIST, accepting string and symbol keys."
  (agent-harbor-models--alist-get key alist))

(defun agent-harbor-reply-feedback-id (feedback)
  "Return FEEDBACK id."
  (agent-harbor-reply--get "id" feedback))

(defun agent-harbor-reply-feedback-type (feedback)
  "Return FEEDBACK type."
  (or (agent-harbor-reply--get "type" feedback) "question"))

(defun agent-harbor-reply-feedback-question (feedback)
  "Return FEEDBACK question."
  (or (agent-harbor-reply--get "question" feedback) "Feedback required"))

(defun agent-harbor-reply--trim (text)
  "Return trimmed TEXT."
  (string-trim (or text "")))

(defun agent-harbor-reply-build-payload (feedback text)
  "Build the response payload for FEEDBACK from TEXT.
Return nil when the feedback type cannot be submitted from a free-text reply."
  (let ((trimmed (agent-harbor-reply--trim text)))
    (pcase (agent-harbor-reply-feedback-type feedback)
      ("question"
       (unless (string-empty-p trimmed)
         `((response . ((text . ,trimmed))))))
      ("confirmation"
       (unless (string-empty-p trimmed)
         `((response . ((confirmed . t)))
           (comment . ,trimmed))))
      ("information"
       `((response . ((acknowledged . t)))
         ,@(unless (string-empty-p trimmed) `((comment . ,trimmed)))))
      ("custom_form"
       (unless (string-empty-p trimmed)
         `((response . ((text . ,trimmed))))))
      (_ nil))))

(defun agent-harbor-reply-quick-payload (feedback approved &optional comment)
  "Build quick-action payload for FEEDBACK with APPROVED and optional COMMENT."
  (unless (string-equal (agent-harbor-reply-feedback-type feedback) "confirmation")
    (signal 'user-error '("Quick approve/reject is available only for confirmation feedback")))
  `((response . ((confirmed . ,(if approved t :false))))
    ,@(let ((trimmed (agent-harbor-reply--trim comment)))
        (unless (string-empty-p trimmed) `((comment . ,trimmed))))))

(defun agent-harbor-reply--current-target ()
  "Return (TASK-ID . FEEDBACK) for the sidebar selection."
  (let* ((task-id (and (fboundp 'agent-harbor-sidebar--selected-task-id)
                       (agent-harbor-sidebar--selected-task-id)))
         (feedback (and task-id
                        (fboundp 'agent-harbor-sidebar--pending-feedback-for-task)
                        (agent-harbor-sidebar--pending-feedback-for-task task-id))))
    (unless (and task-id feedback)
      (signal 'user-error '("No pending Agent Harbor feedback for the selected task")))
    (cons task-id feedback)))

(defun agent-harbor-reply-submit-payload (task-id feedback payload)
  "Submit PAYLOAD for TASK-ID FEEDBACK and refresh the sidebar."
  (let ((response (agent-harbor-rest-respond-feedback
                   task-id
                   (agent-harbor-reply-feedback-id feedback)
                   payload)))
    (when (fboundp 'agent-harbor-sidebar-refresh)
      (agent-harbor-sidebar-refresh))
    response))

(defun agent-harbor-reply-submit-text (text &optional task-id feedback)
  "Submit TEXT for TASK-ID and FEEDBACK, defaulting to the selected feedback."
  (let* ((target (unless (and task-id feedback) (agent-harbor-reply--current-target)))
         (task-id (or task-id (car target)))
         (feedback (or feedback (cdr target)))
         (payload (agent-harbor-reply-build-payload feedback text)))
    (unless payload
      (signal 'user-error '("Feedback response is required or unsupported")))
    (agent-harbor-reply-submit-payload task-id feedback payload)))

(defun agent-harbor-reply-quick-approve (&optional comment)
  "Approve the selected confirmation feedback with optional COMMENT."
  (interactive)
  (let* ((target (agent-harbor-reply--current-target))
         (payload (agent-harbor-reply-quick-payload (cdr target) t comment)))
    (agent-harbor-reply-submit-payload (car target) (cdr target) payload)))

(defun agent-harbor-reply-quick-reject (&optional comment)
  "Reject the selected confirmation feedback with optional COMMENT."
  (interactive)
  (let* ((target (agent-harbor-reply--current-target))
         (payload (agent-harbor-reply-quick-payload (cdr target) nil comment)))
    (agent-harbor-reply-submit-payload (car target) (cdr target) payload)))

(defun agent-harbor-reply-read-and-submit ()
  "Read a short reply in the minibuffer and submit it."
  (interactive)
  (let* ((target (agent-harbor-reply--current-target))
         (feedback (cdr target))
         (text (read-string (format "Reply to %s: "
                                    (agent-harbor-reply-feedback-id feedback)))))
    (agent-harbor-reply-submit-text text (car target) feedback)))

(defun agent-harbor-reply-open-popup ()
  "Open the multi-line Agent Harbor reply buffer for the selected feedback."
  (interactive)
  (let* ((target (agent-harbor-reply--current-target))
         (buffer (get-buffer-create agent-harbor-reply-buffer-name)))
    (with-current-buffer buffer
      (agent-harbor-reply-buffer-mode)
      (setq agent-harbor-reply--task-id (car target)
            agent-harbor-reply--feedback (cdr target)
            agent-harbor-reply--last-error nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n")))
    (pop-to-buffer buffer)
    buffer))

(defun agent-harbor-reply-popup-submit ()
  "Submit the contents of the multi-line reply buffer."
  (interactive)
  (unless (and agent-harbor-reply--task-id agent-harbor-reply--feedback)
    (signal 'user-error '("Agent Harbor reply buffer is not bound to feedback")))
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (condition-case err
        (let ((response (agent-harbor-reply-submit-text
                         text agent-harbor-reply--task-id agent-harbor-reply--feedback)))
          (kill-buffer (current-buffer))
          response)
      (error
       (setq agent-harbor-reply--last-error (error-message-string err))
       (signal (car err) (cdr err))))))

(defun agent-harbor-reply-popup-cancel ()
  "Cancel the multi-line reply buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun agent-harbor-reply--overlay-text (task-id feedback)
  "Return bottom overlay text for TASK-ID and FEEDBACK."
  (let* ((feedback-id (agent-harbor-reply-feedback-id feedback))
         (type (agent-harbor-reply-feedback-type feedback))
         (question (agent-harbor-reply-feedback-question feedback))
         (quick (if (string-equal type "confirmation")
                    "  [Approve] [Reject]"
                  "")))
    (concat
     "\n"
     (propertize
      (format "Feedback required for %s (%s)\n%s\nReply: " task-id feedback-id question)
      'face 'agent-harbor-feedback-banner
      'agent-harbor-feedback-id feedback-id
      'agent-harbor-task-id task-id)
     (propertize quick
                 'face 'agent-harbor-feedback-action
                 'agent-harbor-feedback-id feedback-id
                 'agent-harbor-task-id task-id)
     "\n")))

(defun agent-harbor-reply-clear-overlay ()
  "Remove the current reply overlay."
  (when (overlayp agent-harbor-reply--overlay)
    (delete-overlay agent-harbor-reply--overlay)
    (setq agent-harbor-reply--overlay nil)))

(defun agent-harbor-reply-render-overlay (task-id feedback)
  "Render the bottom reply overlay for TASK-ID and FEEDBACK.
When FEEDBACK is nil, remove any existing overlay."
  (agent-harbor-reply-clear-overlay)
  (when feedback
    (setq agent-harbor-reply--overlay (make-overlay (point-max) (point-max) nil t t))
    (overlay-put agent-harbor-reply--overlay 'after-string
                 (agent-harbor-reply--overlay-text task-id feedback))
    (overlay-put agent-harbor-reply--overlay 'agent-harbor-reply-overlay t)
    (overlay-put agent-harbor-reply--overlay 'agent-harbor-task-id task-id)
    (overlay-put agent-harbor-reply--overlay 'agent-harbor-feedback-id
                 (agent-harbor-reply-feedback-id feedback))))

(defun agent-harbor-reply-test-state ()
  "Return structured reply overlay and popup state for page-object tests."
  (let* ((overlay-visible (and (overlayp agent-harbor-reply--overlay)
                               (overlay-buffer agent-harbor-reply--overlay)))
         (reply-buffer (get-buffer agent-harbor-reply-buffer-name))
         (popup-visible (and (buffer-live-p reply-buffer)
                             (get-buffer-window reply-buffer t))))
    `((visible . ,(if overlay-visible t :false))
      (taskId . ,(and overlay-visible
                      (overlay-get agent-harbor-reply--overlay
                                   'agent-harbor-task-id)))
      (feedbackId . ,(and overlay-visible
                          (overlay-get agent-harbor-reply--overlay
                                       'agent-harbor-feedback-id)))
      (text . ,(if (buffer-live-p reply-buffer)
                   (with-current-buffer reply-buffer
                     (buffer-substring-no-properties (point-min) (point-max)))
                 ""))
      (voiceEnabled . ,(if (not (eq (and (boundp 'agent-harbor-voice-mode)
                                         agent-harbor-voice-mode)
                                    'disabled))
                           t
                         :false))
      (popupVisible . ,(if popup-visible t :false))
      (lastError . ,(and (buffer-live-p reply-buffer)
                         (with-current-buffer reply-buffer
                           agent-harbor-reply--last-error)))
      (lastResponse . nil))))

(provide 'agent-harbor-reply)

;;; agent-harbor-reply.el ends here
