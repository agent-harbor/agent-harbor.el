;;; agent-harbor-sse.el --- Agent Harbor curl SSE client -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; A small curl subprocess EventSource for future editor milestones.  It parses
;; server-sent event frames, reconnects with capped backoff, and cleans up with
;; its process buffer.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(define-error 'agent-harbor-sse-error "Agent Harbor SSE error")

(cl-defstruct (agent-harbor-sse-client
               (:constructor agent-harbor-sse-client-create))
  url callback error-callback process buffer last-event-id retry-delay
  reconnect-timer closed-p partial event-lines)

(defcustom agent-harbor-sse-initial-retry-delay 0.25
  "Initial SSE reconnect backoff in seconds."
  :type 'number
  :group 'agent-harbor)

(defcustom agent-harbor-sse-max-retry-delay 5.0
  "Maximum SSE reconnect backoff in seconds."
  :type 'number
  :group 'agent-harbor)

(defun agent-harbor-sse--decode-event (client lines)
  "Decode SSE LINES for CLIENT."
  (let ((event "message")
        (data-lines nil)
        (id nil)
        (retry nil))
    (dolist (line lines)
      (cond
       ((string-prefix-p ":" line) nil)
       ((string-prefix-p "event:" line)
        (setq event (string-trim-left (substring line 6))))
       ((string-prefix-p "data:" line)
        (push (string-trim-left (substring line 5)) data-lines))
       ((string-prefix-p "id:" line)
        (setq id (string-trim-left (substring line 3))))
       ((string-prefix-p "retry:" line)
        (setq retry (/ (string-to-number (string-trim-left (substring line 6))) 1000.0)))))
    (when id
      (setf (agent-harbor-sse-client-last-event-id client) id))
    (when retry
      (setf (agent-harbor-sse-client-retry-delay client) retry))
    (when data-lines
      (let* ((raw (string-join (nreverse data-lines) "\n"))
             (data (condition-case nil
                       (json-parse-string raw :object-type 'alist :array-type 'list
                                          :null-object nil :false-object :false)
                     (error raw))))
        `((event . ,event) (data . ,data) (id . ,id))))))

(defun agent-harbor-sse--dispatch-frame (client)
  "Dispatch the current SSE frame for CLIENT."
  (let ((frame (agent-harbor-sse--decode-event
                client (nreverse (agent-harbor-sse-client-event-lines client)))))
    (setf (agent-harbor-sse-client-event-lines client) nil)
    (when frame
      (funcall (agent-harbor-sse-client-callback client) frame))))

(defun agent-harbor-sse--handle-line (client line)
  "Handle one SSE LINE for CLIENT."
  (if (string-empty-p (string-trim line))
      (agent-harbor-sse--dispatch-frame client)
    (push (string-remove-suffix "\r" line)
          (agent-harbor-sse-client-event-lines client))))

(defun agent-harbor-sse--filter (client _process chunk)
  "Process SSE CHUNK for CLIENT."
  (let* ((text (concat (or (agent-harbor-sse-client-partial client) "") chunk))
         (lines (split-string text "\n"))
         (complete (string-suffix-p "\n" text)))
    (setf (agent-harbor-sse-client-partial client)
          (unless complete (car (last lines))))
    (dolist (line (if complete lines (butlast lines)))
      (agent-harbor-sse--handle-line client line))))

(defun agent-harbor-sse--schedule-reconnect (client)
  "Schedule reconnect for CLIENT."
  (unless (agent-harbor-sse-client-closed-p client)
    (let ((delay (agent-harbor-sse-client-retry-delay client)))
      (setf (agent-harbor-sse-client-retry-delay client)
            (min agent-harbor-sse-max-retry-delay (* 2 delay)))
      (setf (agent-harbor-sse-client-reconnect-timer client)
            (run-at-time delay nil #'agent-harbor-sse-start client)))))

(defun agent-harbor-sse--sentinel (client process event)
  "Handle PROCESS state EVENT for CLIENT."
  (unless (eq (process-status process) 'run)
    (when-let ((callback (agent-harbor-sse-client-error-callback client)))
      (funcall callback event))
    (agent-harbor-sse--schedule-reconnect client)))

(defun agent-harbor-sse--buffer-killed (client)
  "Clean up CLIENT when its process buffer is killed."
  (agent-harbor-sse-close client))

(defun agent-harbor-sse-start (client)
  "Start or restart CLIENT."
  (unless (executable-find "curl")
    (signal 'agent-harbor-sse-error '("curl is required for SSE")))
  (let* ((buffer (or (agent-harbor-sse-client-buffer client)
                     (generate-new-buffer " *agent-harbor-sse*")))
         (args (append
                '("--noproxy" "*" "--no-buffer" "--silent" "--show-error" "--location"
                  "--header" "Accept: text/event-stream")
                (when-let ((id (agent-harbor-sse-client-last-event-id client)))
                  (list "--header" (concat "Last-Event-ID: " id)))
                (list (agent-harbor-sse-client-url client)))))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook
                (lambda () (agent-harbor-sse--buffer-killed client)) nil t))
    (setf (agent-harbor-sse-client-buffer client) buffer
          (agent-harbor-sse-client-closed-p client) nil
          (agent-harbor-sse-client-partial client) nil
          (agent-harbor-sse-client-event-lines client) nil)
    (let ((process (apply #'start-process "agent-harbor-sse" buffer "curl" args)))
      (set-process-query-on-exit-flag process nil)
      (set-process-filter process (lambda (proc chunk)
                                    (agent-harbor-sse--filter client proc chunk)))
      (set-process-sentinel process (lambda (proc event)
                                      (agent-harbor-sse--sentinel client proc event)))
      (setf (agent-harbor-sse-client-process client) process)
      client)))

(defun agent-harbor-sse-open (url callback &optional error-callback)
  "Open URL as an SSE stream and call CALLBACK for each decoded frame."
  (let ((client (agent-harbor-sse-client-create
                 :url url
                 :callback callback
                 :error-callback error-callback
                 :retry-delay agent-harbor-sse-initial-retry-delay
                 :closed-p nil)))
    (agent-harbor-sse-start client)))

(defun agent-harbor-sse-close (client)
  "Close CLIENT and cancel reconnects."
  (setf (agent-harbor-sse-client-closed-p client) t)
  (when-let ((timer (agent-harbor-sse-client-reconnect-timer client)))
    (cancel-timer timer)
    (setf (agent-harbor-sse-client-reconnect-timer client) nil))
  (when-let ((process (agent-harbor-sse-client-process client)))
    (when (process-live-p process)
      (delete-process process))
    (setf (agent-harbor-sse-client-process client) nil))
  client)

(provide 'agent-harbor-sse)

;;; agent-harbor-sse.el ends here
