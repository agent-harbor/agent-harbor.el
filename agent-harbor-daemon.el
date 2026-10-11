;;; agent-harbor-daemon.el --- Agent Harbor daemon discovery -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; PID-file daemon discovery and real health/capabilities probing for the Emacs
;; package.  This mirrors the VS Code discovery contract where practical.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'agent-harbor-rest)

(defgroup agent-harbor-daemon nil
  "Agent Harbor daemon discovery."
  :group 'tools)

(defcustom agent-harbor-daemon-url nil
  "Optional daemon URL override.  When nil, discovery uses the webui PID file."
  :type '(choice (const :tag "Use PID file" nil) string)
  :group 'agent-harbor-daemon)

(defcustom agent-harbor-daemon-pid-file nil
  "Optional webui PID file path override."
  :type '(choice (const :tag "Use platform default" nil) file)
  :group 'agent-harbor-daemon)

(defcustom agent-harbor-daemon-poll-interval 1.0
  "Seconds between daemon discovery probes."
  :type 'number
  :group 'agent-harbor-daemon)

(defcustom agent-harbor-daemon-probe-timeout 5.0
  "Timeout in seconds for daemon probe requests.

Aligned with `agent-harbor-rest-timeout' so a daemon under the
workspace-wide test load (200+ system load on `just test-rust') still
completes its healthz/capabilities handshake in time.  Tests previously
surfaced as `daemon-state=disconnected' followed by `tasks: []' after
the discovery probe itself raced the 1 s budget."
  :type 'number
  :group 'agent-harbor-daemon)

(defcustom agent-harbor-daemon-auto-start nil
  "When non-nil, start a local daemon process if PID-file discovery fails."
  :type 'boolean
  :group 'agent-harbor-daemon)

(defcustom agent-harbor-daemon-executable "ah"
  "Agent Harbor executable used for daemon auto-start."
  :type 'string
  :group 'agent-harbor-daemon)

(defcustom agent-harbor-daemon-start-args
  '("daemon" "serve" "--daemon-child" "--no-acp" "--no-recording")
  "Arguments passed to `agent-harbor-daemon-executable' for auto-start."
  :type '(repeat string)
  :group 'agent-harbor-daemon)

(defvar agent-harbor-daemon-state 'disconnected
  "Current daemon connection state.")
(defvar agent-harbor-daemon-endpoint nil
  "Current discovered daemon endpoint.")
(defvar agent-harbor-daemon-last-health nil
  "Last successful daemon health response.")
(defvar agent-harbor-daemon-last-capabilities nil
  "Last successful daemon capabilities response.")
(defvar agent-harbor-daemon-last-error nil
  "Last discovery or probe error.")
(defvar agent-harbor-daemon--timer nil)
(defvar agent-harbor-daemon--process nil)

(defun agent-harbor-daemon-resolve-pid-file ()
  "Return the Agent Harbor webui PID file path."
  (or agent-harbor-daemon-pid-file
      (getenv "AH_WEBUI_PID_FILE")
      (let ((ah-home (getenv "AH_HOME")))
        (when (and ah-home (not (string-empty-p ah-home)))
          (expand-file-name "webui.pid" ah-home)))
      (cond
       ((eq system-type 'darwin)
        (expand-file-name "Library/Application Support/agent-harbor/webui.pid" "~"))
       ((eq system-type 'windows-nt)
        (expand-file-name "agent-harbor/webui.pid"
                          (or (getenv "APPDATA") (expand-file-name "AppData/Roaming" "~"))))
       (t
        (expand-file-name "agent-harbor/webui.pid"
                          (or (getenv "XDG_DATA_HOME") (expand-file-name ".local/share" "~")))))))

(defun agent-harbor-daemon--read-pid-file (path)
  "Read and validate PID file PATH."
  (unless (file-exists-p path)
    (signal 'file-missing (list "PID file missing" path)))
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-false :false)
         (parsed (json-read-file path))
         (pid (alist-get 'pid parsed))
         (port (alist-get 'port parsed))
         (host (alist-get 'host parsed))
         (started-at (alist-get 'started_at parsed)))
    (unless (and (integerp pid) (> pid 0))
      (error "webui.pid has an invalid pid"))
    (unless (and (integerp port) (> port 0) (<= port 65535))
      (error "webui.pid has an invalid port"))
    (unless (and (stringp host) (not (string-empty-p host)))
      (error "webui.pid has an invalid host"))
    (unless (and (stringp started-at) (not (string-empty-p started-at)))
      (error "webui.pid has an invalid started_at timestamp"))
    parsed))

(defun agent-harbor-daemon--pid-alive-p (pid)
  "Return non-nil when PID appears alive."
  (condition-case nil
      (progn
        (signal-process pid 0)
        t)
    (file-error nil)
    (error nil)))

(defun agent-harbor-daemon--endpoint-from-pid-file (pid-file)
  "Return daemon endpoint from PID-FILE alist."
  (agent-harbor-rest-normalize-url
   (format "http://%s:%s" (alist-get 'host pid-file) (alist-get 'port pid-file))))

(defun agent-harbor-daemon--probe (endpoint)
  "Probe ENDPOINT health and capabilities using real daemon REST calls."
  (let ((agent-harbor-rest-timeout agent-harbor-daemon-probe-timeout)
        (agent-harbor-rest-retry-delays nil)
        (health nil)
        (health-error nil))
    (dolist (path '("/api/v1/healthz" "/api/health" "/health"))
      (unless health
        (condition-case err
            (setq health (agent-harbor-rest-get-json (concat endpoint path) nil))
          (error (setq health-error err)))))
    (unless health
      (signal (car health-error) (cdr health-error)))
    (let ((capabilities
           (agent-harbor-rest-get-json
            (concat endpoint "/api/v1/platform/capabilities") nil)))
      `((baseUrl . ,endpoint)
        (apiBaseUrl . ,(concat endpoint "/api/v1"))
        (health . ,health)
        (capabilities . ,capabilities)
        (probedAt . ,(format-time-string "%FT%T%z" (current-time) t))))))

(defun agent-harbor-daemon-discover-sync ()
  "Discover and probe the daemon synchronously.
Returns an alist containing endpoint, pid file data, and probe data."
  (let* ((pid-file-path (agent-harbor-daemon-resolve-pid-file))
         (override (or agent-harbor-daemon-url (getenv "AH_DAEMON_URL"))))
    (if (and override (not (string-empty-p (string-trim override))))
        (let* ((endpoint (agent-harbor-rest-normalize-url override))
               (probe (agent-harbor-daemon--probe endpoint)))
          `((ok . t) (endpoint . ,endpoint) (pidFilePath . ,pid-file-path)
            (pidFile . ((pid . 0))) (probe . ,probe)))
      (let* ((pid-file (agent-harbor-daemon--read-pid-file pid-file-path))
             (pid (alist-get 'pid pid-file)))
        (unless (agent-harbor-daemon--pid-alive-p pid)
          (error "daemon pid %s is not alive" pid))
        (let* ((endpoint (agent-harbor-daemon--endpoint-from-pid-file pid-file))
               (probe (agent-harbor-daemon--probe endpoint)))
          `((ok . t) (endpoint . ,endpoint) (pidFilePath . ,pid-file-path)
            (pidFile . ,pid-file) (probe . ,probe)))))))

(defun agent-harbor-daemon-refresh ()
  "Refresh daemon state once."
  (setq agent-harbor-daemon-state 'starting)
  (condition-case err
      (let* ((discovery (agent-harbor-daemon-discover-sync))
             (probe (alist-get 'probe discovery)))
        (setq agent-harbor-daemon-endpoint (alist-get 'endpoint discovery)
              agent-harbor-daemon-last-health (alist-get 'health probe)
              agent-harbor-daemon-last-capabilities (alist-get 'capabilities probe)
              agent-harbor-daemon-last-error nil
              agent-harbor-daemon-state 'connected))
    (error
     (setq agent-harbor-daemon-last-error (error-message-string err)
           agent-harbor-daemon-state 'disconnected)))
  (when (and agent-harbor-daemon-auto-start
             (eq agent-harbor-daemon-state 'disconnected)
             (not (and agent-harbor-daemon--process
                       (process-live-p agent-harbor-daemon--process))))
    (agent-harbor-daemon-start-process)
    (setq agent-harbor-daemon-state 'starting))
  (force-mode-line-update t)
  agent-harbor-daemon-state)

(defun agent-harbor-daemon-start-process ()
  "Start a local Agent Harbor daemon process with `start-process'."
  (interactive)
  (unless (executable-find agent-harbor-daemon-executable)
    (error "Cannot find Agent Harbor executable: %s" agent-harbor-daemon-executable))
  (let* ((buffer (get-buffer-create " *agent-harbor-daemon*"))
         (process-environment (copy-sequence process-environment))
         (pid-file (agent-harbor-daemon-resolve-pid-file)))
    (setenv "AH_WEBUI_PID_FILE" pid-file)
    (setq agent-harbor-daemon--process
          (apply #'start-process "agent-harbor-daemon" buffer
                 agent-harbor-daemon-executable agent-harbor-daemon-start-args))
    (set-process-query-on-exit-flag agent-harbor-daemon--process nil)
    agent-harbor-daemon--process))

(defun agent-harbor-daemon-start-monitor ()
  "Start periodic daemon discovery."
  (unless (timerp agent-harbor-daemon--timer)
    (setq agent-harbor-daemon-state 'starting)
    (setq agent-harbor-daemon--timer
          (run-at-time 0 agent-harbor-daemon-poll-interval
                       #'agent-harbor-daemon-refresh))))

(defun agent-harbor-daemon-stop-monitor ()
  "Stop periodic daemon discovery."
  (when (timerp agent-harbor-daemon--timer)
    (cancel-timer agent-harbor-daemon--timer)
    (setq agent-harbor-daemon--timer nil)))

;;;###autoload
(defun agent-harbor-daemon-status ()
  "Report the current Agent Harbor daemon status."
  (interactive)
  (agent-harbor-daemon-refresh)
  (let ((message
         (pcase agent-harbor-daemon-state
           ('connected
            (format "Agent Harbor daemon connected: %s" agent-harbor-daemon-endpoint))
           ('starting "Agent Harbor daemon starting")
           (_ (format "Agent Harbor daemon disconnected: %s"
                      (or agent-harbor-daemon-last-error "unknown error"))))))
    (when (called-interactively-p 'interactive)
      (message "%s" message))
    message))

(provide 'agent-harbor-daemon)

;;; agent-harbor-daemon.el ends here
