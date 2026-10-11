;;; agent-harbor-rest.el --- Agent Harbor REST client -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Synchronous REST boundary used by the Emacs MVP.  plz.el is preferred when
;; present; repo-local test environments without plz use curl or url.el.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'agent-harbor-models)

(declare-function plz "ext:plz")
(declare-function plz-error-p "ext:plz")

(defgroup agent-harbor-rest nil
  "Agent Harbor REST client."
  :group 'tools)

(defcustom agent-harbor-rest-base-url nil
  "Optional daemon base URL override for REST calls."
  :type '(choice (const :tag "Discover daemon" nil) string)
  :group 'agent-harbor-rest)

(defcustom agent-harbor-rest-timeout 5
  "Per-request REST timeout in seconds.

5 s is the deliberate ceiling for editor panel refreshes under realistic
load.  The Agent Harbor sidebar polls the daemon on a bounded refresh
loop; individual HTTP calls are allowed to spend up to 5 s without
hurting interactivity because the wait-state loops already cap how often
they fire.  Under the full `just test-rust' parallel matrix the host
load routinely exceeds 200 — the daemon is healthy but its tokio
workers are scheduled against the background CPU storm and ~2 s of
curl wall-time is normal.  The previous 2 s default surfaced as
`curl: (28) Operation timed out after 1503 milliseconds with 0 bytes
received' in panel error reports and `tasks: []' in the panel state
snapshot.  See docs/contributors/how-to-solve-test-concurrency-issues.md
for the broader policy on workspace-load-sensitive deadlines."
  :type 'number
  :group 'agent-harbor-rest)

(defcustom agent-harbor-rest-retry-delays '(0.05 0.1 0.2)
  "Retry backoff delays in seconds for boundary REST calls."
  :type '(repeat number)
  :group 'agent-harbor-rest)

(define-error 'agent-harbor-rest-error "Agent Harbor REST error")

(defun agent-harbor-rest-normalize-url (url)
  "Normalize daemon URL string URL."
  (let ((trimmed (string-trim url)))
    (when (string-empty-p trimmed)
      (signal 'agent-harbor-rest-error '("daemon URL is empty")))
    (setq trimmed
          (if (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*://" trimmed)
              trimmed
            (concat "http://" trimmed)))
    (replace-regexp-in-string "/+\\'" "" trimmed)))

(defun agent-harbor-rest-api-base-url (base-url)
  "Return the v1 API base URL for BASE-URL."
  (concat (agent-harbor-rest-normalize-url base-url) "/api/v1"))

(defun agent-harbor-rest--decode-json-string (body)
  "Decode JSON BODY into alists and lists."
  (json-parse-string body :object-type 'alist :array-type 'list :null-object nil :false-object :false))

(defun agent-harbor-rest--json-from-current-buffer ()
  "Read the HTTP response in the current url.el buffer as JSON."
  (goto-char (point-min))
  (unless (re-search-forward "\r?\n\r?\n" nil t)
    (signal 'agent-harbor-rest-error '("HTTP response did not contain headers")))
  (agent-harbor-rest--decode-json-string
   (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8)))

(defun agent-harbor-rest--encode-json (value)
  "Encode VALUE as a UTF-8 JSON string."
  (encode-coding-string (json-serialize value) 'utf-8))

(defun agent-harbor-rest--query-string (pairs)
  "Return URL query string for PAIRS."
  (mapconcat
   (lambda (pair)
     (format "%s=%s"
             (url-hexify-string (car pair))
             (url-hexify-string (cdr pair))))
   pairs
   "&"))

(defun agent-harbor-rest--http-body-from-current-buffer ()
  "Return the current url.el response body as a UTF-8 string."
  (goto-char (point-min))
  (unless (re-search-forward "\r?\n\r?\n" nil t)
    (signal 'agent-harbor-rest-error '("HTTP response did not contain headers")))
  (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8))

(defun agent-harbor-rest--problem-detail (body)
  "Return compact Problem+JSON detail text from BODY when possible."
  (condition-case nil
      (let* ((json (agent-harbor-rest--decode-json-string body))
             (detail (or (alist-get 'detail json)
                         (alist-get "detail" json nil nil #'equal)))
             (errors (or (alist-get 'errors json)
                         (alist-get "errors" json nil nil #'equal))))
        (string-join
         (delq nil
               (list detail
                     (when errors (format "%S" errors))))
         " "))
    (error body)))

(defun agent-harbor-rest--curl-json (method url &optional body extra-headers)
  "Fetch URL as JSON through curl using METHOD, optional JSON BODY, and EXTRA-HEADERS."
  (let* ((buffer (generate-new-buffer " *agent-harbor-curl*"))
         (args `("--noproxy" "*" "--fail-with-body" "--silent" "--show-error" "--location"
                 "--max-time" ,(number-to-string agent-harbor-rest-timeout)
                 "--request" ,method
                 "--header" "Accept: application/json"))
         (_ (when body
              (setq args (append args
                                 `("--header" "Content-Type: application/json"
                                   "--data-binary" ,(agent-harbor-rest--encode-json body))))))
         (_ (dolist (header extra-headers)
              (setq args (append args `("--header" ,(format "%s: %s" (car header) (cdr header)))))))
         (status (unwind-protect
                     (apply #'call-process "curl" nil buffer nil
                            (append args (list url)))
                   (unless (buffer-live-p buffer)
                     (setq buffer nil)))))
    (unwind-protect
        (if (and (integerp status) (zerop status))
            (with-current-buffer buffer
              (agent-harbor-rest--decode-json-string
               (decode-coding-string (buffer-string) 'utf-8)))
          (signal 'agent-harbor-rest-error
                  (list (format "curl request failed for %s: %s" url
                                (and buffer (with-current-buffer buffer (buffer-string)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun agent-harbor-rest--url-json (method url &optional body extra-headers)
  "Fetch URL as JSON through url.el using METHOD, optional JSON BODY, and EXTRA-HEADERS."
  (let ((url-request-method method)
        (url-request-extra-headers
         (append '(("Accept" . "application/json"))
                 (when body '(("Content-Type" . "application/json")))
                 extra-headers))
        (url-request-data (and body (agent-harbor-rest--encode-json body)))
        (url-proxy-services nil))
    (let ((buffer (url-retrieve-synchronously url t nil agent-harbor-rest-timeout)))
      (unless buffer
        (signal 'agent-harbor-rest-error (list (format "request timed out for %s" url))))
      (unwind-protect
          (with-current-buffer buffer
            (let ((status (bound-and-true-p url-http-response-status)))
              (unless (and (integerp status) (<= 200 status 299))
                (signal 'agent-harbor-rest-error
                        (list (format "request failed for %s with HTTP %s: %s"
                                      url status
                                      (agent-harbor-rest--problem-detail
                                       (agent-harbor-rest--http-body-from-current-buffer)))))))
            (agent-harbor-rest--json-from-current-buffer))
        (kill-buffer buffer)))))

(defun agent-harbor-rest--plz-json (method url &optional body extra-headers)
  "Fetch URL as JSON through plz.el using METHOD, optional JSON BODY, and EXTRA-HEADERS."
  (unless (require 'plz nil t)
    (signal 'agent-harbor-rest-error '("plz is unavailable")))
  (plz (intern (downcase method)) url
    :as (lambda ()
          (json-parse-buffer :object-type 'alist :array-type 'list
                             :null-object nil :false-object :false))
    :headers (append '(("Accept" . "application/json"))
                     (when body '(("Content-Type" . "application/json")))
                     extra-headers)
    :body (and body (agent-harbor-rest--encode-json body))
    :timeout agent-harbor-rest-timeout))

(defun agent-harbor-rest--json-once (method url &optional body extra-headers)
  "Fetch URL once using METHOD, optional BODY, and EXTRA-HEADERS with the best backend."
  (cond
   ((require 'plz nil t) (agent-harbor-rest--plz-json method url body extra-headers))
   ((executable-find "curl") (agent-harbor-rest--curl-json method url body extra-headers))
   (t (agent-harbor-rest--url-json method url body extra-headers))))

(defun agent-harbor-rest-json-request (method url &optional body retry-delays extra-headers)
  "Request URL as JSON with METHOD, optional BODY, RETRY-DELAYS, and EXTRA-HEADERS."
  (let ((delays (or retry-delays agent-harbor-rest-retry-delays))
        last-error)
    (catch 'done
      (while t
        (condition-case err
            (throw 'done (agent-harbor-rest--json-once method url body extra-headers))
          (error (setq last-error err)))
        (if delays
            (progn
              (sleep-for (car delays))
              (setq delays (cdr delays)))
          (signal (car last-error) (cdr last-error)))))))

(defun agent-harbor-rest-get-json (url &optional retry-delays)
  "Fetch URL as JSON, retrying with RETRY-DELAYS at the boundary."
  (agent-harbor-rest-json-request "GET" url nil retry-delays))

(defun agent-harbor-rest--plz-get-json-async (url callback error-callback)
  "Asynchronously GET URL as JSON through plz, dispatching to CALLBACK or ERROR-CALLBACK.
Returns the plz process object so callers can track it as in-flight."
  (plz 'get url
    :as (lambda ()
          (json-parse-buffer :object-type 'alist :array-type 'list
                             :null-object nil :false-object :false))
    :headers '(("Accept" . "application/json"))
    :timeout agent-harbor-rest-timeout
    :then (lambda (result) (funcall callback result))
    :else (lambda (err)
            (when error-callback
              (funcall error-callback
                       (if (plz-error-p err)
                           (format "%S" err)
                         (error-message-string err)))))))

(defun agent-harbor-rest--curl-get-json-async (url callback error-callback)
  "Asynchronously GET URL as JSON through curl, dispatching to CALLBACK or ERROR-CALLBACK.
Returns the curl process object so callers can track it as in-flight."
  (let* ((buffer (generate-new-buffer " *agent-harbor-curl-async*"))
         (args `("--noproxy" "*" "--fail-with-body" "--silent" "--show-error" "--location"
                 "--max-time" ,(number-to-string agent-harbor-rest-timeout)
                 "--request" "GET"
                 "--header" "Accept: application/json"
                 ,url))
         (process (make-process
                   :name "agent-harbor-curl-async"
                   :buffer buffer
                   :command (cons "curl" args)
                   :noquery t
                   :connection-type 'pipe
                   :sentinel
                   (lambda (proc _event)
                     (unless (process-live-p proc)
                       (unwind-protect
                           (let ((status (process-exit-status proc))
                                 (body (and (buffer-live-p buffer)
                                            (with-current-buffer buffer
                                              (decode-coding-string (buffer-string) 'utf-8)))))
                             (if (and (integerp status) (zerop status))
                                 (condition-case err
                                     (funcall callback
                                              (agent-harbor-rest--decode-json-string body))
                                   (error (when error-callback
                                            (funcall error-callback (error-message-string err)))))
                               (when error-callback
                                 (funcall error-callback
                                          (format "curl request failed for %s: %s" url body)))))
                         (when (buffer-live-p buffer)
                           (kill-buffer buffer))))))))
    process))

(defun agent-harbor-rest--url-get-json-async (url callback error-callback)
  "Asynchronously GET URL as JSON through url.el, dispatching to CALLBACK or ERROR-CALLBACK.
Returns nil; url.el manages its own retrieval buffer/process internally."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Accept" . "application/json")))
        (url-proxy-services nil))
    (url-retrieve
     url
     (lambda (status)
       (let ((retrieval-buffer (current-buffer)))
         (unwind-protect
             (if (plist-get status :error)
                 (when error-callback
                   (funcall error-callback (format "%S" (plist-get status :error))))
               (condition-case err
                   (let ((http-status (bound-and-true-p url-http-response-status)))
                     (if (and (integerp http-status) (<= 200 http-status 299))
                         (funcall callback (agent-harbor-rest--json-from-current-buffer))
                       (when error-callback
                         (funcall error-callback
                                  (format "request failed for %s with HTTP %s" url http-status)))))
                 (error (when error-callback
                          (funcall error-callback (error-message-string err))))))
           (when (buffer-live-p retrieval-buffer)
             (kill-buffer retrieval-buffer)))))
     nil t)
    nil))

(defun agent-harbor-rest-get-json-async (url callback &optional error-callback)
  "Asynchronously GET URL as JSON, dispatching to CALLBACK on success.
On failure, ERROR-CALLBACK is invoked with a string message when provided.

Uses plz (with `:then'/`:else'), falling back to an async curl
`make-process' and finally to an async `url-retrieve'.  Returns the
backing process object when one exists (plz/curl), or nil for the
url.el path; callers use the return value only to detect whether a
request was actually launched, not to block on it."
  (cond
   ((require 'plz nil t)
    (agent-harbor-rest--plz-get-json-async url callback error-callback))
   ((executable-find "curl")
    (agent-harbor-rest--curl-get-json-async url callback error-callback))
   (t
    (agent-harbor-rest--url-get-json-async url callback error-callback))))

(defun agent-harbor-rest--response-body-string ()
  "Return the current url.el response body as a unibyte string."
  (goto-char (point-min))
  (unless (re-search-forward "\r?\n\r?\n" nil t)
    (signal 'agent-harbor-rest-error '("HTTP response did not contain headers")))
  (encode-coding-string (buffer-substring-no-properties (point) (point-max)) 'binary))

(defun agent-harbor-rest--url-bytes (method url &optional body content-type extra-headers)
  "Fetch URL as raw bytes through url.el using METHOD, BODY, CONTENT-TYPE, and EXTRA-HEADERS."
  (let ((url-request-method method)
        (url-request-extra-headers
         (append '(("Accept" . "application/octet-stream"))
                 (when content-type `(("Content-Type" . ,content-type)))
                 extra-headers))
        (url-request-data (and body (encode-coding-string body 'binary)))
        (url-proxy-services nil))
    (let ((buffer (url-retrieve-synchronously url t nil agent-harbor-rest-timeout)))
      (unless buffer
        (signal 'agent-harbor-rest-error (list (format "request timed out for %s" url))))
      (unwind-protect
          (with-current-buffer buffer
            (let ((status (bound-and-true-p url-http-response-status)))
              (unless (and (integerp status) (<= 200 status 299))
                (signal 'agent-harbor-rest-error
                        (list (format "request failed for %s with HTTP %s: %s"
                                      url status
                                      (agent-harbor-rest--problem-detail
                                       (agent-harbor-rest--http-body-from-current-buffer)))))))
            (agent-harbor-rest--response-body-string))
        (kill-buffer buffer)))))

(defun agent-harbor-rest-bytes-request (method url &optional body content-type extra-headers)
  "Request URL as raw bytes with METHOD, optional BODY, CONTENT-TYPE, and EXTRA-HEADERS."
  (agent-harbor-rest--url-bytes method url body content-type extra-headers))

(defun agent-harbor-rest--base-url (base-url)
  "Resolve BASE-URL or the discovered daemon endpoint."
  (or base-url
      agent-harbor-rest-base-url
      (and (boundp 'agent-harbor-daemon-endpoint) agent-harbor-daemon-endpoint)
      (signal 'agent-harbor-rest-error '("daemon endpoint is not discovered"))))

(defun agent-harbor-rest-list-tasks (&optional base-url)
  "Return daemon tasks from BASE-URL as `agent-harbor-task' values."
  (let* ((url (concat (agent-harbor-rest-api-base-url
                       (agent-harbor-rest--base-url base-url))
                      "/tasks"))
         (json (agent-harbor-rest-get-json url))
         (tasks (or (alist-get 'tasks json) (alist-get "tasks" json nil nil #'equal))))
    (mapcar #'agent-harbor-models-task-from-json tasks)))

(defun agent-harbor-rest-list-tasks-raw (&optional base-url)
  "Return raw daemon task JSON alists from BASE-URL."
  (let* ((url (concat (agent-harbor-rest-api-base-url
                       (agent-harbor-rest--base-url base-url))
                      "/tasks"))
         (json (agent-harbor-rest-get-json url)))
    (or (alist-get 'tasks json) (alist-get "tasks" json nil nil #'equal))))

(defun agent-harbor-rest-task-sessions (task-id &optional base-url)
  "Return raw session summaries for TASK-ID from BASE-URL."
  (let* ((url (concat (agent-harbor-rest-api-base-url
                       (agent-harbor-rest--base-url base-url))
                      "/tasks/" (url-hexify-string task-id) "/sessions"))
         (json (agent-harbor-rest-get-json url)))
    (or (alist-get 'sessions json) (alist-get "sessions" json nil nil #'equal))))

(defun agent-harbor-rest-list-sessions-raw (&optional base-url)
  "Return raw daemon sessions from BASE-URL."
  (let* ((url (concat (agent-harbor-rest-api-base-url
                       (agent-harbor-rest--base-url base-url))
                      "/sessions?perPage=100"))
         (json (agent-harbor-rest-get-json url)))
    (or (alist-get 'items json) (alist-get "items" json nil nil #'equal))))

(defun agent-harbor-rest-list-branches-raw (&optional session-id base-url)
  "Return raw daemon AgentHarborFS branches for optional SESSION-ID from BASE-URL."
  (let* ((url (concat (agent-harbor-rest-api-base-url
                       (agent-harbor-rest--base-url base-url))
                      "/branches"))
         (json (agent-harbor-rest-get-json url))
         (branches (or (alist-get 'branches json) (alist-get "branches" json nil nil #'equal))))
    (if session-id
        (cl-remove-if-not
         (lambda (branch)
           (equal (or (agent-harbor-models--alist-get "sessionId" branch)
                      (agent-harbor-models--alist-get "session_id" branch))
                  session-id))
         branches)
      branches)))

(defun agent-harbor-rest-get-branch-raw (branch-id &optional base-url)
  "Return raw daemon AgentHarborFS branch metadata for BRANCH-ID from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/branches/" (url-hexify-string branch-id))))
    (agent-harbor-rest-get-json url)))

(defun agent-harbor-rest-get-snapshot-raw (snapshot-id &optional base-url)
  "Return raw daemon AgentHarborFS snapshot metadata for SNAPSHOT-ID from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/snapshots/" (url-hexify-string snapshot-id))))
    (agent-harbor-rest-get-json url)))

(defun agent-harbor-rest-create-agentharborfs-import (source-url &optional token base-url materialization-mode)
  "Import AgentHarborFS SOURCE-URL through daemon BASE-URL.

TOKEN carries an optional share bearer token.  MATERIALIZATION-MODE is optional;
omitting it preserves the daemon's default lazy import behavior."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/agentharborfs/imports"))
        (payload `((url . ,source-url))))
    (when token
      (setq payload (append payload `((token . ,token)))))
    (when materialization-mode
      (setq payload (append payload `((materializationMode . ,materialization-mode)))))
    (agent-harbor-rest-json-request "POST" url payload)))

(defun agent-harbor-rest-create-agentharborfs-share (identifier scope ttl &optional base-url)
  "Create a public AgentHarborFS share URL for IDENTIFIER with SCOPE and TTL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/agentharborfs/share")))
    (agent-harbor-rest-json-request
     "POST"
     url
     `((identifier . ,identifier)
       (scope . ,scope)
       (ttl . ,ttl)))))

(defun agent-harbor-rest-branch-status-raw (branch-id &optional base-url)
  "Return AgentHarborFS branch status for BRANCH-ID from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/agentharborfs/branches/" (url-hexify-string branch-id) "/status")))
    (agent-harbor-rest-get-json url)))

(defun agent-harbor-rest-session-history (session-id &optional base-url)
  "Return raw event history response for SESSION-ID from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/sessions/" (url-hexify-string session-id)
                     "/events/history?limit=200")))
    (agent-harbor-rest-get-json url)))

(defun agent-harbor-rest-task-diff (task-id &optional base-url)
  "Return raw task diff response for TASK-ID from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/tasks/" (url-hexify-string task-id) "/diff")))
    (agent-harbor-rest-get-json url)))

(defun agent-harbor-rest-session-files (session-id &optional base-url)
  "Return raw modified-file metadata for SESSION-ID from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/sessions/" (url-hexify-string session-id)
                     "/files?perPage=100")))
    (agent-harbor-rest-get-json url)))

(defun agent-harbor-rest-session-file-content (session-id path &optional snapshot-id base-url)
  "Return raw file bytes for SESSION-ID PATH, optionally at SNAPSHOT-ID."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/sessions/" (url-hexify-string session-id)
                     "/files/content/" (url-hexify-string path)
                     (or (when snapshot-id
                           (concat "?"
                                   (agent-harbor-rest--query-string
                                    `(("snapshotId" . ,snapshot-id)))))
                         ""))))
    (agent-harbor-rest-bytes-request "GET" url)))

(defun agent-harbor-rest-branch-file-content (branch-id path &optional base-url)
  "Return raw file bytes for BRANCH-ID PATH from the branch head snapshot."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/branches/" (url-hexify-string branch-id)
                     "/files/" (url-hexify-string path))))
    (agent-harbor-rest-bytes-request "GET" url)))

(defun agent-harbor-rest-agentharborfs-branch-manifest
    (branch-id &optional prefix cursor limit token base-url)
  "Return AgentHarborFS manifest JSON for BRANCH-ID.
Optional PREFIX, CURSOR, LIMIT, TOKEN, and BASE-URL are passed to the daemon."
  (let* ((pairs (delq nil
                      (list (and prefix `("prefix" . ,prefix))
                            (and cursor `("cursor" . ,cursor))
                            (and limit `("limit" . ,(number-to-string limit)))
                            (and token `("token" . ,token)))))
         (url (concat (agent-harbor-rest-api-base-url
                       (agent-harbor-rest--base-url base-url))
                      "/agentharborfs/branches/" (url-hexify-string branch-id) "/manifest"
                      (if pairs
                          (concat "?" (agent-harbor-rest--query-string pairs))
                        ""))))
    (agent-harbor-rest-get-json url)))

(defun agent-harbor-rest-agentharborfs-snapshot-manifest
    (snapshot-id &optional prefix cursor limit token base-url)
  "Return AgentHarborFS manifest JSON for SNAPSHOT-ID.
Optional PREFIX, CURSOR, LIMIT, TOKEN, and BASE-URL are passed to the daemon."
  (let* ((pairs (delq nil
                      (list (and prefix `("prefix" . ,prefix))
                            (and cursor `("cursor" . ,cursor))
                            (and limit `("limit" . ,(number-to-string limit)))
                            (and token `("token" . ,token)))))
         (url (concat (agent-harbor-rest-api-base-url
                       (agent-harbor-rest--base-url base-url))
                      "/agentharborfs/snapshots/" (url-hexify-string snapshot-id) "/manifest"
                      (if pairs
                          (concat "?" (agent-harbor-rest--query-string pairs))
                        ""))))
    (agent-harbor-rest-get-json url)))

(defun agent-harbor-rest-agentharborfs-blob (algorithm hash &optional token base-url)
  "Return raw AgentHarborFS blob bytes for ALGORITHM and HASH."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/agentharborfs/blobs/" (url-hexify-string algorithm)
                     "/" (url-hexify-string hash)
                     (if token
                         (concat "?"
                                 (agent-harbor-rest--query-string
                                  `(("token" . ,token))))
                       ""))))
    (agent-harbor-rest-bytes-request "GET" url)))

(defun agent-harbor-rest-agentharborfs-branch-prefetch
    (branch-id &optional prefixes globs limit base-url)
  "Prefetch imported AgentHarborFS blobs for BRANCH-ID."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/agentharborfs/branches/" (url-hexify-string branch-id) "/prefetch"))
        (body nil))
    (when prefixes
      (setq body (append body `((prefixes . ,(vconcat prefixes))))))
    (when globs
      (setq body (append body `((globs . ,(vconcat globs))))))
    (when limit
      (setq body (append body `((limit . ,limit)))))
    (agent-harbor-rest-json-request "POST" url body)))

(defun agent-harbor-rest-write-branch-file (branch-id path bytes &optional expected-head base-url)
  "Write BYTES to BRANCH-ID PATH, guarded by optional EXPECTED-HEAD."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/branches/" (url-hexify-string branch-id)
                     "/files/" (url-hexify-string path)))
        (headers (when expected-head `(("If-Match" . ,(format "\"%s\"" expected-head))))))
    (agent-harbor-rest--decode-json-string
     (decode-coding-string
      (agent-harbor-rest-bytes-request
       "PUT" url (encode-coding-string bytes 'binary) "application/octet-stream" headers)
      'utf-8))))

(defun agent-harbor-rest-create-branch-directory (branch-id path &optional expected-head base-url)
  "Create directory PATH in BRANCH-ID, guarded by optional EXPECTED-HEAD."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/branches/" (url-hexify-string branch-id)
                     "/directories/" (url-hexify-string path))))
    (agent-harbor-rest-json-request
     "POST" url nil nil
     (when expected-head `(("If-Match" . ,(format "\"%s\"" expected-head)))))))

(defun agent-harbor-rest-delete-branch-path (branch-id path &optional expected-head base-url)
  "Delete PATH in BRANCH-ID, guarded by optional EXPECTED-HEAD."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/branches/" (url-hexify-string branch-id)
                     "/files/" (url-hexify-string path)))
        (headers (when expected-head `(("If-Match" . ,(format "\"%s\"" expected-head))))))
    (agent-harbor-rest--decode-json-string
     (decode-coding-string
      (agent-harbor-rest-bytes-request "DELETE" url nil nil headers)
      'utf-8))))

(defun agent-harbor-rest-move-branch-path
    (branch-id from to &optional overwrite expected-head base-url)
  "Move FROM to TO in BRANCH-ID with OVERWRITE, guarded by optional EXPECTED-HEAD."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/branches/" (url-hexify-string branch-id) "/moves")))
    (agent-harbor-rest-json-request
     "POST" url `((from . ,from) (to . ,to) (overwrite . ,(if overwrite t :false))) nil
     (when expected-head `(("If-Match" . ,(format "\"%s\"" expected-head)))))))

(defun agent-harbor-rest-checkpoint-session (session-id message &optional base-url)
  "Create a checkpoint for SESSION-ID with MESSAGE from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/sessions/" (url-hexify-string session-id) "/checkpoint")))
    (agent-harbor-rest-json-request "POST" url `((message . ,message)))))

(defun agent-harbor-rest-sandbox-diff (task-id from-branch-id to-branch-id &optional base-url)
  "Return daemon change manifest for TASK-ID from FROM-BRANCH-ID to TO-BRANCH-ID."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/sessions/" (url-hexify-string task-id) "/sandbox-diff?"
                     (agent-harbor-rest--query-string
                      `(("from" . ,from-branch-id)
                        ("to" . ,to-branch-id))))))
    (agent-harbor-rest-json-request "POST" url '())))

(defun agent-harbor-rest-stash-buffer (task-id path bytes &optional base-url)
  "Store dirty buffer BYTES for TASK-ID and PATH in the daemon stash."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/sessions/" (url-hexify-string task-id) "/stash?"
                     (agent-harbor-rest--query-string
                      `(("editor_pid" . ,(number-to-string (emacs-pid)))
                        ("path" . ,path))))))
    (agent-harbor-rest--decode-json-string
     (decode-coding-string
      (agent-harbor-rest-bytes-request
       "POST" url (encode-coding-string bytes 'binary) "application/octet-stream")
      'utf-8))))

(defun agent-harbor-rest-get-stash-buffer (task-id path &optional base-url)
  "Return dirty buffer bytes for TASK-ID and PATH from the daemon stash."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/sessions/" (url-hexify-string task-id) "/stash?"
                     (agent-harbor-rest--query-string
                      `(("editor_pid" . ,(number-to-string (emacs-pid)))
                        ("path" . ,path))))))
    (agent-harbor-rest-bytes-request "GET" url)))

(defun agent-harbor-rest-create-task (payload &optional base-url)
  "Create a task with daemon JSON PAYLOAD from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/tasks")))
    (agent-harbor-rest-json-request "POST" url payload)))

(defun agent-harbor-rest-list-feedback-raw (task-id &optional base-url)
  "Return raw feedback items for TASK-ID from BASE-URL."
  (let* ((url (concat (agent-harbor-rest-api-base-url
                       (agent-harbor-rest--base-url base-url))
                      "/tasks/" (url-hexify-string task-id) "/feedback"))
         (json (agent-harbor-rest-get-json url)))
    (or (alist-get 'items json) (alist-get "items" json nil nil #'equal))))

(defun agent-harbor-rest-respond-feedback (task-id feedback-id payload &optional base-url)
  "Respond to FEEDBACK-ID on TASK-ID with JSON PAYLOAD from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/tasks/" (url-hexify-string task-id)
                     "/feedback/" (url-hexify-string feedback-id) "/respond")))
    (agent-harbor-rest-json-request "POST" url payload)))

(defun agent-harbor-rest-transcribe-wav-file (path &optional base-url)
  "Upload WAV file PATH to the daemon transcription endpoint from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/voice/transcriptions")))
    (agent-harbor-rest--decode-json-string
     (decode-coding-string
      (agent-harbor-rest-bytes-request
       "POST"
       url
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally path)
         (buffer-string))
       "audio/wav")
      'utf-8))))

(defun agent-harbor-rest-voice-always-on-start (&optional options base-url)
  "Start an always-on voice stream with OPTIONS from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/voice/always-on")))
    (agent-harbor-rest-json-request "POST" url (or options '()))))

(defun agent-harbor-rest-voice-always-on-chunk (stream-id path sequence last &optional base-url)
  "Upload WAV PATH as always-on stream STREAM-ID chunk SEQUENCE.
When LAST is non-nil, the daemon finalizes any open VAD segment for the
stream."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/voice/always-on/" (url-hexify-string stream-id) "/chunks")))
    (agent-harbor-rest-json-request
     "POST"
     url
     `((audioBase64 . ,(base64-encode-string
                        (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally path)
                          (buffer-string))
                        t))
       (contentType . "audio/wav")
       (sequence . ,sequence)
       (last . ,(if last t :false))))))

(defun agent-harbor-rest-voice-always-on-stop (stream-id &optional base-url)
  "Finalize and drop always-on voice STREAM-ID from BASE-URL."
  (let ((url (concat (agent-harbor-rest-api-base-url
                      (agent-harbor-rest--base-url base-url))
                     "/voice/always-on/" (url-hexify-string stream-id))))
    (agent-harbor-rest-json-request "DELETE" url nil)))

(provide 'agent-harbor-rest)

;;; agent-harbor-rest.el ends here
