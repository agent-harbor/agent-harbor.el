;;; agent-harbor-branchfs.el --- BranchFS file-name handlers -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Native Emacs file-name handler for Agent Harbor BranchFS URIs.  Mutable branch
;; heads use M49 REST mutations; snapshots, branch bases, and shares are
;; read-only manifest/blob views.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'agent-harbor-models)
(require 'agent-harbor-rest)

(defgroup agent-harbor-branchfs nil
  "Agent Harbor BranchFS virtual file support."
  :group 'agent-harbor)

(defvar-local agent-harbor-branchfs-expected-head nil
  "Branch head observed when this buffer was last read or written.")

(defvar-local agent-harbor-branchfs-uri nil
  "Agent Harbor BranchFS URI backing the current buffer.")

(defvar agent-harbor-branchfs-last-error nil
  "Last BranchFS handler error message.")

(defvar agent-harbor-branchfs--handler-regexp "\\`agent-harbor://"
  "Regexp matched by `file-name-handler-alist' for BranchFS URIs.")

(defun agent-harbor-branchfs--get (key alist)
  "Return KEY from ALIST, accepting string and symbol keys."
  (agent-harbor-models--alist-get key alist))

(defun agent-harbor-branchfs--decode-segments (value)
  "Return decoded non-empty slash-separated segments from VALUE."
  (mapcar #'url-unhex-string (split-string (or value "") "/" t)))

(defun agent-harbor-branchfs--parse (filename)
  "Parse BranchFS FILENAME into a resource alist."
  (let* ((parsed (url-generic-parse-url filename))
         (scheme (url-type parsed)))
    (unless (equal scheme "agent-harbor")
      (signal 'file-error (list "Unsupported Agent Harbor URI" filename)))
    (let* ((parts (append (when-let ((host (url-host parsed))) (list host))
                          (agent-harbor-branchfs--decode-segments
                           (car (split-string (or (url-filename parsed) "") "\\?" t)))))
           (kind (car parts)))
      (pcase parts
        ((or 'nil '(""))
         '((kind . root)
           (path . "")
           (readOnly . t)))
        (`(,(or "branch" "branches" "snapshot" "snapshots" "base" "branch-base"
                "branch-bases" "share"))
         `((kind . namespace)
           (path . "")
           (readOnly . t)))
        (`(,(or "branch" "branches") ,branch-id . ,path)
         `((kind . branch)
           (id . ,branch-id)
           (path . ,(string-join path "/"))
           (readOnly . :false)))
        (`(,(or "snapshot" "snapshots") ,snapshot-id . ,path)
         `((kind . snapshot)
           (id . ,snapshot-id)
           (path . ,(string-join path "/"))
           (readOnly . t)))
        (`(,(or "base" "branch-base" "branch-bases") ,branch-id . ,path)
         `((kind . branch-base)
           (id . ,branch-id)
           (path . ,(string-join path "/"))
           (readOnly . t)))
        (`("share" ,token ,(or "branch" "branches") ,branch-id . ,path)
         `((kind . share)
           (id . ,branch-id)
           (sourceKind . branch)
           (token . ,token)
           (path . ,(string-join path "/"))
           (readOnly . t)))
        (`("share" ,token ,(or "snapshot" "snapshots") ,snapshot-id . ,path)
         `((kind . share)
           (id . ,snapshot-id)
           (sourceKind . snapshot)
           (token . ,token)
           (path . ,(string-join path "/"))
           (readOnly . t)))
        (_ (signal 'file-error
                   (list (format "Unsupported Agent Harbor BranchFS URI kind: %s" kind)
                         filename)))))))

(defun agent-harbor-branchfs--manifest-entries (manifest)
  "Return MANIFEST entries as a list."
  (or (agent-harbor-branchfs--get "entries" manifest) nil))

(defun agent-harbor-branchfs--manifest-snapshot-id (manifest)
  "Return snapshot id from MANIFEST."
  (agent-harbor-branchfs--get "snapshotId" manifest))

(defun agent-harbor-branchfs--entry-path (entry)
  "Return path from manifest ENTRY."
  (agent-harbor-branchfs--get "path" entry))

(defun agent-harbor-branchfs--entry-kind (entry)
  "Return normalized kind from manifest ENTRY."
  (downcase (format "%s" (agent-harbor-branchfs--get "kind" entry))))

(defun agent-harbor-branchfs--entry-directory-p (entry)
  "Return non-nil when ENTRY is a directory."
  (equal (agent-harbor-branchfs--entry-kind entry) "directory"))

(defun agent-harbor-branchfs--entry-size (entry)
  "Return size from manifest ENTRY."
  (or (agent-harbor-branchfs--get "size" entry) 0))

(defun agent-harbor-branchfs--entry-hash (entry)
  "Return content hash from manifest ENTRY."
  (agent-harbor-branchfs--get "hash" entry))

(defun agent-harbor-branchfs--branch-base-snapshot-id (branch-id)
  "Return base snapshot id for BRANCH-ID."
  (let* ((branch (agent-harbor-rest-get-branch-raw branch-id))
         (base (agent-harbor-branchfs--get "baseSnapshot" branch)))
    (or (agent-harbor-branchfs--get "id" base)
        (agent-harbor-branchfs--get "baseSnapshotId" branch)
        (signal 'file-error (list "Branch has no base snapshot" branch-id)))))

(defun agent-harbor-branchfs--branch-status (branch-id)
  "Return BranchFS status for BRANCH-ID."
  (let ((default-directory temporary-file-directory))
    (agent-harbor-rest-branch-status-raw branch-id)))

(defun agent-harbor-branchfs--branch-head (branch-id)
  "Return current head snapshot id for BRANCH-ID."
  (agent-harbor-branchfs--get
   "headSnapshotId"
   (agent-harbor-branchfs--branch-status branch-id)))

(defun agent-harbor-branchfs--set-buffer-state (filename &optional expected-head read-only)
  "Record BranchFS FILENAME state on the current and visiting buffers."
  (setq-local agent-harbor-branchfs-uri filename)
  (when expected-head
    (setq-local agent-harbor-branchfs-expected-head expected-head))
  (when (booleanp read-only)
    (setq-local buffer-read-only read-only))
  (when-let ((buffer (get-file-buffer filename)))
    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (setq-local agent-harbor-branchfs-uri filename)
        (when expected-head
          (setq-local agent-harbor-branchfs-expected-head expected-head))
        (when (booleanp read-only)
          (setq-local buffer-read-only read-only))))))

(defun agent-harbor-branchfs--manifest (resource)
  "Return manifest for RESOURCE."
  (pcase (agent-harbor-branchfs--get "kind" resource)
    ('branch
     (agent-harbor-rest-agentharborfs-branch-manifest
      (agent-harbor-branchfs--get "id" resource)))
    ('snapshot
     (agent-harbor-rest-agentharborfs-snapshot-manifest
      (agent-harbor-branchfs--get "id" resource)))
    ('branch-base
     (agent-harbor-rest-agentharborfs-snapshot-manifest
      (agent-harbor-branchfs--branch-base-snapshot-id
       (agent-harbor-branchfs--get "id" resource))))
    ('share
     (let ((token (agent-harbor-branchfs--get "token" resource))
           (id (agent-harbor-branchfs--get "id" resource)))
       (if (eq (agent-harbor-branchfs--get "sourceKind" resource) 'snapshot)
           (agent-harbor-rest-agentharborfs-snapshot-manifest id nil nil nil token)
         (agent-harbor-rest-agentharborfs-branch-manifest id nil nil nil token))))
    (_ (signal 'file-error (list "Unsupported BranchFS resource" resource)))))

(defun agent-harbor-branchfs--entry (resource)
  "Return manifest entry for RESOURCE path, or nil for root."
  (let ((path (agent-harbor-branchfs--get "path" resource)))
    (unless (string-empty-p path)
      (cl-find path (agent-harbor-branchfs--manifest-entries
                     (agent-harbor-branchfs--manifest resource))
               :key #'agent-harbor-branchfs--entry-path
               :test #'equal))))

(defun agent-harbor-branchfs--path-prefix-p (parent child)
  "Return non-nil when CHILD is under PARENT."
  (if (string-empty-p parent)
      (not (string-empty-p child))
    (string-prefix-p (file-name-as-directory parent) child)))

(defun agent-harbor-branchfs--children (resource)
  "Return immediate child (NAME . ENTRY) pairs for RESOURCE directory."
  (if (memq (agent-harbor-branchfs--get "kind" resource) '(root namespace))
      nil
    (let* ((path (agent-harbor-branchfs--get "path" resource))
           (entries (agent-harbor-branchfs--manifest-entries
                     (agent-harbor-branchfs--manifest resource)))
           (children (make-hash-table :test #'equal)))
      (dolist (entry entries)
        (let ((entry-path (agent-harbor-branchfs--entry-path entry)))
          (when (and (not (equal entry-path path))
                     (agent-harbor-branchfs--path-prefix-p path entry-path))
            (let* ((relative (if (string-empty-p path)
                                 entry-path
                               (substring entry-path (1+ (length path)))))
                   (name (car (split-string relative "/" t)))
                   (child-entry (if (string-match-p "/" relative)
                                    `((path . ,(concat (file-name-as-directory path) name))
                                      (kind . "directory")
                                      (size . 0))
                                  entry)))
              (when name
                (puthash name child-entry children))))))
      (let (result)
        (maphash (lambda (name entry) (push (cons name entry) result)) children)
        (sort result (lambda (left right) (string< (car left) (car right))))))))

(defun agent-harbor-branchfs--file-bytes (resource)
  "Return raw file bytes for RESOURCE."
  (let ((path (agent-harbor-branchfs--get "path" resource)))
    (pcase (agent-harbor-branchfs--get "kind" resource)
      ('branch
       (let* ((status (agent-harbor-branchfs--branch-status
                       (agent-harbor-branchfs--get "id" resource)))
              (head (agent-harbor-branchfs--get "headSnapshotId" status)))
         (setq-local agent-harbor-branchfs-expected-head head)
         (if (eq (agent-harbor-branchfs--get "writable" status) t)
             (agent-harbor-rest-branch-file-content
              (agent-harbor-branchfs--get "id" resource) path)
           (let* ((manifest (agent-harbor-branchfs--manifest resource))
                  (entry (cl-find path (agent-harbor-branchfs--manifest-entries manifest)
                                  :key #'agent-harbor-branchfs--entry-path
                                  :test #'equal)))
             (unless entry
               (signal 'file-missing (list "Opening input file" "No such file" path)))
             (when (agent-harbor-branchfs--entry-directory-p entry)
               (signal 'file-error (list "File is a directory" path)))
             (when (agent-harbor-branchfs--get "import" status)
               (agent-harbor-rest-agentharborfs-branch-prefetch
                (agent-harbor-branchfs--get "id" resource)
                (list path)
                nil
                1))
             (agent-harbor-rest-agentharborfs-blob
              (or (agent-harbor-branchfs--get "algorithm" manifest) "b3")
              (agent-harbor-branchfs--entry-hash entry))))))
      (_
       (let* ((manifest (agent-harbor-branchfs--manifest resource))
              (entry (cl-find path (agent-harbor-branchfs--manifest-entries manifest)
                              :key #'agent-harbor-branchfs--entry-path
                              :test #'equal)))
         (unless entry
           (signal 'file-missing (list "Opening input file" "No such file" path)))
         (when (agent-harbor-branchfs--entry-directory-p entry)
           (signal 'file-error (list "File is a directory" path)))
         (agent-harbor-rest-agentharborfs-blob
          (or (agent-harbor-branchfs--get "algorithm" manifest) "b3")
          (agent-harbor-branchfs--entry-hash entry)
          (agent-harbor-branchfs--get "token" resource)))))))

(defun agent-harbor-branchfs--ensure-writable (resource)
  "Signal a permission error unless RESOURCE is a writable branch path."
  (unless (eq (agent-harbor-branchfs--get "kind" resource) 'branch)
    (signal 'file-error (list "Agent Harbor BranchFS resource is read-only")))
  (when (string-empty-p (agent-harbor-branchfs--get "path" resource))
    (signal 'file-error (list "Agent Harbor BranchFS mutation requires a path")))
  (let ((status (agent-harbor-rest-branch-status-raw
                 (agent-harbor-branchfs--get "id" resource))))
    (unless (eq (agent-harbor-branchfs--get "writable" status) t)
      (signal 'file-error
              (list (format "Agent Harbor BranchFS branch is %s; writes are disabled"
                            (or (agent-harbor-branchfs--get "mutationMode" status)
                                "read-only")))))
    (or agent-harbor-branchfs-expected-head
        (agent-harbor-branchfs--get "headSnapshotId" status))))

(defun agent-harbor-branchfs--record-mutation (filename response)
  "Record successful mutation RESPONSE for FILENAME."
  (let* ((resource (agent-harbor-branchfs--parse filename))
         (head-metadata (agent-harbor-branchfs--get "head" response))
         (head (or (agent-harbor-branchfs--get "headSnapshotId" response)
                   (and head-metadata
                        (agent-harbor-branchfs--get "headSnapshotId" head-metadata))
                   (agent-harbor-branchfs--get "head_snapshot_id" response)
                   (and head-metadata
                        (agent-harbor-branchfs--get "head_snapshot_id" head-metadata))
                   (when (eq (agent-harbor-branchfs--get "kind" resource) 'branch)
                     (agent-harbor-branchfs--branch-head
                      (agent-harbor-branchfs--get "id" resource))))))
    (agent-harbor-branchfs--set-buffer-state filename head nil)))

(defun agent-harbor-branchfs--readable-string (bytes)
  "Decode raw BYTES for insertion into an Emacs buffer."
  (decode-coding-string bytes 'utf-8))

(defun agent-harbor-branchfs--handler-inhibited (operation args)
  "Run OPERATION with ARGS without the BranchFS handler."
  (let ((inhibit-file-name-handlers
         (cons 'agent-harbor-branchfs-file-name-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun agent-harbor-branchfs--insert-file-contents
    (filename &optional visit beg end replace)
  "Implement `insert-file-contents' for FILENAME."
  (when (or beg end)
    (signal 'file-error (list "Partial BranchFS reads are not supported" filename)))
  (let* ((resource (agent-harbor-branchfs--parse filename))
         (bytes (agent-harbor-branchfs--file-bytes resource))
         (text (agent-harbor-branchfs--readable-string bytes)))
    (when replace
      (delete-region (point-min) (point-max)))
    (insert text)
    (when visit
      (setq buffer-file-name filename))
    (let ((status (when (eq (agent-harbor-branchfs--get "kind" resource) 'branch)
                    (agent-harbor-branchfs--branch-status
                     (agent-harbor-branchfs--get "id" resource)))))
      (agent-harbor-branchfs--set-buffer-state
       filename
       (and status (agent-harbor-branchfs--get "headSnapshotId" status))
       (not (and (eq (agent-harbor-branchfs--get "kind" resource) 'branch)
                 (eq (agent-harbor-branchfs--get "writable" status) t)))))
    (list filename (length text))))

(defun agent-harbor-branchfs--write-region
    (start end filename &optional append visit _lockname _mustbenew)
  "Implement `write-region' for BranchFS."
  (when append
    (signal 'file-error (list "Appending to BranchFS files is not supported" filename)))
  (let* ((resource (agent-harbor-branchfs--parse filename))
         (expected (agent-harbor-branchfs--ensure-writable resource))
         (text (cond
                ((stringp start) start)
                ((and (null start) (null end))
                 (buffer-substring-no-properties (point-min) (point-max)))
                (t
                 (buffer-substring-no-properties start end))))
         (response (condition-case err
                       (agent-harbor-rest-write-branch-file
                        (agent-harbor-branchfs--get "id" resource)
                        (agent-harbor-branchfs--get "path" resource)
                        text
                        expected)
                     (error
                      (setq agent-harbor-branchfs-last-error
                            (error-message-string err))
                      (signal 'file-error
                              (list (format "Agent Harbor BranchFS write failed: %s"
                                            agent-harbor-branchfs-last-error)
                                    filename))))))
    (agent-harbor-branchfs--record-mutation filename response)
    (when visit
      (setq buffer-file-name filename)
      (set-buffer-modified-p nil))
    nil))

(defun agent-harbor-branchfs--file-exists-p (filename)
  "Return non-nil when FILENAME exists."
  (let* ((resource (agent-harbor-branchfs--parse filename))
         (path (agent-harbor-branchfs--get "path" resource)))
    (or (string-empty-p path)
        (not (null (agent-harbor-branchfs--entry resource))))))

(defun agent-harbor-branchfs--file-directory-p (filename)
  "Return non-nil when FILENAME is a directory."
  (let* ((resource (agent-harbor-branchfs--parse filename))
         (path (agent-harbor-branchfs--get "path" resource))
         (entry (agent-harbor-branchfs--entry resource)))
    (or (string-empty-p path)
        (and entry (agent-harbor-branchfs--entry-directory-p entry)))))

(defun agent-harbor-branchfs--file-attributes (filename &optional _id-format)
  "Return file attributes for FILENAME."
  (let* ((resource (agent-harbor-branchfs--parse filename))
         (path (agent-harbor-branchfs--get "path" resource))
         (entry (agent-harbor-branchfs--entry resource))
         (directory (or (string-empty-p path)
                        (and entry (agent-harbor-branchfs--entry-directory-p entry))))
         (size (if directory 0 (agent-harbor-branchfs--entry-size entry)))
         (mutable (eq (agent-harbor-branchfs--get "kind" resource) 'branch))
         (modes (cond
                 ((and directory mutable) "drwxr-xr-x")
                 (directory "dr-xr-xr-x")
                 (mutable "-rw-r--r--")
                 (t "-r--r--r--"))))
    (when (or (string-empty-p path) entry)
      (list (if directory t nil)
            1 "agent-harbor" "agent-harbor"
            '(0 0) '(0 0) '(0 0)
            size modes t 0 0))))

(defun agent-harbor-branchfs--directory-files
    (directory &optional full match nosort count)
  "Return BranchFS DIRECTORY files."
  (let* ((resource (agent-harbor-branchfs--parse directory))
         (children (mapcar #'car (agent-harbor-branchfs--children resource)))
         (names (append '("." "..") children)))
    (when match
      (setq names (cl-remove-if-not (lambda (name) (string-match-p match name)) names)))
    (unless nosort
      (setq names (sort names #'string<)))
    (when count
      (setq names (seq-take names count)))
    (if full
        (mapcar (lambda (name)
                  (if (member name '("." ".."))
                      (concat (file-name-as-directory directory) name)
                    (concat (file-name-as-directory directory) name)))
                names)
      names)))

(defun agent-harbor-branchfs--file-name-all-completions (file directory)
  "Return BranchFS completions for FILE in DIRECTORY."
  (let* ((resource (agent-harbor-branchfs--parse directory))
         (children (agent-harbor-branchfs--children resource)))
    (delq nil
          (mapcar (lambda (child)
                    (let ((name (car child))
                          (entry (cdr child)))
                      (when (string-prefix-p file name)
                        (if (agent-harbor-branchfs--entry-directory-p entry)
                            (concat name "/")
                          name))))
                  children))))

(defun agent-harbor-branchfs--make-directory (directory &optional parents)
  "Create BranchFS DIRECTORY."
  (let* ((resource (agent-harbor-branchfs--parse directory))
         (expected (agent-harbor-branchfs--ensure-writable resource)))
    (when parents
      (let ((prefix ""))
        (dolist (part (split-string (agent-harbor-branchfs--get "path" resource) "/" t))
          (setq prefix (if (string-empty-p prefix) part (concat prefix "/" part)))
          (let ((part-resource `((kind . branch)
                                 (id . ,(agent-harbor-branchfs--get "id" resource))
                                 (path . ,prefix))))
            (unless (agent-harbor-branchfs--file-exists-p
                     (format "agent-harbor://branch/%s/%s"
                             (agent-harbor-branchfs--get "id" resource)
                             prefix))
              (setq expected
                    (agent-harbor-branchfs--get
                     "headSnapshotId"
                     (agent-harbor-rest-create-branch-directory
                      (agent-harbor-branchfs--get "id" part-resource)
                      (agent-harbor-branchfs--get "path" part-resource)
                      expected))))))))
    (unless parents
      (agent-harbor-branchfs--record-mutation
       directory
       (agent-harbor-rest-create-branch-directory
        (agent-harbor-branchfs--get "id" resource)
        (agent-harbor-branchfs--get "path" resource)
        expected)))))

(defun agent-harbor-branchfs--delete (filename)
  "Delete BranchFS FILENAME."
  (let* ((resource (agent-harbor-branchfs--parse filename))
         (expected (agent-harbor-branchfs--ensure-writable resource))
         (response (agent-harbor-rest-delete-branch-path
                    (agent-harbor-branchfs--get "id" resource)
                    (agent-harbor-branchfs--get "path" resource)
                    expected)))
    (agent-harbor-branchfs--record-mutation filename response)))

(defun agent-harbor-branchfs--rename (file newname &optional ok-if-already-exists)
  "Rename BranchFS FILE to NEWNAME."
  (let* ((old (agent-harbor-branchfs--parse file))
         (new (agent-harbor-branchfs--parse newname)))
    (unless (and (eq (agent-harbor-branchfs--get "kind" old) 'branch)
                 (eq (agent-harbor-branchfs--get "kind" new) 'branch)
                 (equal (agent-harbor-branchfs--get "id" old)
                        (agent-harbor-branchfs--get "id" new)))
      (signal 'file-error (list "BranchFS rename requires one mutable branch" file newname)))
    (let ((response (agent-harbor-rest-move-branch-path
                     (agent-harbor-branchfs--get "id" old)
                     (agent-harbor-branchfs--get "path" old)
                     (agent-harbor-branchfs--get "path" new)
                     ok-if-already-exists
                     (agent-harbor-branchfs--ensure-writable old))))
      (agent-harbor-branchfs--record-mutation newname response))))

(defun agent-harbor-branchfs--copy-file
    (file newname &optional ok-if-already-exists _keep-time _preserve-uid-gid _preserve-permissions)
  "Copy BranchFS FILE to NEWNAME."
  (when (and (not ok-if-already-exists)
             (agent-harbor-branchfs--file-exists-p newname))
    (signal 'file-already-exists (list "File exists" newname)))
  (let* ((new (agent-harbor-branchfs--parse newname))
         (expected (agent-harbor-branchfs--ensure-writable new))
         (bytes (agent-harbor-branchfs--file-bytes
                 (agent-harbor-branchfs--parse file)))
         (response (agent-harbor-rest-write-branch-file
                    (agent-harbor-branchfs--get "id" new)
                    (agent-harbor-branchfs--get "path" new)
                    bytes
                    expected)))
    (agent-harbor-branchfs--record-mutation newname response)))

(defun agent-harbor-branchfs-file-name-handler (operation &rest args)
  "Handle BranchFS file OPERATION with ARGS."
  (let ((default-directory temporary-file-directory))
    (pcase operation
      ('insert-file-contents
       (apply #'agent-harbor-branchfs--insert-file-contents args))
      ('write-region
       (apply #'agent-harbor-branchfs--write-region args))
      ('file-exists-p
       (agent-harbor-branchfs--file-exists-p (car args)))
      ('file-readable-p
       (agent-harbor-branchfs--file-exists-p (car args)))
      ('file-writable-p
       (condition-case nil
           (progn
             (agent-harbor-branchfs--ensure-writable
              (agent-harbor-branchfs--parse (car args)))
             t)
         (error nil)))
      ('file-directory-p
       (agent-harbor-branchfs--file-directory-p (car args)))
      ('file-regular-p
       (and (agent-harbor-branchfs--file-exists-p (car args))
            (not (agent-harbor-branchfs--file-directory-p (car args)))))
      ('file-attributes
       (apply #'agent-harbor-branchfs--file-attributes args))
      ('file-modes
       (let ((resource (agent-harbor-branchfs--parse (car args))))
         (if (eq (agent-harbor-branchfs--get "kind" resource) 'branch)
             #o644
           #o444)))
      ('set-file-modes
       nil)
      ('access-file
       (unless (agent-harbor-branchfs--file-exists-p (car args))
         (signal 'file-missing (list "Accessing file" "No such file" (car args)))))
      ('directory-files
       (apply #'agent-harbor-branchfs--directory-files args))
      ('file-name-all-completions
       (apply #'agent-harbor-branchfs--file-name-all-completions args))
      ('file-name-completion
       (try-completion (car args)
                       (mapcar #'list
                               (agent-harbor-branchfs--file-name-all-completions
                                (car args) (cadr args)))))
      ('make-directory
       (apply #'agent-harbor-branchfs--make-directory args))
      ('delete-file
       (agent-harbor-branchfs--delete (car args)))
      ('delete-directory
       (agent-harbor-branchfs--delete (car args)))
      ('rename-file
       (apply #'agent-harbor-branchfs--rename args))
      ('copy-file
       (apply #'agent-harbor-branchfs--copy-file args))
      ('expand-file-name
       (car args))
      ('file-truename
       (car args))
      ('substitute-in-file-name
       (car args))
      (_ (agent-harbor-branchfs--handler-inhibited operation args)))))

(defun agent-harbor-branchfs--after-find-file ()
  "Normalize buffer flags after visiting a BranchFS URI."
  (when (and buffer-file-name
             (string-match-p agent-harbor-branchfs--handler-regexp buffer-file-name))
    (let ((resource (agent-harbor-branchfs--parse buffer-file-name)))
      (setq-local backup-inhibited t)
      (setq-local make-backup-files nil)
      (setq-local agent-harbor-branchfs-uri buffer-file-name)
      (if (eq (agent-harbor-branchfs--get "kind" resource) 'branch)
          (let ((status (agent-harbor-branchfs--branch-status
                         (agent-harbor-branchfs--get "id" resource))))
            (setq-local buffer-read-only
                        (not (eq (agent-harbor-branchfs--get "writable" status) t)))
            (setq-local agent-harbor-branchfs-expected-head
                        (agent-harbor-branchfs--get "headSnapshotId" status)))
        (setq-local buffer-read-only t)))))

(defun agent-harbor-branchfs--after-save ()
  "Refresh BranchFS write guard state after a successful save."
  (when (and buffer-file-name
             (string-match-p agent-harbor-branchfs--handler-regexp buffer-file-name))
    (let ((resource (agent-harbor-branchfs--parse buffer-file-name)))
      (when (eq (agent-harbor-branchfs--get "kind" resource) 'branch)
        (setq-local agent-harbor-branchfs-uri buffer-file-name)
        (setq-local agent-harbor-branchfs-expected-head
                    (agent-harbor-branchfs--branch-head
                     (agent-harbor-branchfs--get "id" resource)))))))

;;;###autoload
(defun agent-harbor-branchfs-register ()
  "Register Agent Harbor BranchFS file-name handlers."
  (interactive)
  (unless (assoc agent-harbor-branchfs--handler-regexp file-name-handler-alist)
    (add-to-list 'file-name-handler-alist
                 (cons agent-harbor-branchfs--handler-regexp
                       #'agent-harbor-branchfs-file-name-handler))))

(agent-harbor-branchfs-register)
(add-hook 'find-file-hook #'agent-harbor-branchfs--after-find-file)
(add-hook 'after-save-hook #'agent-harbor-branchfs--after-save)

(provide 'agent-harbor-branchfs)

;;; agent-harbor-branchfs.el ends here
