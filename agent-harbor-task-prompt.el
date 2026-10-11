;;; agent-harbor-task-prompt.el --- Agent Harbor external-editor task prompts -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; When `ah task create' runs without `--prompt' it launches `$EDITOR' on a
;; scratch file whose name is a published contract:
;;
;;     <tmp>/agent-harbor/ah-task-prompt-<pid>-<nonce>.ahtask
;;     <tmp>/agent-harbor/ah-plan-prompt-<pid>-<nonce>.ahplan
;;
;; `agent-harbor-task-prompt-mode' activates on those names and offers
;; completion on the same trigger symbols the TUI dashboard's prompt textarea
;; uses: `/' and `~' for filesystem paths and `@' for workspace files.
;;
;; The candidates are NOT computed here.  This mode is a thin client over
;; `ah autocomplete --json', which is the single authority for both the token
;; rules (where a trigger is recognized, and what span a candidate replaces) and
;; the candidate vocabulary.  Keeping that in the `ah' binary is what stops the
;; editor modes from drifting away from the dashboard.
;;
;; Because `ah' has already filtered and ranked the candidates, they generally do
;; not share a literal prefix with the typed text (`@edi' matches
;; `crates/ah-core/src/editor.rs').  Emacs's built-in completion styles would
;; discard them all, so this file registers a pass-through completion style and
;; scopes it to its own completion category.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function server-edit "server")
(defvar server-buffer-clients)

(defgroup agent-harbor-task-prompt nil
  "Agent Harbor external-editor task prompts."
  :group 'agent-harbor)

(defcustom agent-harbor-task-prompt-ah-command "ah"
  "Executable used to query Agent Harbor prompt completions."
  :type 'string
  :group 'agent-harbor-task-prompt)

(defcustom agent-harbor-task-prompt-limit 25
  "Maximum number of completion candidates requested from `ah'."
  :type 'integer
  :group 'agent-harbor-task-prompt)

;; Autoloaded because the `auto-mode-alist' entry below is, and an autoload
;; form is copied verbatim into the generated autoloads file and evaluated
;; before this file is loaded.  Without this cookie that entry raised
;; "Error loading autoloads: (void-variable
;; agent-harbor-task-prompt-file-regexp)" on every start of a session where the
;; package was installed, because the variable did not exist yet.  Autoloading
;; the definition keeps one copy of the regexp instead of duplicating the
;; literal into the alist entry.
;;;###autoload
(defconst agent-harbor-task-prompt-file-regexp
  (concat "\\(?:\\`\\|/\\)ah-"
          "\\(?:\\(task\\)-prompt-[0-9]+-[^/]+\\.ahtask"
          "\\|\\(plan\\)-prompt-[0-9]+-[^/]+\\.ahplan\\)\\'")
  "Regexp matching an Agent Harbor prompt scratch file.

The prefix and the extension are deliberately tied together in one
alternation rather than matched independently: the extension alone would
also claim a user's own `notes.ahtask', the prefix alone would claim an
unrelated file, and a mismatched pair like `ah-task-prompt-1-x.ahplan' is
not a name `ah' can produce.  Group 1 matches a task prompt and group 2 a
plan prompt.  Mirrors `ah_core::editor::classify_prompt_path'.")

(defconst agent-harbor-task-prompt-triggers '(?/ ?~ ?@)
  "Trigger characters that open a completion menu.

Mirrors `ah_tui::view_model::autocomplete::Trigger', minus the backtick
command palette, which is a dashboard-only surface.")

(defvar-local agent-harbor-task-prompt--last-request nil
  "Cons of (KEY . RESPONSE) caching the most recent `ah autocomplete' answer.
KEY is a cons of the queried line and cursor column.")

(defvar-local agent-harbor-task-prompt--last-error nil
  "Human-readable description of the last completion failure, or nil.")

;;; Prompt-file recognition

(defun agent-harbor-task-prompt-file-p (&optional file)
  "Return non-nil when FILE is an Agent Harbor prompt scratch file.
FILE defaults to the current buffer's file name."
  (let ((file (or file buffer-file-name)))
    (and file (string-match-p agent-harbor-task-prompt-file-regexp file))))

(defun agent-harbor-task-prompt-kind (&optional file)
  "Return `task' or `plan' for FILE, or nil when FILE is not a prompt file."
  (let ((file (or file buffer-file-name)))
    (when (and file (string-match agent-harbor-task-prompt-file-regexp file))
      (if (match-string 1 file) 'task 'plan))))

(defun agent-harbor-task-prompt-workspace ()
  "Return the directory that `@' completions resolve against.

`ah' publishes it as AH_PROMPT_WORKSPACE, because the scratch file itself
lives in the temp directory and so `default-directory' is useless here.
The frame environment is consulted first so this also works under
`emacsclient', which forwards the launching environment per-frame."
  (or (and (frame-parameter nil 'environment)
           (getenv "AH_PROMPT_WORKSPACE" (selected-frame)))
      (getenv "AH_PROMPT_WORKSPACE")
      default-directory))

;;; Completion backend

(defun agent-harbor-task-prompt--line ()
  "Return the current line's text up to and including point's line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun agent-harbor-task-prompt--cursor-offset ()
  "Return point's offset within the current line, in bytes.

`ah autocomplete' speaks byte offsets because that is what Rust string
slicing uses; Emacs columns are character counts, so convert."
  (string-bytes
   (buffer-substring-no-properties (line-beginning-position) (point))))

(defun agent-harbor-task-prompt--trigger-at-point-p ()
  "Return non-nil when the token before point starts with a trigger character.

This is a cheap local pre-filter so that typing prose does not spawn a
subprocess on every keystroke.  It deliberately does not decide the
replacement span: `ah' remains the authority for that."
  (save-excursion
    (let ((limit (line-beginning-position)))
      (skip-chars-backward "^ \t\"'" limit)
      (and (< (point) (line-end-position))
           (memq (char-after) agent-harbor-task-prompt-triggers)
           ;; Must be at a token boundary: start of line, or preceded by
           ;; whitespace or a quote.
           (or (= (point) limit)
               (memq (char-before) '(?\s ?\t ?\" ?')))))))

(defun agent-harbor-task-prompt--run (line cursor)
  "Run `ah autocomplete' for LINE at byte offset CURSOR.
Return the parsed JSON object, or nil on failure.

The failure description is recorded in the *prompt* buffer, not in the
scratch output buffer: `agent-harbor-task-prompt--last-error' is
buffer-local, so it must be assigned outside `with-current-buffer'."
  (let* ((default-directory (file-name-as-directory
                             (agent-harbor-task-prompt-workspace)))
         (buffer (generate-new-buffer " *agent-harbor-autocomplete*"))
         (failure nil)
         (parsed nil))
    (unwind-protect
        (condition-case err
            (let ((status (call-process
                           agent-harbor-task-prompt-ah-command nil buffer nil
                           "autocomplete" "--json"
                           "--line" line
                           "--cursor" (number-to-string cursor)
                           "--limit" (number-to-string
                                      agent-harbor-task-prompt-limit))))
              (if (and (integerp status) (zerop status))
                  (setq parsed (with-current-buffer buffer
                                 (goto-char (point-min))
                                 (json-parse-buffer :object-type 'alist
                                                    :array-type 'list
                                                    :null-object nil
                                                    :false-object nil)))
                (setq failure
                      (format "ah autocomplete exited with %s: %s"
                              status
                              (string-trim (with-current-buffer buffer
                                             (buffer-string)))))))
          (error (setq failure (error-message-string err))))
      (kill-buffer buffer))
    (setq agent-harbor-task-prompt--last-error failure)
    parsed))

(defun agent-harbor-task-prompt--response ()
  "Return the `ah autocomplete' response for point, using a one-entry cache.

`completion-at-point-functions' is called repeatedly for a single
completion attempt (and again by `company'/`corfu' UIs), so the cache
keeps that to one subprocess per cursor position."
  (let* ((line (agent-harbor-task-prompt--line))
         (cursor (agent-harbor-task-prompt--cursor-offset))
         (key (cons line cursor)))
    (if (equal key (car agent-harbor-task-prompt--last-request))
        (cdr agent-harbor-task-prompt--last-request)
      (let ((response (agent-harbor-task-prompt--run line cursor)))
        (setq agent-harbor-task-prompt--last-request (cons key response))
        response))))

(defun agent-harbor-task-prompt--byte-to-position (byte-offset)
  "Return the buffer position BYTE-OFFSET bytes into the current line."
  (let ((start (line-beginning-position)))
    (if (= byte-offset (string-bytes
                        (buffer-substring-no-properties start (point))))
        (point)
      ;; Walk forward one character at a time; prompt lines are short and this
      ;; avoids assuming the line is all-ASCII.
      (save-excursion
        (goto-char start)
        (let ((seen 0))
          (while (and (< seen byte-offset) (not (eolp)))
            (setq seen (+ seen (string-bytes (string (char-after)))))
            (forward-char 1))
          (point))))))

(defun agent-harbor-task-prompt-candidates ()
  "Return (START END CANDIDATES KIND-ALIST) for point, or nil.

START and END delimit the token `ah' says a candidate replaces, including
the trigger character.  CANDIDATES are the replacement strings."
  (when-let* ((response (agent-harbor-task-prompt--response))
              (trigger (alist-get 'trigger response))
              (start-byte (alist-get 'tokenStart response))
              (end-byte (alist-get 'tokenEnd response))
              (items (alist-get 'items response)))
    (let ((candidates nil)
          (kinds nil))
      (dolist (item items)
        (let ((replacement (alist-get 'replacement item))
              (kind (alist-get 'kind item)))
          (when replacement
            (push replacement candidates)
            (push (cons replacement kind) kinds))))
      (when candidates
        (list (agent-harbor-task-prompt--byte-to-position start-byte)
              (agent-harbor-task-prompt--byte-to-position end-byte)
              (nreverse candidates)
              (nreverse kinds))))))

;;; A pass-through completion style
;;
;; `ah' has already matched and ranked; re-filtering with `basic' would throw
;; every fuzzy hit away.  These two functions implement the
;; `completion-styles-alist' contract for "accept whatever the table returns".

(defun agent-harbor-task-prompt--all-completions (_string table pred _point)
  "Return every completion in TABLE, ignoring STRING.
The trailing base size of 0 tells Emacs that all of STRING is replaced."
  (let ((all (all-completions "" table pred)))
    (when all
      (nconc all 0))))

(defun agent-harbor-task-prompt--try-completion (string table pred point)
  "Insert a unique candidate; otherwise leave STRING alone and show the menu.

Returning STRING unchanged for a sole match would be wrong: Emacs would
pop up a one-entry `*Completions*' buffer instead of completing, which is
not what any other completion style does."
  (let ((all (agent-harbor-task-prompt--all-completions string table pred point)))
    (cond
     ((null all) nil)
     ;; `--all-completions' appends the base size, so a single candidate is a
     ;; list whose cdr is that number rather than another cons.
     ((atom (cdr all)) (cons (car all) (length (car all))))
     (t (cons string point)))))

(add-to-list 'completion-styles-alist
             '(agent-harbor-task-prompt
               agent-harbor-task-prompt--try-completion
               agent-harbor-task-prompt--all-completions
               "Pass-through style for candidates already ranked by `ah'."))

(defun agent-harbor-task-prompt-completion-at-point ()
  "`completion-at-point-functions' entry for Agent Harbor prompt buffers."
  (when (agent-harbor-task-prompt--trigger-at-point-p)
    (when-let* ((found (agent-harbor-task-prompt-candidates)))
      (cl-destructuring-bind (start end candidates kinds) found
        (list start end
              (lambda (string pred action)
                (if (eq action 'metadata)
                    '(metadata (category . agent-harbor-task-prompt))
                  (complete-with-action action candidates string pred)))
              :exclusive 'no
              :annotation-function
              (lambda (candidate)
                (when-let* ((kind (cdr (assoc candidate kinds))))
                  (concat " " kind))))))))

(defun agent-harbor-task-prompt-complete ()
  "Complete the trigger token at point, or indent when none is active."
  (interactive)
  (if (agent-harbor-task-prompt--trigger-at-point-p)
      (or (completion-at-point)
          (when agent-harbor-task-prompt--last-error
            (message "Agent Harbor completion unavailable: %s"
                     agent-harbor-task-prompt--last-error)))
    (indent-for-tab-command)))

;;; Finishing and aborting

(defun agent-harbor-task-prompt--hand-back ()
  "Return control to the `ah' process that launched this editor."
  (cond
   ((and (boundp 'server-buffer-clients) server-buffer-clients)
    (server-edit))
   (t (save-buffers-kill-terminal))))

(defun agent-harbor-task-prompt-finish ()
  "Save the prompt and hand control back to `ah'."
  (interactive)
  (save-buffer)
  (agent-harbor-task-prompt--hand-back))

(defun agent-harbor-task-prompt-abort ()
  "Abort task creation by saving an empty prompt.

`ah' treats a prompt that is empty after comment stripping as an abort,
so emptying the buffer is the documented way to cancel."
  (interactive)
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p "Abort Agent Harbor task creation? "))
    (erase-buffer)
    (save-buffer)
    (agent-harbor-task-prompt--hand-back)))

;;; The mode

(defvar agent-harbor-task-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'agent-harbor-task-prompt-complete)
    (define-key map (kbd "C-c C-c") #'agent-harbor-task-prompt-finish)
    (define-key map (kbd "C-c C-k") #'agent-harbor-task-prompt-abort)
    map)
  "Keymap for `agent-harbor-task-prompt-mode'.")

;;;###autoload
(define-derived-mode agent-harbor-task-prompt-mode text-mode "AH Task Prompt"
  "Major mode for Agent Harbor task and plan prompts opened in `$EDITOR'.

Completion is offered on the same trigger symbols as the TUI dashboard:
\\<agent-harbor-task-prompt-mode-map>
  /  and  ~   filesystem paths
  @           workspace files

\\[agent-harbor-task-prompt-complete] completes the token at point,
\\[agent-harbor-task-prompt-finish] saves and returns to `ah', and
\\[agent-harbor-task-prompt-abort] aborts task creation."
  ;; The hint block `ah' writes is comment-prefixed and stripped on return.
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[ \t]*")
  (setq-local require-final-newline t)
  (setq-local completion-at-point-functions
              (list #'agent-harbor-task-prompt-completion-at-point))
  ;; Scope the pass-through style to this mode's own category so that no other
  ;; completion in the session changes behaviour.
  (setq-local completion-category-overrides
              (cons '(agent-harbor-task-prompt
                      (styles agent-harbor-task-prompt))
                    completion-category-overrides))
  (setq agent-harbor-task-prompt--last-request nil
        agent-harbor-task-prompt--last-error nil))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons agent-harbor-task-prompt-file-regexp
                   #'agent-harbor-task-prompt-mode))

(defun agent-harbor-task-prompt-test-state ()
  "Return structured state for task-prompt page-object tests."
  `((promptFile . ,(if (agent-harbor-task-prompt-file-p) t :false))
    (kind . ,(agent-harbor-task-prompt-kind))
    (mode . ,major-mode)
    (workspace . ,(agent-harbor-task-prompt-workspace))
    (triggerActive . ,(if (agent-harbor-task-prompt--trigger-at-point-p) t :false))
    (lastError . ,agent-harbor-task-prompt--last-error)))

(provide 'agent-harbor-task-prompt)

;;; agent-harbor-task-prompt.el ends here
