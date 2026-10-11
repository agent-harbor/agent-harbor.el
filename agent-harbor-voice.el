;;; agent-harbor-voice.el --- Agent Harbor voice input -*- lexical-binding: t; -*-

;; Copyright 2026 Schelling Point Labs Inc
;; SPDX-License-Identifier: AGPL-3.0-only

;;; Commentary:

;; Voice input support for editor feedback replies.  Recording uses a fixture
;; WAV path in tests, otherwise an external recorder (sox/rec or arecord), then
;; uploads the WAV bytes to the daemon transcription endpoint.

;;; Code:

(require 'subr-x)
(require 'agent-harbor-rest)

(declare-function agent-harbor-reply-submit-text "agent-harbor-reply")

(defgroup agent-harbor-voice nil
  "Agent Harbor voice input."
  :group 'agent-harbor)

(defcustom agent-harbor-voice-mode 'click-to-talk
  "Voice input mode."
  :type '(choice (const disabled) (const click-to-talk) (const hold-to-talk) (const always-on))
  :group 'agent-harbor-voice)

(defcustom agent-harbor-voice-record-seconds 2
  "Default recording duration for click-to-talk voice input."
  :type 'number
  :group 'agent-harbor-voice)

(defcustom agent-harbor-voice-fixture-wav nil
  "Optional fixture WAV file used before invoking external recorders."
  :type '(choice (const nil) file)
  :group 'agent-harbor-voice)

(defvar agent-harbor-voice-last-transcription nil
  "Last daemon transcription response.")

(defvar agent-harbor-voice-always-on-stream-id nil
  "Current always-on voice stream ID.")

(defvar agent-harbor-voice-last-mode nil
  "Last voice mode used for a reply submission.")

(defun agent-harbor-voice--fixture-path ()
  "Return configured fixture WAV path, if any."
  (let ((path (or agent-harbor-voice-fixture-wav
                  (getenv "AH_VOICE_FIXTURE_WAV"))))
    (and path (not (string-empty-p path)) (file-readable-p path) path)))

(defun agent-harbor-voice--recorder-command (output)
  "Return external recorder command writing WAV to OUTPUT."
  (cond
   ((executable-find "sox")
    (list "sox" "-q" "-d" "-r" "16000" "-c" "1" "-b" "16" output "trim" "0"
          (number-to-string agent-harbor-voice-record-seconds)))
   ((executable-find "rec")
    (list "rec" "-q" "-r" "16000" "-c" "1" "-b" "16" output "trim" "0"
          (number-to-string agent-harbor-voice-record-seconds)))
   ((executable-find "arecord")
    (list "arecord" "-q" "-f" "S16_LE" "-r" "16000" "-c" "1" "-d"
          (number-to-string (ceiling agent-harbor-voice-record-seconds)) output))
   (t nil)))

(defun agent-harbor-voice-record-wav ()
  "Return a WAV path from fixture configuration or an external recorder."
  (or (agent-harbor-voice--fixture-path)
      (let* ((output (make-temp-file "agent-harbor-voice-" nil ".wav"))
             (command (agent-harbor-voice--recorder-command output)))
        (unless command
          (signal 'user-error '("No voice recorder found; install sox/rec or arecord")))
        (unless (zerop (apply #'call-process (car command) nil nil nil (cdr command)))
          (signal 'user-error '("Voice recorder failed")))
        output)))

(defun agent-harbor-voice-transcribe-file (path)
  "Upload WAV file PATH and return the daemon transcription response."
  (setq agent-harbor-voice-last-transcription
        (agent-harbor-rest-transcribe-wav-file path))
  agent-harbor-voice-last-transcription)

(defun agent-harbor-voice-transcript-text (response)
  "Return corrected transcript text from RESPONSE."
  (or (alist-get 'correctedTranscript response)
      (alist-get "correctedTranscript" response nil nil #'equal)
      (alist-get 'transcript response)
      (alist-get "transcript" response nil nil #'equal)
      ""))

(defun agent-harbor-voice--stream-id (response)
  "Return stream ID from always-on start RESPONSE."
  (or (alist-get 'streamId response)
      (alist-get "streamId" response nil nil #'equal)))

(defun agent-harbor-voice--chunk-transcription (response)
  "Return first closed-segment transcription from always-on chunk RESPONSE."
  (let ((items (or (alist-get 'transcriptions response)
                   (alist-get "transcriptions" response nil nil #'equal))))
    (car items)))

(defun agent-harbor-voice-always-on-start ()
  "Start an always-on voice stream through the daemon."
  (interactive)
  (let* ((response (agent-harbor-rest-voice-always-on-start
                    '((threshold . 0.02) (silenceMs . 25))))
         (stream-id (agent-harbor-voice--stream-id response)))
    (unless stream-id
      (signal 'user-error '("Always-on voice stream did not return a stream ID")))
    (setq agent-harbor-voice-always-on-stream-id stream-id)
    response))

(defun agent-harbor-voice-always-on-stop ()
  "Stop the current always-on voice stream."
  (interactive)
  (unless agent-harbor-voice-always-on-stream-id
    (signal 'user-error '("No always-on voice stream is active")))
  (prog1
      (agent-harbor-rest-voice-always-on-stop agent-harbor-voice-always-on-stream-id)
    (setq agent-harbor-voice-always-on-stream-id nil)))

(defun agent-harbor-voice-always-on-finalize ()
  "Record one WAV chunk, close VAD, and return its transcription."
  (interactive)
  (let* ((stream-id (or agent-harbor-voice-always-on-stream-id
                        (agent-harbor-voice--stream-id
                         (agent-harbor-voice-always-on-start))))
         (wav (agent-harbor-voice-record-wav))
         (chunk (unwind-protect
                    (agent-harbor-rest-voice-always-on-chunk stream-id wav 0 t)
                  (agent-harbor-voice-always-on-stop)))
         (transcription (agent-harbor-voice--chunk-transcription chunk)))
    (setq agent-harbor-voice-last-transcription
          (or transcription chunk))
    agent-harbor-voice-last-transcription))

(defun agent-harbor-reply-voice-submit ()
  "Record voice, transcribe through the daemon, and submit to current feedback."
  (interactive)
  (when (eq agent-harbor-voice-mode 'disabled)
    (signal 'user-error '("Agent Harbor voice input is disabled")))
  (setq agent-harbor-voice-last-mode agent-harbor-voice-mode)
  (let* ((response (if (eq agent-harbor-voice-mode 'always-on)
                       (agent-harbor-voice-always-on-finalize)
                     (agent-harbor-voice-transcribe-file
                      (agent-harbor-voice-record-wav))))
         (text (string-trim (agent-harbor-voice-transcript-text response))))
    (when (string-empty-p text)
      (signal 'user-error '("Voice transcription returned an empty transcript")))
    (agent-harbor-reply-submit-text text)))

(provide 'agent-harbor-voice)

;;; agent-harbor-voice.el ends here
