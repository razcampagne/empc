(require 'tq)

(defgroup empc nil
  "Customize group for empc."
  :group 'external
  :group 'applications
  :group 'multimedia)

(defcustom empc-server-host (or (getenv "MPD_HOST") "localhost")
  "The MPD server that we should connect to."
  :type 'string
  :group 'empc)

(defcustom empc-server-port (or (getenv "MPD_PORT") 6600)
  "The port of the MPD server."
  :type 'integer
  :group 'empc)

(defcustom empc-server-password nil
  "The password for the MPD server."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'empc)

(defcustom empc-stream-url nil
  "The url of the stream to play when asking MPD to start."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'empc)

(defcustom empc-stream-program "mplayer"
  "The program to launch to play the stream."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'empc)

(defgroup empc-debug nil
  "Customize group for debugging empc."
  :group 'empc)

(defcustom empc-verbose nil
  "Whether to provide notifications for server connection events and errors."
  :type 'boolean
  :group 'empc-debug)

(defcustom empc-buffer-name nil
  "The name of the buffer for server responses."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'empc-debug)

(defvar empc-process nil)
(defvar empc-queue nil)
(defvar empc-status-regexp
  "^\\(OK\\( MPD \\)?\\|ACK \\[\\([0-9]+\\)@[0-9]+\\] \\(.+\\)\\)\n+\\'"
  "Regexp that matches the valid status strings that MusicPD can
return at the end of a request.")

(defun empc-process-sentinel (proc event)
  "Process sentinel for empc-process."
  (let ((status (process-status proc)))
    (cond ((eq status 'closed)
	   (when empc-verbose
	     (message "empc: connection closed"))))))

(defun empc-response-message (closure msg)
  "Print the response into the minibuffer if EMPC-VERBOSE is not-nil."
  (when empc-verbose
    (message "empc: %s" msg)))

(defun empc-ensure-connected ()
  "Make sure empc is connected and ready to talk to mpd."
  (unless (and empc-process
	       (processp empc-process)
	       (eq (process-status empc-process) 'open))
    (setq empc-process (open-network-stream "empc" empc-buffer-name empc-server-host empc-server-port))
    (set-process-sentinel empc-process 'empc-process-sentinel)
    (if (fboundp 'set-process-query-on-exit-flag)
	(set-process-query-on-exit-flag empc-process nil)
      (process-kill-without-query empc-process))
    (set-process-coding-system empc-process 'utf-8-unix 'utf-8-unix)
    (setq empc-queue (tq-create empc-process))))

(defun empc-close-connection ()
  "Closes connection between empc and mpd."
  (when (and empc-process
	     (processp empc-process)
	     (eq (process-status empc-process) 'open))
    (empc-send "close")))

(defun empc-send (command)
  "Send COMMAND to the mpd server."
  (empc-ensure-connected)
  (unless (string= (substring command -1) "\n")
    (setq command (concat command "\n")))
  (tq-enqueue empc-queue command empc-status-regexp nil 'empc-response-message t))

(provide 'empc)
