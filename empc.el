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

(defcustom empc-default-crossfade 5
  "The default crossfade to apply."
  :type 'integer
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
(defvar empc-idle-state nil)
(defvar empc-available-commands nil)
(defvar empc-last-crossfade nil)
(defvar empc-current-status nil)
(defconst empc-response-regexp
  "^\\(OK\\( MPD \\)?\\|ACK \\[\\([0-9]+\\)@[0-9]+\\] \\(.+\\)\\)\n+\\'"
  "Regexp that matches the valid status strings that MusicPD can
return at the end of a request.")

(defun empc-process-sentinel (proc event)
  "Process sentinel for empc-process."
  (let ((status (process-status proc)))
    (cond ((eq status 'closed)
	   (when empc-verbose
	     (message "empc: connection closed"))))))

(defun empc-echo-minibuffer (msg)
  "Print the response into the minibuffer if EMPC-VERBOSE is non nil."
  (when empc-verbose
    (message "empc: %s" (if (string= (substring msg -1) "\n")
			    (substring msg 0 -1)
			  msg))))

(defun empc-echo-notify (msg)
  "Notify the response using notification system if available, in echo area if not."
  (let ((msg (if (string= (substring msg -1) "\n")
		 (substring msg 0 -1)
	       msg)))
    (if (eq window-system 'x)
	(shell-command (concat "notify-send " " \"Music Player Daemon\" \"" msg "\"") nil nil)
      (message msg))))

(defun empc-response-parse-line (line)
  "Turn the given line into a cons cell.
Return nil if the line is not of the form \"key: value\"."
  (if (string-match "\\([^:\n]+\\):\\s-*\\(.+\\)" line)
      (cons (downcase (match-string 1 line))
	    (match-string 2 line))
    nil))

(defun empc-response-parse-message (msg)
  "Check the result code and parse the response into an alist.
If the command resulted in an error, return a plist of the
form '('error (error-code . error-message))."
  (save-match-data
    (let* ((data (split-string msg "\n" t))
	   (status (car (last data))))
      (when (and (stringp (car data))
		 (string-match "^OK\\( MPD \\)?" (car data)))
	(setq data (cdr data)))
      (if (and (stringp status)
	       (string-match "^ACK \\[\\([0-9]+\\)@[0-9]+\\] \\(.+\\)" status))
	  (cons 'error (cons (match-string 1 status)
			     (match-string 2 status)))
	(let ((result nil))
	  (dolist (line data)
	    (let ((cell (empc-response-parse-line line)))
	      (when cell
		(setq result (cons cell result)))))
	  result)))))

(defun empc-response-get-status (data)
  "Arrange data into a plist and store it into EMPC-CURRENT-STATUS."
  (setq empc-current-status nil)
  (dolist (cell data)
    (let ((attr (car cell)))
      (setq empc-current-status
	    (cond
	     ((member attr '("volume" "repeat" "random" "single" "consume" "playlist"
			     "playlistlength" "song" "songid" "nextsong" "nextsongid"
			     "bitrate" "xfade" "mixrampdb" "mixrampdelay" "updating_db"))
	      (plist-put empc-current-status (intern attr) (string-to-number (cdr cell))))
	     ((and (string= attr "state") (member (cdr cell) '("play" "pause" "stop")))
	      (plist-put empc-current-status 'state (intern (cdr cell))))
	     ((and (string= attr "time") (string-match "^\\([0-9]*\\):\\([0-9]*\\)$" (cdr cell)))
	      (plist-put empc-current-status 'time-elapsed (string-to-number (match-string 1 (cdr cell))))
	      (plist-put empc-current-status 'time-total (string-to-number (match-string 2 (cdr cell)))))
	     (t (plist-put empc-current-status (intern attr) (cdr cell))))))))

(defun empc-response-idle (data)
  "React from idle interruption."
  (setq empc-idle-state nil)
  (dolist (cell data)
    (when (string= (car cell) "changed")
      (let ((changed (cdr cell)))
	(cond
	 ((string= changed "player")
	  (empc-send "status" 'empc-response-get-status)
	  (empc-send "currentsong"))
	 ((string= changed "options")
	  (empc-send "status" 'empc-response-get-status)))))))

(defun empc-handle-response (closure msg)
  "Retrieve the response from the server.
Check the error code and process it using CLOSURE."
  (let ((data (empc-response-parse-message msg)))
    (if (eq (car data) 'error)
	(empc-echo-error (cdr data))
      (if closure
	  (funcall closure data))))
  (empc-maybe-enter-idle-state))

(defun empc-initialize ()
  "Initialize the client after connection.
Send the password or retrieve available commands."
  (when empc-server-password
    (empc-send (concat "password " empc-server-password)))
  (empc-send "commands" '(lambda (data)
			   (setq empc-available-commands nil)
			   (dolist (cell data)
			     (setq empc-available-commands (cons (cdr cell) empc-available-commands)))))
  (setq empc-idle-state nil
	empc-last-crossfade nil))

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
    (setq empc-queue (tq-create empc-process))
    (empc-initialize)))

(defun empc-close-connection ()
  "Close connection between empc and mpd."
  (interactive)
  (when (and empc-process
	     (processp empc-process)
	     (eq (process-status empc-process) 'open))
    (empc-leave-idle-state)
    (empc-send "close"))
  (when empc-queue
    (tq-close empc-queue))
  (setq empc-queue nil
	empc-process nil
	empc-idle-state nil
	empc-last-crossfade nil
	empc-available-commands nil))

(defun empc-maybe-enter-idle-state ()
  "If not already in idle state and there is no other commands pending,
enter idle state to accept notifications from the server."
  (unless (or empc-idle-state
	      (cdr (tq-queue empc-queue)))
    (tq-enqueue empc-queue "idle\n" empc-response-regexp
		'empc-response-idle 'empc-handle-response t)
    (setq empc-idle-state t)))

(defun empc-leave-idle-state ()
  "If in idle state, regain control."
  (when empc-idle-state
    (process-send-string empc-process "noidle\n")
    (setq empc-idle-state nil)))

(defun empc-send (command &optional closure)
  "Send COMMAND to the mpd server.
Parse the response using the function FN which will then call CLOSURE."
  (empc-ensure-connected)
  (empc-leave-idle-state)
  (unless (string= (substring command -1) "\n")
    (setq command (concat command "\n")))
  (tq-enqueue empc-queue command empc-response-regexp
	      closure 'empc-handle-response t))

(defun empc-stream-start (status)
  "Start the stream process if the command to mpd returned successfully.
If the stream process is killed for whatever the reason, pause mpd if possible."
  (when (and empc-stream-url empc-stream-program
	     (eq (plist-get status 'error) nil))
    (set-process-sentinel (start-process "empc-stream" nil empc-stream-program empc-stream-url)
			  '(lambda (proc event)
			     (when (and (eq (process-status proc) 'exit)
					empc-process
					(processp empc-process)
					(eq (process-status empc-process) 'open))
			       (empc-toggle-pause 1))))))

(defmacro empc-define-simple-command (command)
  "Define a simple command that doesn't require heavy response processing."
  `(defun ,(intern (concat "empc-send-" command)) (&optional arg)
     ,(concat "Send " command " to the server.")
     (interactive)
     (empc-leave-idle-state)
     (if arg
	 (empc-send (concat ,(concat command " ") arg))
       (empc-send ,command))))

(defmacro empc-define-toggle-command (command &optional state-name attr &rest body)
  "Define a command that toggle a state."
  `(defun ,(intern (concat "empc-toggle-" command)) (&optional state)
     ,(concat "Toggle " command ".")
     (interactive)
     (empc-leave-idle-state)
     (if state
	 (empc-send (concat ,(concat command " ") (int-to-string state)))
       (let ((,(if attr attr
		 (intern command))
	      (plist-get empc-current-status (quote ,(intern (if state-name
								 state-name
							       command))))))
	 ,(if body
	      `(progn ,@body)
	    `(empc-send (concat ,command (if (= ,(if attr attr
						   (intern command)) 1) " 0" " 1"))))))))

;; Controlling playback
(empc-define-simple-command "next")
(empc-define-toggle-command "pause" "state" state
			    (cond
			     ((eq state 'play)
			      (empc-send "pause 1"))
			     ((eq state 'pause)
			      (empc-send "pause 0" 'empc-stream-start))
			     (t (empc-send "play" 'empc-stream-start))))
(empc-define-simple-command "play")
(empc-define-simple-command "playid")
(empc-define-simple-command "previous")
(empc-define-simple-command "stop")

;; Playback options
(empc-define-toggle-command "consume")
(empc-define-toggle-command "crossfade" "xfade" xfade
			    (if (= xfade 0)
				(empc-send (concat "crossfade "
						   (int-to-string (if empc-last-crossfade
								      empc-last-crossfade
								    empc-default-crossfade))))
			      (progn
				(setq empc-last-crossfade xfade)
				(empc-send "crossfade 0"))))
(empc-define-toggle-command "random")
(empc-define-toggle-command "repeat")
(empc-define-simple-command "setvol")
(empc-define-toggle-command "single")

;; The current playlist
(empc-define-simple-command "clear")

(provide 'empc)
