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
(defvar empc-response-regexp
  "^\\(OK\\( MPD \\)?\\|ACK \\[\\([0-9]+\\)@[0-9]+\\] \\(.+\\)\\)\n+\\'"
  "Regexp that matches the valid status strings that MusicPD can
return at the end of a request.")

(defun empc-process-sentinel (proc event)
  "Process sentinel for empc-process."
  (let ((status (process-status proc)))
    (cond ((eq status 'closed)
	   (when empc-verbose
	     (message "empc: connection closed"))))))

(defun empc-echo-response (msg)
  "Print the response into the minibuffer if EMPC-VERBOSE is non nil."
  (when empc-verbose
    (message "empc: %s" (if (string= (substring msg -1) "\n")
			    (substring msg 0 -1)
			  msg))))

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
	   (status (last data)))
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

(defun empc-response-generic (closure msg)
  "Parse the server response, arrange it into an alist and call CLOSURE on it."
  (empc-echo-response msg)
  (when closure
    (funcall closure (empc-response-parse-message msg))))

(defun empc-response-parse-status (closure msg)
  "Parse the status response, arrange it into a plist and call CLOSURE on it."
  (empc-echo-response msg)
  (setplist 'empc-status-plist nil)
  (dolist (cell (empc-response-parse-message msg))
    (let ((attr (car cell)))
      (cond
       ((member attr '("volume" "repeat" "random" "single" "consume" "playlist"
		     "playlistlength" "song" "songid" "nextsong" "nextsongid"
		     "bitrate" "xfade" "mixrampdb" "mixrampdelay" "updating_db"))
	(put 'empc-status-plist (intern attr) (string-to-number (cdr cell))))
       ((and (string= attr "state") (member (cdr cell) '("play" "pause" "stop")))
	(put 'empc-status-plist 'state (intern (cdr cell))))
       ((and (string= attr "time") (string-match "^\\([0-9]*\\):\\([0-9]*\\)$" (cdr cell)))
	(put 'empc-status-plist 'time-elapsed (string-to-number (match-string 1 (cdr cell))))
	(put 'empc-status-plist 'time-total (string-to-number (match-string 2 (cdr cell)))))
       (t (put 'empc-status-plist (intern attr) (cdr cell))))))
  (when closure
    (funcall closure (symbol-plist 'empc-status-plist))))

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
    (when empc-server-password
      (empc-send (concat "password " empc-server-password)))))

(defun empc-close-connection ()
  "Close connection between empc and mpd."
  (when (and empc-process
	     (processp empc-process)
	     (eq (process-status empc-process) 'open))
    (empc-send "close"))
  (tq-close empc-queue)
  (setq empc-process nil))

(defun empc-send (command &optional closure fn delay)
  "Send COMMAND to the mpd server.
Parse the response using the function FN which will then call CLOSURE."
  (empc-ensure-connected)
  (unless (string= (substring command -1) "\n")
    (setq command (concat command "\n")))
  (tq-enqueue empc-queue command empc-response-regexp
	      closure (if fn fn 'empc-response-generic)
  	      (if delay delay t)))

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

(defmacro empc-with-updated-status (status &rest body)
  "Update the status and execute the forms in BODY."
  (empc-send "status" `(lambda (status) ,@body) 'empc-response-parse-status))

(defmacro empc-define-simple-command (command)
  "Define a simple command that doesn't require heavy response processing."
  `(defun ,(intern (concat "empc-send-" command)) ()
     ,(concat "Send " command " to the server.")
     (empc-send ,command)))

(defmacro empc-define-toggle-command (command)
  "Define a command that toggle a state."
  `(defun ,(intern (concat "empc-send-" command)) (&optional state)
     ,(concat "Send " command " to the server.")
     (if state
	 ,(if (member command '("consume" "random" "repeat" "single" "pause"))
	      `(empc-send (concat ,command " " (int-to-string state)))
	    `(empc-send (concat ,command " " state)))
       (let ((status (plist-get (empc-update-status) ,(intern command))))
	 ,(if (member command '("consume" "random" "repeat" "single" "pause"))
	      `(empc-send (concat ,command " " (if (= status 1) "0" "1")))
	    (when (string= command "xfade")
		`(empc-send (concat ,command " " (if (= status 0)
						     (int-to-string empc-default-crossfade)
						   (progn
						     (setq empc-default-crossfade status)
						     "0"))))))))))

(defun empc-toggle-pause (&optional state)
  "Toggle pause."
  (interactive)
  (if state
      (empc-send (concat "pause " (int-to-string state)))
    (empc-with-updated-status status
			      (if (eq (plist-get status 'state) 'play)
				  (empc-send "pause 1")
				(empc-send "pause 0" 'empc-stream-start)))))

(empc-define-simple-command "play")
(empc-define-simple-command "stop")
(empc-define-simple-command "next")
(empc-define-simple-command "previous")

;; (empc-define-toggle-command "consume")
;; (empc-define-toggle-command "random")
;; (empc-define-toggle-command "repeat")
;; (empc-define-toggle-command "single")
;; (empc-define-toggle-command "xfade")
;; (empc-define-toggle-command "pause")

(provide 'empc)
