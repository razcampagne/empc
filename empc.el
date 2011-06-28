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

(defun empc-response-message (closure msg)
  "Print the response into the minibuffer if EMPC-VERBOSE is not-nil."
  (when empc-verbose
    (message "empc: %s" msg)))

(defun empc-response-parse-line (line)
  "Turn the given line into a cons cell.
Return nil if the line should be ignored."
  (when (string-match "\\([^:\n]+\\):\\s-*\\(.+\\)" line)
    (let ((key (match-string 1 line))
	  (value (match-string 2 line)))
      (if (and key value)
	  (cons (downcase key) value)
	nil))))

(defun empc-response-parse-message (msg)
  "Check the result code and parse the response into an alist."
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

(defun empc-response-parse-status (closure msg)
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
  "Closes connection between empc and mpd."
  (when (and empc-process
	     (processp empc-process)
	     (eq (process-status empc-process) 'open))
    (empc-send "close"))
  (tq-close empc-queue)
  (setq empc-process nil))

(defun empc-send (command &optional fn closure delay)
  "Send COMMAND to the mpd server."
  (empc-ensure-connected)
  (unless (string= (substring command -1) "\n")
    (setq command (concat command "\n")))
  (tq-enqueue empc-queue command empc-response-regexp
	      closure
  	      (if fn
  		  fn
  		'empc-response-message)
  	      (if delay
  		  delay
  		t)))

(defun empc-update-status (&optional closure)
  "Retreive the current status and update EMPC-CURRENT-STATUS."
  (empc-send "status" 'empc-response-parse-status closure))

(defmacro define-simple-command (command)
  "Define a simple command that doesn't require heavy response processing."
  `(defun ,(intern (concat "empc-send-" command)) ()
     ,(concat "Send " command " to the server.")
     (empc-send ,command)))

(defmacro define-toggle-command (command)
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
(defun empc-send-pause (&optional state)
  "Send pause to the server."
  (interactive)
  (if state
      (empc-send (concat "pause " state))
    (empc-update-status '(lambda (plist)
			   (if (eq (plist-get plist 'state) 'play)
			       (empc-send "pause 1")
			     (empc-send "pause 0"))))))

(define-simple-command "play")
(define-simple-command "stop")
(define-simple-command "next")
(define-simple-command "previous")


(define-toggle-command "consume")
(define-toggle-command "random")
(define-toggle-command "repeat")
(define-toggle-command "single")
(define-toggle-command "xfade")
(define-toggle-command "pause")

(provide 'empc)
