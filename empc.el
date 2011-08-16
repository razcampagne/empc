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
(defvar empc-stream-process nil)
(defvar empc-queue nil)
(defvar empc-idle-state nil)
(defvar empc-available-commands nil)
(defvar empc-last-crossfade nil)
(defvar empc-current-status nil)
(defvar empc-current-playlist nil)
(defvar empc-current-playlist-songs nil)
(defconst empc-response-regexp
  "^\\(OK\\( MPD \\)?\\|ACK \\[\\([0-9]+\\)@[0-9]+\\] \\(.+\\)\\)\n+\\'"
  "Regexp that matches the valid status strings that MusicPD can
return at the end of a request.")

(defun empc-process-sentinel (proc event)
  "Process sentinel for `empc-process'."
  (let ((status (process-status proc)))
    (cond ((eq status 'closed)
	   (when empc-verbose
	     (message "empc: connection closed"))))))

(defun empc-stream-process-sentinel (proc event)
  "Process sentinel for `empc-stream-process'."
  (when (and (eq (process-status proc) 'exit)
	     empc-process
	     (processp empc-process)
	     (eq (process-status empc-process) 'open)
	     (eq (plist-get empc-current-status :state) 'play))
    (empc-toggle-pause 1))
  (setq empc-stream-process nil))

(defun empc-echo-minibuffer (msg)
  "Print the response into the minibuffer if EMPC-VERBOSE is non nil."
  (when empc-verbose
    (message "empc: %s" (if (string= (substring msg -1) "\n")
			    (substring msg 0 -1)
			  msg))))

(defun empc-echo-notify (msg)
  "Notify MSG using notification system if available, in echo area if not."
  (when (eq window-system 'x)
    (start-process "empc-notify" nil "notify-send" "Music Player Daemon" msg))
  (message (concat "empc: " msg)))

(defun empc-echo-song (&optional song)
  "Notify SONG."
  (interactive)
  (unless song
    (setq song (gethash (plist-get empc-current-status :songid) empc-current-playlist-songs)))
  (empc-echo-notify (concat "[" (int-to-string (+ (plist-get song :pos) 1))
			    "/" (int-to-string (plist-get empc-current-status :playlistlength)) "] "
			    (if (and (plist-get song :artist) (plist-get song :title))
				(concat (plist-get song :artist) " - " (plist-get song :title))
			      (plist-get song :file)))))

(defun empc-make-modeline ()
  "Create the string to insert into the modeline."
  )

(defun empc-response-parse-line (line)
  "Turn the given line into a cons cell.
If the line is not of the form \"key: value\",
check if it matches \"list_OK\"."
  (if (string-match "\\([^:\n]+\\):\\s-*\\(.+\\)" line)
      (cons (downcase (match-string 1 line))
	    (match-string 2 line))
    (string= line "list_OK")))

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
	(let ((result nil)
	      (response nil))
	  (dolist (line data (if result (reverse result) response))
	    (let ((cell (empc-response-parse-line line)))
	      (when cell
		(if (consp cell)
		    (setq response (cons cell response))
		  (setq result (cons response result)
			response nil))))))))))

(defun empc-response-get-commands (data)
  "Parse DATA to get the available commands."
  (setq empc-available-commands nil)
  (dolist (cell data)
    (setq empc-available-commands (cons (cdr cell) empc-available-commands))))

(defun empc-status-on/off-stringify (status key)
  "Return `on' or `off' if KEY is active or inactive in STATUS."
  (if (= (plist-get status key) 0) "off" "on"))

(defun empc-response-get-status (data)
  "Parse DATA to get a diff with `empc-current-status'.

According to what is in the diff, several actions can be performed:
	if :state or :songid is changed, report it to the user,
	if :state is set to 'play, start the streaming process."
  (let ((status-diff (empc-diff-status data))
	(notify nil))
    (when (plist-get status-diff :songid)
      (setq notify '(lambda () (when empc-current-playlist-songs
				 (empc-echo-song (gethash (plist-get status-diff :songid)
							  empc-current-playlist-songs))))))
    (when (plist-get status-diff :state)
      (if (eq (plist-get status-diff :state) 'play)
	  (progn
	    (unless notify
	      (setq notify '(lambda () (when empc-current-playlist-songs
					 (empc-echo-song)))))
	    (empc-stream-start))
	(setq notify '(lambda () (empc-echo-notify (symbol-name (plist-get status-diff :state)))))))
    (when (or (plist-member status-diff :repeat) (plist-member status-diff :random)
	      (plist-member status-diff :single) (plist-member status-diff :consume)
	      (plist-member status-diff :xfade))
      (setq notify '(lambda () (empc-echo-notify (format "repeat: %s, random: %s, single: %s, consume: %s, crossfade: %s"
							 (empc-status-on/off-stringify empc-current-status :repeat)
							 (empc-status-on/off-stringify empc-current-status :random)
							 (empc-status-on/off-stringify empc-current-status :single)
							 (empc-status-on/off-stringify empc-current-status :consume)
							 (empc-status-on/off-stringify empc-current-status :xfade))))))
    (when notify
      (funcall notify))))

(defun empc-parse-status-attr (attr value)
  "Parse a single attribute from status."
  (cond
   ((eq value nil) nil)
   ((member attr '(:volume :repeat :random :single :consume :playlist :playlistlength
			   :song :songid :nextsong :nextsongid :bitrate :xfade :mixrampdb
			   :mixrampdelay :updating_db))
    (string-to-number value))
   ((and (eq attr :state) (member value '("play" "pause" "stop")))
    (intern value))
   ((and (eq attr :time) (string-match "^\\([0-9]*\\):\\([0-9]*\\)$" value))
    (cons (string-to-number (match-string 1 value))
	  (string-to-number (match-string 2 value))))
   (t value)))

(defun empc-diff-status (data)
  "Get the diff from `empc-current-status' and server response."
  (let ((status-diff nil)
	(attributes '(:volume :repeat :random :single :consume :playlist :playlistlength :state
			      :song :songid :nextsong :nextsongid :time :elapsed :bitrate :xfade
			      :mixrampdb :mixrampdelay :audio :updating_db :error)))
    (dolist (attr attributes status-diff)
      (let ((value (empc-parse-status-attr attr (cdr (assoc (substring (symbol-name attr) 1) data)))))
	(unless (equal (plist-get empc-current-status attr) value)
	  (setq status-diff (plist-put status-diff attr value))
	  (setq empc-current-status (plist-put empc-current-status attr value)))))))

(defun empc-response-get-playlist (data)
  "Parse information regarding songs in current playlist and arrange it into a
hash table `empc-current-playlist-songs'sorted by songid.
songs order is kept into an avector `empc-current-playlist'."
  (setq empc-current-playlist-songs (make-hash-table :rehash-threshold 1.0 :size (plist-get empc-current-status :playlistlength)))
  (setq empc-current-playlist (make-vector (plist-get empc-current-status :playlistlength) nil))
  (let ((song nil)
	(index (- (length empc-current-playlist) 1)))
    (dolist (cell data)
      (let ((field (intern (concat ":" (car cell)))))
	(when (and (eq field :id) song)
	  (puthash (plist-get song :id) song empc-current-playlist-songs)
	  (aset empc-current-playlist index (plist-get song :id))
	  (setq song nil)
	  (decf index))
	(cond
	 ((member field '(:time :track :date :pos :id))
	  (setq song (plist-put song field (string-to-number (cdr cell)))))
	 (t (if (plist-get song field)
		(setq song (plist-put song field (concat (plist-get song field) ", " (cdr cell))))
	      (setq song (plist-put song field (cdr cell))))))))
    (when (and song (>= index 0))
      (puthash (plist-get song :id) song empc-current-playlist-songs)
      (aset empc-current-playlist index (plist-get song :id)))))

(defun empc-response-idle (data)
  "React from idle interruption."
  (setq empc-idle-state nil)
  (dolist (cell data)
    (when (string= (car cell) "changed")
      (let ((changed (cdr cell)))
	(cond
	 ((member changed '("player" "options"))
	  (empc-send-status))
	 ((string= changed "playlist")
	  (empc-send-status)
	  (empc-send-playlistinfo)))))))

(defun empc-handle-closure-call (closures data)
  "If CLOSURES is a list of function, call them in turn with DATA
  as parameter."
  (when closures
    (if (and (listp closures) (not (member (car closures) '(quote lambda))))
	(dolist (closure closures (reverse notifications))
	  (funcall closure data))
      (funcall closures data))))

(defun empc-handle-response (closures msg)
  "Retrieve the response from the server.
Check the error code and process it using CLOSURES."
  (let ((data (empc-response-parse-message msg)))
    (if (eq (car data) 'error)
	(empc-echo-error (cdr data))
      (empc-handle-closure-call closures data)))
  (empc-maybe-enter-idle-state))

(defun empc-handle-response-list (closures msg)
  "Retrieve the responses from the server.
Check the error code and process the different responses to the
commands send as command_list."
  (let ((data (empc-response-parse-message msg)))
    (if (eq (car data) 'error)
	(empc-echo-error (cdr data))
      (dolist (closure closures)
	(empc-handle-closure-call closure (car data))
	(setq data (cdr data)))))
  (empc-maybe-enter-idle-state))

(defun empc-initialize ()
  "Initialize the client after connection.
Send the password or retrieve available commands."
  (empc-send-list (when empc-server-password
		    `(,(concat "password " empc-server-password)))
		  '("commands" . empc-response-get-commands)
		  '("status" . empc-response-get-status)
		  '("playlistinfo" . empc-response-get-playlist))
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
    (empc-send-close))
  (when empc-queue
    (tq-close empc-queue))
  (setq empc-process nil
	empc-queue nil
	empc-idle-state nil
	empc-available-commands nil
	empc-last-crossfade nil
	empc-current-status nil
	empc-current-playlist-songs nil
	empc-current-playlist nil))

(defun empc-maybe-enter-idle-state ()
  "If not already in idle state and there is no other commands pending,
enter idle state to accept notifications from the server."
  (unless (or empc-idle-state
	      (cdr (tq-queue empc-queue)))
    (empc-send-idle)
    (setq empc-idle-state t)))

(defun empc-leave-idle-state ()
  "If in idle state, regain control."
  (when empc-idle-state
    (process-send-string empc-process "noidle\n")
    (setq empc-idle-state nil)))

(defun empc-send (command &optional closure handler)
  "Send COMMAND to the mpd server.
CLOSURE will be called on the parsed response."
  (empc-ensure-connected)
  (empc-leave-idle-state)
  (unless (string= (substring command -1) "\n")
    (setq command (concat command "\n")))
  (tq-enqueue empc-queue command empc-response-regexp
	      closure (if handler handler 'empc-handle-response) t))

(defun empc-send-list (&rest commands)
  "Send COMMANDS to the mpd server using command_list.
COMMANDS is a list of cons of the form: '(COMMAND . CLOSURE),
where CLOSURE (may be a list of functions) will be called on the parsed response."
  (let ((command "command_list_ok_begin\n")
	(closures nil))
    (setq closures (dolist (cell commands (reverse closures))
		     (setq command (concat command (car cell) "\n"))
		     (setq closures (cons (cdr cell) closures))))
    (setq command (concat command "command_list_end\n"))
    (empc-send command closures 'empc-handle-response-list)))

(defun empc-stream-start ()
  "Start the stream process if the command to mpd returned successfully.
If the stream process is killed for whatever the reason, pause mpd if possible."
  (when (and (not empc-stream-process)
	     empc-stream-url empc-stream-program)
    (setq empc-stream-process (start-process "empc-stream" nil empc-stream-program empc-stream-url))
    (set-process-sentinel empc-stream-process 'empc-stream-process-sentinel)))

(defmacro with-updated-status (&rest body)
  "Update the status and execute the forms in BODY."
  `(if empc-current-status
       ,@body
     (empc-send "status\n" '(empc-response-get-status (lambda (data) ,@body)))))

(defmacro empc-define-simple-command (command &optional closure)
  "Define a simple command that doesn't need an argument."
  `(defun ,(intern (concat "empc-send-" command)) (&optional arg)
     ,(concat "Send " command " to the server.")
     (interactive)
     (empc-leave-idle-state)
     (if arg
	 (empc-send (concat ,(concat command " ") arg "\n") ,closure)
       (empc-send (concat ,command "\n") ,closure))))

(defmacro empc-define-toggle-command (command &optional state-name attr &rest body)
  "Define a command that toggle a state."
  `(defun ,(intern (concat "empc-toggle-" command)) (&optional state)
     ,(concat "Toggle " command ".")
     (interactive)
     (empc-leave-idle-state)
     (if state
	 (empc-send (concat ,(concat command " ") (int-to-string state) "\n"))
       (with-updated-status
	(let ((,(if attr attr
		  (intern command))
	       (plist-get empc-current-status (quote ,(intern (concat ":" (if state-name
									      state-name
									    command)))))))
	  ,(if body
	       `(progn ,@body)
	     `(empc-send (concat ,command (if (= ,(if attr attr
						    (intern command)) 1) " 0" " 1") "\n"))))))))

;; Querying MPD's status
(empc-define-simple-command "clearerror")
(empc-define-simple-command "currentsong")
(empc-define-simple-command "idle" 'empc-response-idle)
(empc-define-simple-command "status" 'empc-response-get-status)
(empc-define-simple-command "stats")

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

;; Controlling playback
(empc-define-simple-command "next")
(empc-define-toggle-command "pause" "state" state
			    (cond
			     ((eq state 'play)
			      (empc-send "pause 1"))
			     ((eq state 'pause)
			      (empc-send "pause 0"))
			     (t (empc-send-play))))
(empc-define-simple-command "play")
(empc-define-simple-command "previous")
(empc-define-simple-command "stop")

;; The current playlist
(empc-define-simple-command "clear")
(empc-define-simple-command "playlistinfo" 'empc-response-get-playlist)
(empc-define-simple-command "shuffle")

;; Stored playlists
(empc-define-simple-command "listplaylists")

;; The music database

;; Stickers

;; Connection settings
(empc-define-simple-command "close")
(empc-define-simple-command "kill")
(empc-define-simple-command "ping")

;; Audio output devices

;; Reflection
(empc-define-simple-command "commands" '(lambda (data)
					  (setq empc-available-commands nil)
					  (dolist (cell data)
					    (setq empc-available-commands (cons (cdr cell) empc-available-commands)))))

;; Client to client

(provide 'empc)
