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

(defconst empc-buffer-name "*empc-process*"
  "The name of the buffer receiving output from server.")

(defconst empc-response-regexp
  "^\\(OK\\( MPD .*\\)?\\|ACK \\[\\([0-9]+\\)@[0-9]+\\] \\(.+\\)\\)\n+\\'"
  "Regexp that matches the valid status strings that MusicPD can
return at the end of a request.")

(defvar empc-object nil
  "Structure containing data for empc.
The structure is of the form:

	((queue . (process . (log . commands))) . (status . (playlist . playlist-songs)))

The fields are describe below:

process keeps the network-stream object.

log is the log buffer that keeps all commands sent to the server.

queue stores the commands to send to the server as well as the
  function to call when receiving the response, and a closure to
  pass to the function together with the response.
  This structure has the following type:
   (command closure . fn).

commands keeps the commands available to the
  user. There is no need to send a command to the server if we
  know already this command is unavailable due to restricted
  permissions or because the server has an older version.

status is another plist describing the server status.

playlist-songs is a hash table storing informations about the songs
  currently in the playlist.

playlist is a vector of song ids, keeping the order of the songs
  in the playlist.")

;; Accessors for the empc object.
(defun empc-process (object) (cadar object))
(defun empc-log (object) (caddar object))
(defun empc-queue (object) (caar object))
(defun empc-stream (object) empc-stream-process)
(defun empc-commands (object) (cdddar object))
(defun empc-status (object) (cadr object))
(defun empc-playlist-songs (object) (cdddr object))
(defun empc-playlist (object) (caddr object))

(defun empc-queue-head-command (object) (caar (empc-queue object)))
(defun empc-queue-head-closure (object) (cadar (empc-queue object)))
(defun empc-queue-head-fn (object) (cddar (empc-queue object)))
(defun empc-queue-push (object command closure fn)
  "Enqueue '(COMMAND . (CLOSURE . FN)) to the queue of OBJECT.
Leave the idle state beforehand if necessary."
  (when (empc-process object)
    (when command
      (with-current-buffer (empc-log object)
	(goto-char (point-max))
	(insert command)))
    (if (empc-queue object)
	(when (string= (empc-queue-head-command object) "idle\n")
	  (setcar (caaar object) "noidle\n")
	  (process-send-string (empc-process object) "noidle\n"))
      (when command
	(process-send-string (empc-process object) command))))
  (setcar (car object)
	  (nconc (empc-queue object)
		 (list (cons command (cons closure fn))))))

(defun empc-queue-sync (object)
  "Empty object's queue synchronously."
  (while (and (not (string= (empc-queue-head-command object) "idle\n"))
	      (accept-process-output (empc-process object) 10))))

(defun empc-queue-pop (object)
  "Pop the head of the queue then send the next command.
If there is no command left to send, put the client in idle state."
  (setcar (car object) (cdr (empc-queue object)))
  (if (empc-queue object)
      (progn
	(when (> (length (empc-queue object)) 1)
	  (empc-queue-merge object))
	(when (empc-queue-head-command object)
	  (process-send-string (empc-process object) (empc-queue-head-command object))))
    (empc-queue-push object "idle\n" 'empc-response-idle 'empc-handle-response)))

(defun empc-queue-merge (object)
  "Merge all commands in the queue as a single command_list."
  (let ((command "command_list_ok_begin\n")
	(closures nil))
    (setq closures (dolist (cell (empc-queue object) (reverse closures))
		     (setq command (concat command (car cell)))
		     (setq closures (cons (cadr cell) closures))))
    (setq command (concat command "command_list_end\n"))
    (setcar (car object) (list (cons command (cons closures 'empc-handle-response-list))))))

(defun empc-commands-set (object commands) (setcdr (cddar object) commands))
(defun empc-status-put (object attr value) (setcar (cdr object) (plist-put (empc-status object) attr value)))
(defun empc-status-get (object attr) (plist-get (empc-status object) attr))
(defun empc-playlist-set (object playlist) (setcar (cddr object) playlist))
(defun empc-playlist-songs-set (object playlist-songs) (setcdr (cddr object) playlist-songs))
(defun empc-song-by-id (object id) (gethash id (empc-playlist-songs object)))
(defun empc-song-by-pos (object pos) (empc-song-by-id object (elt (empc-playlist object) pos)))
(defun empc-current-song (object) (gethash (empc-status-get object :songid) (empc-playlist-songs object)))

(defun empc-modified (object)
  "Return true if the object has been modified between the moment
  the command has been sent and the moment the response came."
  (> (length (empc-queue object)) 1))

(defun empc-update-playlistsongs (object)
  "Fix songs position in the playlist."
  (let ((i 0)
	hash-items)
    (mapcar (lambda (id)
	      (puthash id (plist-put (empc-song-by-id object id) :pos i) (empc-playlist-songs object))
	      (incf i))
	    (empc-playlist object))
    (let ((playlist-items (append (empc-playlist object) nil)))
      (maphash (lambda (key value)
		 (unless (memq key playlist-items)
		   (setq hash-items (cons key hash-items))))
	       (empc-playlist-songs object)))
    (mapcar (lambda (id)
	      (remhash id (empc-playlist-songs object))) hash-items)))

(defun empc-create (name buffer host service)
  "Create and return a new object for empc. The parameters are as follows:

NAME is the name for the process.  It is modified if necessary to
 make it unique.
BUFFER is a buffer or buffer name to associate with the process.
 Process output goes at end of that buffer.  BUFFER may be nil,
 meaning that the process is not associated with any buffer.
HOST is the name or IP address of the host to connect to.
SERVICE is the name of the service desired, or an integer specifying
 a port number to connect to."

  (let* ((process (open-network-stream name buffer host service))
	 (object `((nil ,process ,(generate-new-buffer "*empc-log*") . ("password" "commands" "status" "playlistinfo" "idle")) nil nil))) ;; this weird form represents an empty object as described in empc-object
;    (empc-commands-set object '("password" "commands" "status" "playlistinfo" "idle"))
    (empc-queue-push object nil nil `(lambda (command closure msg)
				      (message "Connection to %s established" ',host)))
    (set-process-filter process `(lambda (proc string)
				  (empc-process-filter ',object string)))
    (set-process-sentinel process 'empc-process-sentinel)
    (if (fboundp 'set-process-query-on-exit-flag)
	(set-process-query-on-exit-flag process nil)
      (process-kill-without-query process))
    (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
    (buffer-disable-undo (process-buffer process))
    object))

(defun empc-close (object)
  "Close OBJECT: delete the process and kill the buffers."
  (let ((buffer (process-buffer (empc-process object))))
    (delete-process (empc-process object))
    (kill-buffer buffer)
    (kill-buffer (empc-log object))))

(defvar empc-song-format '(time "\t" (if (and artist title) (concat artist " - " title) file))
  "Format used to express songs in playlist and browser.
The construction is the same as `mode-line-format'.
You can use the following already defined variables:

  time, file, artist, title, album, year, track, full-track,
  genre, composer, performer, disc, comment, pos, full-pos.")

(defvar empc-mode-line-format '((if (string= state "play")
				    (concat " [" pos "/" playlistlength "] " (if (and artist title)
										(concat artist " - " title)
									      file))
				  (concat " " state)))
  "Format used to display empc state in mode-line.
The construction is the same as `mode-line-format'.
You can use the following already defined variables:

For songs:
  time, file, artist, title, album, year, track, full-track,
  genre, composer, performer, disc, comment, pos, full-pos.

For status:
  playlistlength, state.")

(defun empc-time-to-string (time &optional stop)
  "Format TIME to hh:mm:ss format."
  (if (> time 3600)
      (format "%d:%.2d:%.2d" (/ time 3600) (mod (/ time 60) 60) (mod time 60))
    (format "%d:%.2d" (/ time 60) (mod time 60))))

(defun empc-song-to-string (song)
  "Return a string representing a song formatted for the playlist buffer."
  (when song
    (concat (empc-time-to-string (plist-get song :time)) "\t"
	    (if (and (plist-member song :artist) (plist-member song :title))
		(concat (plist-get song :artist) " - " (plist-get song :title))
	      (plist-get song :file)))))

(defun empc-mode-line-to-string ()
  "Return a string to write to the mode-line."
  (if (eq (empc-status-get empc-object :state) 'play)
      (concat " [" (number-to-string (1+ (empc-status-get empc-object :song))) "/" (number-to-string (empc-status-get empc-object :playlistlength)) "] "
	      (if (and (plist-member (empc-current-song empc-object) :artist)
		       (plist-member (empc-current-song empc-object) :title))
		  (concat (plist-get (empc-current-song empc-object) :artist)
			  " - " (plist-get (empc-current-song empc-object) :title))
		(plist-get (empc-current-song empc-object) :file)))
    (concat " " (symbol-name (empc-status-get empc-object :state)))))

(defvar empc-last-crossfade nil)
(defvar empc-mode-line-string "")
(defvar empc-stream-process nil)
(defvar empc-may-pulse nil)
(when (require 'pulse nil t)
  (setq empc-may-pulse t))

(defconst empc-playlist-map (make-keymap) "Keymap for `empc'")
(define-key empc-playlist-map "q" 'empc-bury-buffers)
(define-key empc-playlist-map "Q" 'empc-quit)
(define-key empc-playlist-map "j" 'forward-line)
(define-key empc-playlist-map "k" (lambda () (interactive) (forward-line -1)))
(define-key empc-playlist-map "P" 'empc-toggle-pause)
(define-key empc-playlist-map "s" 'empc-send-stop)
(define-key empc-playlist-map "<" 'empc-send-previous)
(define-key empc-playlist-map ">" 'empc-send-next)
(define-key empc-playlist-map "r" 'empc-toggle-repeat)
(define-key empc-playlist-map "R" 'empc-toggle-consume)
(define-key empc-playlist-map "y" 'empc-toggle-single)
(define-key empc-playlist-map "z" 'empc-toggle-random)
(define-key empc-playlist-map "x" 'empc-toggle-crossfade)
(define-key empc-playlist-map "o" 'empc-playlist-goto-current-song)
(define-key empc-playlist-map [return] (lambda () (interactive) (empc-with-song-at-point song (empc-send-playid (plist-get song :id)))))
(define-key empc-playlist-map "d" 'empc-playlist-delete-song)
(define-key empc-playlist-map "c" 'empc-send-clear)

(defun empc-process-sentinel (proc event)
  "Process sentinel for `empc-process'."
  (let ((debug-on-error t)
	(status (process-status proc)))
    (cond ((eq status 'closed)
	   (when empc-verbose
	     (message "empc: connection closed"))))))

(defun empc-process-filter (object string)
  "Append STRING to the process buffer then process the data."
  (let ((debug-on-error t)
	(buffer (process-buffer (empc-process object))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert string))
      (empc-process-buffer object))))

(defun empc-process-buffer (object)
  "If the output stored in the buffer contains the regexp
  expressing the end of a command response, call the function
  stored at the head of the queue with the associated closure and
  the output as parameters."
  (let ((buffer (process-buffer (empc-process object)))
	(complete-response))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(unless (= 0 (buffer-size))
	  (if (not (empc-queue object))
	      (let ((buf (generate-new-buffer "*spurious*")))
		(copy-to-buffer buf (point-min) (point-max))
		(delete-region (point-min) (point))
		(pop-to-buffer buf nil)
		(error "Spurious communication from process %s, see buffer %s"
		       (process-name (empc-process object))
		       (buffer-name buf)))
	    (goto-char (point-min))
	    (when (re-search-forward empc-response-regexp nil t)
		(setq complete-response (buffer-substring (point-min) (point)))
		(delete-region (point-min) (point)))))))
    (when complete-response
      (unwind-protect
	  (funcall (empc-queue-head-fn object)
		   (empc-queue-head-command object)
		   (empc-queue-head-closure object) complete-response)
	(empc-queue-pop object))
      (empc-process-buffer object))))

(defun empc-stream-process-sentinel (proc event)
  "Process sentinel for `empc-stream-process'."
  (let ((debug-on-error t)
	(process (empc-process empc-object)))
    (when (and (eq (process-status proc) 'exit)
	       process
	       (processp process)
	       (eq (process-status process) 'open)
	       (eq (empc-status-get empc-object :state) 'play))
      (empc-toggle-pause 1)))
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
  "Notify SONG in the echo area."
  (interactive)
  (unless song
    (setq song (empc-current-song empc-object)))
  (when song
    (empc-echo-notify (concat "[" (int-to-string (+ (plist-get song :pos) 1))
			      "/" (int-to-string (empc-status-get empc-object :playlistlength)) "] "
			      (if (and (plist-get song :artist) (plist-get song :title))
				  (concat (plist-get song :artist) " - " (plist-get song :title))
				(plist-get song :file))))))

(defun empc-mode-line-update ()
  "Change the string to write in the mode-line and force-update it."
  (setq empc-mode-line-string (empc-mode-line-to-string))
  (force-mode-line-update))

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

(defun empc-response-get-commands (command data)
  "Parse DATA to get the available commands."
  (let ((commands))
    (dolist (cell data)
      (setq commands (cons (cdr cell) commands)))
    (empc-commands-set empc-object commands)))

(defun empc-status-on/off-stringify (status key)
  "Return `on' or `off' if KEY is active or inactive in STATUS."
  (if (= (plist-get status key) 0) "off" "on"))

(defun empc-response-get-status (command data)
  "Parse DATA to get a diff with `empc-current-status'.

According to what is in the diff, several actions can be performed:
	if :state or :songid is changed, report it to the user,
	if :state is set to 'play, start the streaming process."
  (let ((status-diff (empc-diff-status data))
	(notify nil))
    (when (plist-get status-diff :songid)
      (setq notify '(lambda ()
		      (when (empc-playlist-songs empc-object)
			(empc-echo-song))
		      (empc-playlist-goto-current-song))))
    (when (plist-get status-diff :state)
      (if (eq (plist-get status-diff :state) 'play)
	  (progn
	    (unless notify
	      (setq notify '(lambda () (when (empc-playlist-songs empc-object)
					 (empc-echo-song)))))
	    (empc-stream-start))
	(setq notify '(lambda () (empc-echo-notify (symbol-name (plist-get status-diff :state)))))))
    (when (or (plist-member status-diff :repeat) (plist-member status-diff :random)
	      (plist-member status-diff :single) (plist-member status-diff :consume)
	      (plist-member status-diff :xfade))
      (setq notify '(lambda () (empc-echo-notify (format "repeat: %s, random: %s, single: %s, consume: %s, crossfade: %s"
							 (empc-status-on/off-stringify (empc-status empc-object) :repeat)
							 (empc-status-on/off-stringify (empc-status empc-object) :random)
							 (empc-status-on/off-stringify (empc-status empc-object) :single)
							 (empc-status-on/off-stringify (empc-status empc-object) :consume)
							 (empc-status-on/off-stringify (empc-status empc-object) :xfade))))))
    (while status-diff
      (let ((attr (car status-diff))
	    (value (cadr status-diff)))
	(setq status-diff (cddr status-diff))
	(empc-status-put empc-object attr value)))
    (when (empc-playlist-songs empc-object)
      (empc-mode-line-update))
    (when notify
      (funcall notify))))

(defun empc-parse-status-attr (attr value)
  "Parse a single attribute from status."
  (cond
   ((eq value nil) nil)
   ((memq attr '(:volume :repeat :random :single :consume :playlist :playlistlength
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
	(unless (equal (empc-status-get empc-object attr) value)
	  (setq status-diff (plist-put status-diff attr value)))))))

(defun empc-playlist-get-songs (begin &optional end)
  "Retreive a list of songs' id between BEGIN and END."
  (when (eq major-mode 'empc-playlist-mode)
    (let* ((bpos (1- (line-number-at-pos begin)))
	   (epos (if end (count-lines begin end) 1))
	   ids)
      (dotimes (i epos ids)
	(setq ids (cons (empc-song-by-pos empc-object (+ bpos i)) ids))))))

(defun empc-playlist-goto-current-song ()
  "Put point at currently playing song."
  (interactive)
  (when (get-buffer "*empc*")
    (let ((buffer nil))
      (unless (called-interactively-p)
	(dolist (frame (frame-list))
	  (with-selected-frame frame
	    (let ((bwindow (get-buffer-window "*empc*")))
	      (when bwindow
		(with-selected-window bwindow
		  (goto-char (point-min))
		  (forward-line (empc-status-get empc-object :song))
		  (when (and (not buffer) empc-may-pulse)
		    (pulse-momentary-highlight-one-line (point))))
		(setq buffer bwindow))))))
      (unless buffer
	(with-current-buffer "*empc*"
	  (goto-char (point-min))
	  (forward-line (empc-status-get empc-object :song))
	  (when (and (called-interactively-p) empc-may-pulse)
	    (pulse-momentary-highlight-one-line (point))))))))

(defun empc-populate-playlist-buffer ()
  "Write playlist into the *empc* buffer."
  (save-window-excursion
    (empc-switch-to-playlist)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (when (empc-playlist-songs empc-object)
	(mapcar (lambda (id)
		  (empc-playlist-insert-song (empc-song-by-id empc-object id)))
		(empc-playlist empc-object))))))

(defmacro empc-with-current-playlist (&rest body)
  "Set the playlist buffer as the current one."
  `(save-window-excursion
     (empc-switch-to-playlist)
     (save-excursion
       (let ((buffer-read-only nil))
	 ,@body))))

(defmacro empc-with-song-at-point (song &rest body)
  "Retreive the song at point if applicable."
  `(when (memq major-mode '(empc-playlist-mode))
     (let ((,song (empc-song-by-pos empc-object (1- (line-number-at-pos (point))))))
       ,@body)))

(defun empc-playlist-insert-song (song)
  "Insert SONG into the playlist buffer."
  (empc-with-current-playlist
   (goto-char (point-min))
   (let ((needed-lines (forward-line (plist-get song :pos))))
     (when (or (> needed-lines 0) (eq (point) (point-max)))
       (dotimes (i (1+ needed-lines))
	 (insert "\n"))
       (forward-line -1)))
   (kill-region (line-beginning-position) (line-end-position))
   (insert (empc-song-to-string song))))

(defun empc-playlist-kill-song (song)
  "Kill SONG from the playlist."
  (empc-with-current-playlist
   (let ((songs (if (listp (car song)) song
		  (list song))))
     (mapcar (lambda (song)
	       (goto-char (point-min))
	       (forward-line (plist-get song :pos))
	       (kill-line 1))
	     songs))))

(defun empc-playlist-clean-after-playlist (pos)
  "Kill all the songs appearing in the playlist buffer that are
  located after the end of current playlist."
  (empc-with-current-playlist
   (goto-char (point-min))
   (forward-line pos)
   (kill-region (line-beginning-position) (point-max))))

(defun empc-playlist-delete-song ()
  "Delete song at point or a range of song if the mark is
  active."
  (interactive)
  (let (ids
	(songs (if (and mark-active (use-region-p))
		   (call-interactively (lambda (begin end)
					 (interactive "r")
					 (empc-playlist-get-songs begin end)))
		 (empc-playlist-get-songs (point)))))
    (when songs
      (mapcar (lambda (song)
		(empc-send-deleteid (plist-get song :id))
		(empc-playlist-kill-song song)
		(setq ids (cons (plist-get song :id) ids)))
	      songs)
      (empc-playlist-set empc-object (delete-if (lambda (id) (memq id ids))
						(empc-playlist empc-object)))
      (empc-update-playlistsongs empc-object))))

(defun empc-response-get-playlist (command data)
  "Parse information regarding songs in current playlist and arrange it into a
hash table `empc-current-playlist-songs'sorted by songid.
songs order is kept into an avector `empc-current-playlist'."
  (let* ((playlist-songs (make-hash-table :rehash-threshold 1.0 :size (empc-status-get empc-object :playlistlength)))
	 (playlist (make-vector (empc-status-get empc-object :playlistlength) nil))
	 (song nil)
	 (index (- (length playlist) 1)))
    (dolist (cell data)
      (let ((field (intern (concat ":" (car cell)))))
	(when (and (eq field :id) song)
	  (puthash (plist-get song :id) song playlist-songs)
	  (aset playlist index (plist-get song :id))
	  (setq song nil)
	  (decf index))
	(cond
	 ((memq field '(:time :track :date :pos :id))
	  (setq song (plist-put song field (string-to-number (cdr cell)))))
	 (t (if (plist-get song field)
		(setq song (plist-put song field (concat (plist-get song field) ", " (cdr cell))))
	      (setq song (plist-put song field (cdr cell))))))))
    (when (and song (>= index 0))
      (puthash (plist-get song :id) song playlist-songs)
      (aset playlist index (plist-get song :id)))
    (empc-playlist-set empc-object playlist)
    (empc-playlist-songs-set empc-object playlist-songs))
  (empc-populate-playlist-buffer))

(defun empc-response-parse-song (song)
  "Parse a single song."
  (let (song)
    (dolist (cell data song)
      (let ((field (intern (concat ":" (car cell)))))
	(cond
	 ((memq field '(:time :track :date :pos :id))
	  (setq song (plist-put song field (string-to-number (cdr cell)))))
	 (t (if (plist-get song field)
		(setq song (plist-put song field (concat (plist-get song field) ", " (cdr cell))))
	      (setq song (plist-put song field (cdr cell))))))))))

(defun empc-response-get-playlistid (command data)
  "Parse a single song and insert it into playlist-songs."
  (let ((song (empc-response-parse-song data)))
    (puthash (plist-get song :id) song (empc-playlist-songs empc-object))
    (empc-playlist-insert-song song)))

(defun empc-response-get-plchangesposid (command data)
  "Parse information regarding changes in the playlist since the last version."
  (if (empc-modified empc-object)
      (empc-send command 'empc-response-get-plchangesposid)
    (let ((new-pl (make-vector (empc-status-get empc-object :playlistlength) nil)))
      (dotimes (i (min (length new-pl) (length (empc-playlist empc-object))))
	(aset new-pl i (aref (empc-playlist empc-object) i)))
      (empc-playlist-clean-after-playlist (length new-pl))
      (empc-playlist-set empc-object new-pl)
      (while data
	(let ((id (string-to-number (cdar data)))
	      (cpos (string-to-number (cdadr data))))
	  (setq data (cddr data))
	  (let ((song (gethash id (empc-playlist-songs empc-object))))
	    (if (not song)
		(empc-send-playlistid id)
	      (puthash id (setq song (plist-put song :pos cpos)) (empc-playlist-songs empc-object))
	      (empc-playlist-insert-song song)))
	  (aset (empc-playlist empc-object) cpos id))))))

(defun empc-response-idle (command data)
  "React from idle interruption."
  (dolist (cell data)
    (when (string= (car cell) "changed")
      (let ((changed (cdr cell)))
	(cond
	 ((member changed '("player" "options"))
	  (empc-send-status))
	 ((member changed '("playlist"))
	  (empc-send-status)
	  (empc-send-plchangesposid (empc-status-get empc-object :playlist))))))))

(defun empc-handle-closure-call (command closures data)
  "If CLOSURES is a list of function, call them in turn with DATA
  as parameter."
  (when closures
    (if (and (listp closures) (not (memq (car closures) '(quote lambda))))
	(dolist (closure closures)
	  (funcall closure command data))
      (funcall closures command data))))

(defun empc-handle-response (command closures msg)
  "Retrieve the response from the server.
Check the error code and process it using CLOSURES."
  (let ((data (empc-response-parse-message msg)))
    (if (eq (car data) 'error)
	(empc-echo-notify (cdr data))
      (empc-handle-closure-call command closures data))))

(defun empc-handle-response-list (command closures msg)
  "Retrieve the responses from the server.
Check the error code and process the different responses to the
commands send as command_list."
  (let ((data (empc-response-parse-message msg))
	(commands (cdr (split-string command "\n"))))
    (if (eq (car data) 'error)
	(empc-echo-notify (cddr data))
      (dolist (closure closures)
	(empc-handle-closure-call (car commands) closure (car data))
	(setq commands (cdr commands))
	(setq data (cdr data))))))

(defun empc-mode-line (arg)
  "Add empc info to the mode-line if ARG is non-nil, remove if
ARG is nil."
  (interactive "p")
  (if arg
      (setq global-mode-string (append global-mode-string '(empc-mode-line-string)))
    (setq global-mode-string (remove 'empc-mode-line-string global-mode-string))))

(defun empc-initialize ()
  "Initialize the client after connection.
Send the password or retrieve available commands."
  (when empc-server-password
    (empc-send-password empc-server-password))
  (empc-send-commands)
  (empc-send-status)
  (empc-send-playlistinfo)
  (empc-mode-line t)
  (setq empc-last-crossfade nil))

(defun empc-ensure-connected ()
  "Make sure empc is connected and ready to talk to mpd."
  (let ((process (empc-process empc-object)))
    (unless (and process
		 (processp process)
		 (eq (process-status process) 'open))
      (setq empc-object (empc-create "empc" empc-buffer-name empc-server-host empc-server-port))
      (empc-initialize))))

(defun empc-bury-buffers ()
  "Bury all empc related buffers."
  (interactive)
  (while (member major-mode '(empc-playlist-mode))
    (bury-buffer)))

(defun empc-quit ()
  "Close connection between empc and mpd."
  (interactive)
  (let* ((process (empc-process empc-object)))
    (when (and process
	       (processp process)
	       (eq (process-status process) 'open))
      (empc-send-close)))
  (when empc-object
    (empc-close empc-object))
  (empc-mode-line nil)
  (when (get-buffer "*empc*")
    (kill-buffer "*empc*"))
  (setq empc-object nil
	empc-last-crossfade nil))

(defun empc ()
  "Emacs MPC (not really the most original name, but oh well…)."
  (interactive)
  (let ((debug-on-error t))
    (empc-ensure-connected)
    (empc-switch-to-playlist)))

(defun empc-send (command &optional closure handler)
  "Send COMMAND to the mpd server.
CLOSURE will be called on the parsed response."
  (empc-ensure-connected)
  (if (member (car (split-string command)) (empc-commands empc-object))
      (progn
	(unless (string= (substring command -1) "\n")
	  (setq command (concat command "\n")))
	(empc-queue-push empc-object command closure
			 (if handler handler 'empc-handle-response)))
    (message "empc: Command `%s' is not available (not supported by the server or forbidden to you)" command)))

(defun empc-send-retry ()
  "Resend the last command."
  (empc-send (empc-queue-head-command empc-object)
	     (empc-queue-head-closure empc-object)
	     (empc-queue-head-fn empc-object)))

(defun empc-send-sync (command &optional closure handler)
  "Send COMMAND synchronously. That means empc will push the
  command to the queue before synchronously emptying it."
  (empc-send command closure handler)
  (empc-queue-sync empc-object))

(defun empc-send-output (command)
  "Send COMMAND synchronously and return the server response as string."
  (let ((output))
    (empc-send-sync command nil (lambda (closures msg) (setq output msg)))
    output))

(defun empc-stream-start ()
  "Start the stream process if the command to mpd returned successfully.
If the stream process is killed for whatever the reason, pause mpd if possible."
  (let ((stream-process (empc-stream empc-object)))
    (when (and (not stream-process)
	       empc-stream-url empc-stream-program)
      (setq stream-process (start-process "empc-stream" nil empc-stream-program empc-stream-url))
      (set-process-sentinel stream-process 'empc-stream-process-sentinel)
      (setq empc-stream-process stream-process))))

(defun empc-playlist-mode ()
  "empc playlist mode."
  (use-local-map empc-playlist-map)
  (setq major-mode 'empc-playlist-mode)
  (setq mode-name "Empc-Playlist")
  (setq buffer-read-only t))

(defun empc-switch-to-playlist ()
  "Switch to the playlist buffer."
  (cond
   ((get-buffer-window "*empc*")
    (select-window (get-buffer-window "*empc*")))
   (t
    (switch-to-buffer "*empc*")))
   (empc-playlist-mode))

(defmacro empc-with-updated-status (&rest body)
  "Update the status and execute the forms in BODY."
  `(if (empc-status empc-object)
       ,@body
     (empc-send "status" '(empc-response-get-status (lambda (command data) ,@body)))))

(defmacro empc-define-simple-command (command &optional closure)
  "Define a simple command that doesn't need an argument."
  `(defun ,(intern (concat "empc-send-" command)) (&optional arg)
     ,(concat "Send " command " to the server.")
     (interactive)
     (let ((debug-on-error t))
       (empc-send (concat ,command (when arg (concat " " (if (stringp arg)
							     arg (number-to-string arg)))))
		  ,closure))))

(defmacro empc-define-toggle-command (command &optional state-name attr &rest body)
  "Define a command that toggle a state."
  `(progn
     (empc-define-simple-command ,command)
     (defun ,(intern (concat "empc-toggle-" command)) (&optional state)
       ,(concat "Toggle " command ".")
       (interactive)
       (let ((debug-on-error t))
	 (if state
	     (,(intern (concat "empc-send-" command)) (int-to-string state))
	   (empc-with-updated-status
	    (let ((,(if attr attr
		      (intern command))
		   (empc-status-get empc-object ,(intern (concat ":" (if state-name
										state-name
									      command))))))
	      ,(if body
		   `(progn ,@body)
		 `(,(intern (concat "empc-send-" command)) (% (1+ ,(if attr attr
								     (intern command))) 2))))))))))

;; Querying MPD's status
(empc-define-simple-command "clearerror")
(empc-define-simple-command "currentsong")
(empc-define-simple-command "status" 'empc-response-get-status)
(empc-define-simple-command "stats")

;; Playback options
(empc-define-toggle-command "consume")
(empc-define-toggle-command "crossfade" "xfade" xfade
			    (if (= xfade 0)
				(empc-send-crossfade (if empc-last-crossfade
							 empc-last-crossfade
						       empc-default-crossfade))
			      (progn
				(setq empc-last-crossfade xfade)
				(empc-send-crossfade 0))))
(empc-define-toggle-command "random")
(empc-define-toggle-command "repeat")
(empc-define-simple-command "setvol")
(empc-define-toggle-command "single")

;; Controlling playback
(empc-define-simple-command "next")
(empc-define-toggle-command "pause" "state" state
			    (cond
			     ((eq state 'play)
			      (empc-send-pause 1))
			     ((eq state 'pause)
			      (empc-send-pause 0))
			     (t (empc-send-play))))
(empc-define-simple-command "playid")
(empc-define-simple-command "previous")
(empc-define-simple-command "stop")

;; The current playlist
(empc-define-simple-command "clear")
(empc-define-simple-command "deleteid")
(empc-define-simple-command "playlistid" 'empc-response-get-playlistid)
(empc-define-simple-command "playlistinfo" 'empc-response-get-playlist)
(empc-define-simple-command "plchangesposid" 'empc-response-get-plchangesposid)
(empc-define-simple-command "shuffle")

;; Stored playlists
(empc-define-simple-command "listplaylists")

;; The music database

;; Stickers

;; Connection settings
(empc-define-simple-command "close")
(empc-define-simple-command "kill")
(empc-define-simple-command "password")
(empc-define-simple-command "ping")

;; Audio output devices

;; Reflection
(empc-define-simple-command "commands" 'empc-response-get-commands)

;; Client to client

(provide 'empc)
