(require 'libmpdee)

(defgroup ido-mpc nil
  "customize group for ido-mpc."
  :group 'external
  :group 'applications
  :group 'multimedia)

(defcustom ido-mpc-server-url (or (getenv "MPD_HOST") "localhost")
  "The MPD server that we should connect to."
  :type 'string
  :group 'ido-mpc)

(defcustom ido-mpc-server-port (or (getenv "MPD_PORT") 6600)
  "The port of the MPD server."
  :type 'integer
  :group 'ido-mpc)

(defcustom ido-mpc-server-password nil
  "The password for the MPD server."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'ido-mpc)

(defcustom ido-mpc-stream-url nil
  "The url of the stream to play when asking MPD to start."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'ido-mpc)

(defcustom ido-mpc-stream-program "mplayer"
  "The program to launch to play the stream."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'ido-mpc)

(defcustom ido-mpc-check-interval 1
  "How often to check to see whether MusicPD has advanced to the
next song.  This may be an integer or nil. If set to nil, this
check will not be periodically performed."
  :type '(choice (const :tag "Disable check" nil)
		 integer)
  :group 'ido-mpc)

(defvar ido-mpc-conn nil)
(defvar ido-mpc-state nil)

(defun ido-mpc-connect ()
  (interactive)
  (when ido-mpc-conn
    (mpd-close-connection ido-mpc-conn))
  (setq ido-mpc-conn (mpd-conn-new ido-mpc-server-url
				   ido-mpc-server-port
				   1 nil))
  (when ido-mpc-server-password
    (mpd-set-password ido-mpc-conn ido-mpc-server-password))
  (ido-mpc-update-state))

(defun ido-mpc-update-state ()
  (setq ido-mpc-state (plist-get (mpd-get-status ido-mpc-conn) 'state))
  (unless ido-mpc-state
    (ido-mpc-connect)
    (setq ido-mpc-state (plist-get (mpd-get-status ido-mpc-conn) 'state))))

(defun ido-mpc-stream-play ()
  (interactive)
  (when (and ido-mpc-stream-url ido-mpc-stream-program ido-mpc-state)
    (start-process "mpc-stream" nil ido-mpc-stream-program ido-mpc-stream-url)))

(defun ido-mpc-play (&optional pos)
  (interactive)
  (ido-mpc-update-state)
  (if pos
      (mpd-play ido-mpc-conn pos)
    (mpd-play ido-mpc-conn))
  (ido-mpc-update-state)
  (ido-mpc-stream-play))

(defun ido-mpc-pause ()
  (interactive)
  (ido-mpc-update-state)
  (if (eq ido-mpc-state 'play)
      (mpd-pause ido-mpc-conn t)
    (if (eq ido-mpc-state 'stop)
	(ido-mpc-play)
      (mpd-pause ido-mpc-conn nil)))
  (ido-mpc-update-state)
  (ido-mpc-stream-play))

(provide 'ido-mpc)
