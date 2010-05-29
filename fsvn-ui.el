;;; fsvn-ui.el --- Global User Interface definitions for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'dired)
(require 'fsvn-deps)



(defcustom fsvn-generic-datetime-format "%Y-%m-%d %H:%M"
  "*Date and time format in any."
  :group 'fsvn
  :type 'string)

(defcustom fsvn-help-locale nil
  "*Locale of your favorite."
  :group 'fsvn
  :type 'string)

(defconst fsvn-mark-mark-char dired-marker-char
  "In `fsvn-browse-mode', the current mark character.
This is what the do-commands look for, and what the mark-commands store.")

(defconst fsvn-mark-delete-char dired-del-marker
  "Character used to flag files for deletion.")

(defgroup fsvn-faces nil
  "*"
  :group 'fsvn)

(defface fsvn-header-face
  '((t (:inherit dired-header)))
  "Face used for directory headers."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-header-face 'fsvn-header-face
  "Face name used for directory headers.")

(defface fsvn-header-key-face
  '((t (:inherit dired-header :bold t)))
  "Face used for directory headers."
  :group 'fsvn-faces
  :version "22.1")

(defconst fsvn-header-key-face 'fsvn-header-key-face
  "Face name used for directory headers.")

(defface fsvn-mark-face
  '((t (:inherit dired-mark)))
  "Face used for fsvn marks."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-mark-face 'fsvn-mark-face
  "Face name used for fsvn marks.")

(defface fsvn-marked-face
  '((t (:inherit dired-marked)))
  "Face used for marked files."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-marked-face 'fsvn-marked-face
  "Face name used for marked files.")

(defface fsvn-flagged-face
  '((t (:inherit dired-flagged)))
  "Face used for flagged files."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-flagged-face 'fsvn-flagged-face
  "Face name used for flagged files.")

(defface fsvn-warning-face
  '((t (:inherit dired-warning)))
  "Face used to highlight a part of a buffer that needs user attention."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-warning-face 'fsvn-warning-face
  "Face name used for a part of a buffer that needs user attention.")

(defface fsvn-directory-face
  '((t (:inherit dired-directory)))
  "Face used for subdirectories."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-directory-face 'fsvn-directory-face
  "Face name used for subdirectories.")

(defface fsvn-symlink-face
  '((t (:inherit dired-symlink)))
  "Face used for subdirectories."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-symlink-face 'fsvn-symlink-face
  "Face name used for subdirectories.")

(defface fsvn-ignored-face
  '((t (:inherit shadow)))
  "Face used for files suffixed with `completion-ignored-extensions'."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-ignored-face 'fsvn-ignored-face
  "Face name used for files suffixed with `completion-ignored-extensions'.")

(defface fsvn-keyname-face
  '((t (:inherit dired-directory)))
  "Face used for revision"
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-keyname-face 'fsvn-keyname-face
  "Face name used for revision.")

(defface fsvn-link-face
  '((t (:foreground "blue" :underline "blue")))
  "Face used for any link"
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-link-face 'fsvn-link-face
  "Face used for any link.")



(defcustom fsvn-dired-copy-filename-separator " "
  "*String value of separate multiple filenames when killing."
  :group 'fsvn-dired
  :type 'string)



;; playing
;;  http://ja.wiktionary.org/wiki/Category:%E8%8B%B1%E8%AA%9E_%E4%B8%8D%E8%A6%8F%E5%89%87%E8%A4%87%E6%95%B0%E5%BD%A2%E3%81%AE%E5%90%8D%E8%A9%9E

(defconst fsvn-word-irregular-plural-alist
  '(
    ("child" . "children" )
    ;;    ("crux" . "cruxes" )
    ("foot" . "feet" )
    ("knife" . "knives" )
    ("leaf" . "leaves" )
    ("louse" . "lice" )
    ("man" . "men" )
    ("medium" . "media" )
    ("mouse" . "mice" )
    ("oasis" . "oases" )
    ("person" . "people" )
    ("phenomenon" . "phenomena" )
    ("seaman" . "seamen" )
    ("snowman" . "snowmen" )
    ("tooth" . "teeth" )
    ("woman" . "women" )
    ))

(defun fsvn-word-plural (word)
  (cond
   ((fsvn-string-assoc word fsvn-word-irregular-plural-alist)
    (cdr (fsvn-string-assoc word fsvn-word-irregular-plural-alist)))
   ((string-match "\\(sh\\|ch\\|o\\|s\\|x\\)$" word)
    (concat word "es"))
   ((string-match "\\(y\\)$" word)
    (replace-match "ies" nil nil word 1))
   (t
    (concat word "s"))))



;; face utility

;; FIXME want to well contrast value
(defun fsvn-get-background-color (foreground)
  (let ((count (length (defined-colors)))
	(rest (member foreground (defined-colors))))
    ;;FIXME
    (nth (% (+ (length rest) 100) count) (defined-colors))))



(defun fsvn-header-tail (&optional len)
  (make-string (abs len) ?-))

(defun fsvn-header-tail-fill-line ()
  (let ((width (- (frame-width) (current-column))))
    (when (> width 0)
      (insert (fsvn-header-tail width) "\n"))))



(defun fsvn-buffer-popup-as-information (buffer)
  (delete-other-windows)
  (let ((win (split-window)))
    (set-window-buffer win buffer)
    (fsvn-save-window-only win
      (goto-char (point-min)))))



(defconst fsvn-brief-message-buffer-name " *Fsvn Popup*")

(defun fsvn-brief-message-show-popup ()
  (let* ((buf (get-buffer-create fsvn-brief-message-buffer-name))
	 (win (get-buffer-window buf)))
    (when (and win (window-live-p win))
      (delete-window win))
    (dired-pop-to-buffer buf)))

(defun fsvn-brief-message-clear-message ()
  (with-current-buffer (get-buffer-create fsvn-brief-message-buffer-name)
    (erase-buffer)))

(defun fsvn-brief-message-set-message (message)
  (with-current-buffer (get-buffer-create fsvn-brief-message-buffer-name)
    (erase-buffer)
    (insert message)))

(defun fsvn-brief-message-add-message (message)
  (with-current-buffer (get-buffer-create fsvn-brief-message-buffer-name)
    (goto-char (point-max))
    (insert message "\n")
    (fsvn-brief-message-show-popup)))

(defun fsvn-brief-message-popup (message confirmer)
  (save-window-excursion
    (fsvn-brief-message-set-message message)
    (fsvn-brief-message-show-popup)
    (funcall confirmer)))

(defmacro fsvn-brief-message-showing (&rest form)
  `(save-window-excursion
     (fsvn-brief-message-clear-message)
     ,@form))



(require 'electric)

(defvar unread-command-events)

(defvar fsvn-electric-line-select-mode-map nil)
(defvar fsvn-electric-scroll-terminate nil)

(unless fsvn-electric-line-select-mode-map
  (setq fsvn-electric-line-select-mode-map
	(let ((map (make-keymap)))
	  (fillarray (car (cdr map)) 'undefined)

	  (define-key map "\C-c" nil)
	  (define-key map "\e" nil)

	  (define-key map "\C-c\C-c" 'fsvn-electric-line-select-quit)
	  (define-key map "\C-]" 'fsvn-electric-line-select-quit)
	  (define-key map "q" 'fsvn-electric-line-select-quit)
	  (define-key map " " 'fsvn-electric-line-select-select)
	  (define-key map "\C-m" 'fsvn-electric-line-select-select)
	  (define-key map "\C-n" 'fsvn-electric-next-line)
	  (define-key map "\C-p" 'fsvn-electric-previous-line)
	  (define-key map "\C-u" 'universal-argument)
	  (define-key map "n" 'fsvn-electric-next-line)
	  (define-key map "p" 'fsvn-electric-previous-line)
	  (define-key map "\C-v" 'fsvn-electric-scroll-up)
	  (define-key map "\ev" 'fsvn-electric-scroll-down)
	  map)))

(defvar fsvn-electric-line-alist nil)
(defvar fsvn-electric-start-point nil)
(defvar fsvn-electric-end-point nil)
(defconst fsvn-electric-line-select-buffer-local-variables
  '(
    (fsvn-electric-line-alist)
    (fsvn-electric-start-point)
    (fsvn-electric-end-point)
    (truncate-lines)
    ))

(defcustom fsvn-electric-line-select-mode-hook nil
  "*Run at the very end of `fsvn-electric-line-select-mode'."
  :group 'fsvn
  :type 'hook)

(define-minor-mode fsvn-electric-line-select-mode
  "
Keybindings:
\\{fsvn-electric-line-select-mode-map"
  nil " (Electric)" fsvn-electric-line-select-mode-map
  (fsvn-make-buffer-variables-internal fsvn-electric-line-select-buffer-local-variables))

(defun fsvn-electric-line-select (buffer)
  (let (select message-log-max)
    (save-window-excursion
      (Electric-pop-up-window buffer)
      (unwind-protect
	  (progn
	    (set-buffer buffer)
	    (fsvn-electric-line-select-buffer-update-highlight)
	    (setq select
		  (catch 'fsvn-electric-buffer-menu-select
		    (message "->")
		    (when (eq (setq unread-command-events (list (read-event))) ?\s)
		      (setq unread-command-events nil)
		      (throw 'fsvn-electric-buffer-menu-select nil))
		    (let ((start-point (point))
			  (first (or fsvn-electric-start-point (point-min)))
			  (last (or fsvn-electric-end-point 
				    (progn 
				      (goto-char (1- (point-max))) 
				      (line-beginning-position )))))
		      ;; Use start-point if it is meaningful.
		      (goto-char (if (or (< start-point first)
					 (> start-point last))
				     first
				   start-point))
		      (Electric-command-loop 'fsvn-electric-buffer-menu-select
					     nil
					     t
					     'fsvn-electric-line-select-buffer-menu-looper
					     (cons first last))))))
	(message "")))
    select))

(defun fsvn-electric-line-select-buffer-menu-looper (state condition)
  (cond 
   ((and condition
	 (not (memq (car condition) '(buffer-read-only
				      end-of-buffer
				      beginning-of-buffer))))
    (signal (car condition) (cdr condition)))
   ((< (point) (car state))
    (goto-char (point-min)))
   ((> (point) (cdr state))
    (goto-char (point-max))
    (forward-line -1)
    (if (pos-visible-in-window-p (point-max))
	(recenter -1))))
  (fsvn-electric-line-select-buffer-update-highlight))

(defvar fsvn-electric-line-select-buffer-overlay nil)
(defun fsvn-electric-line-select-buffer-update-highlight ()
  (when fsvn-electric-line-select-mode
    ;; Make sure we have an overlay to use.
    (unless fsvn-electric-line-select-buffer-overlay
      (make-local-variable 'fsvn-electric-line-select-buffer-overlay)
      (setq fsvn-electric-line-select-buffer-overlay (make-overlay (point) (point))))
    (move-overlay fsvn-electric-line-select-buffer-overlay 
		  (line-beginning-position)
		  (line-end-position))
    (overlay-put fsvn-electric-line-select-buffer-overlay 'face 'highlight)))

(defun fsvn-electric-line-select-quit ()
  (interactive)
  (throw 'fsvn-electric-buffer-menu-select nil))

(defun fsvn-electric-line-select-select ()
  (interactive)
  (let (p1 p2 filename)
    (setq p1 (dired-move-to-filename))
    (setq p2 (dired-move-to-end-of-filename))
    (setq filename (buffer-substring-no-properties p1 p2))
  (throw 'fsvn-electric-buffer-menu-select (expand-file-name filename))))

(defmacro fsvn-electric-scroll (scroller error next-pos)
  `(condition-case nil
       (prog1
	   ,scroller
	 (setq fsvn-electric-scroll-terminate nil))
     (,error
      (if (and fsvn-electric-scroll-terminate
	       (not (pos-visible-in-window-p ,next-pos)))
	  (goto-char ,next-pos)
	(ding))
      (setq fsvn-electric-scroll-terminate t))))

(defun fsvn-electric-scroll-down ()
  (interactive)
  (fsvn-electric-scroll (scroll-down) beginning-of-buffer (point-max)))

(defun fsvn-electric-scroll-up ()
  (interactive)
  (fsvn-electric-scroll (scroll-up) end-of-buffer (point-min)))

(defun fsvn-electric-previous-line (&optional arg)
  (interactive "p")
  (forward-line (- arg)))

(defun fsvn-electric-next-line (&optional arg)
  (interactive "p")
  (forward-line arg))



(require 'ls-lisp)

(defconst fsvn-electric-select-file-list-buffer-name " *Fsvn Electric* ")

(defun fsvn-electric-select-file (base-directory files)
  (let ((buffer (get-buffer-create fsvn-electric-select-file-list-buffer-name)))
    (with-current-buffer buffer
      (set (make-local-variable 'font-lock-defaults)
	   '(dired-font-lock-keywords t nil nil beginning-of-line))
      (let ((ls-lisp-filesize-d-fmt "%10d")
	    ls-lisp-use-insert-directory-program buffer-read-only)
	(erase-buffer)
	(fsvn-electric-line-select-mode 1)
	(setq default-directory (file-name-as-directory base-directory))
	(mapc
	 (lambda (file)
	   (insert "  ")
	   (insert-directory file "-ald"))
	 files)
	(setq fsvn-electric-line-alist (mapcar (lambda (f) (cons f nil)) files)))
      (font-lock-mode 1)
      (font-lock-fontify-buffer)
      (run-hooks 'fsvn-electric-line-select-mode-hook))
    (fsvn-electric-line-select buffer)))



(defmacro fsvn-cmd-read-subcommand-args (subcommand var)
  `(if current-prefix-arg
       (fsvn-read-svn-subcommand-args ,subcommand t ,var)
     ,var))



(provide 'fsvn-ui)

;;; fsvn-ui.el ends here
