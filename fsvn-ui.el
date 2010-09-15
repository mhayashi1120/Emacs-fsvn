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
  (let ((win (fsvn-buffer-popup-window)))
    (unless win
      (delete-other-windows)
      (setq win (split-window)))
    (set-window-buffer win buffer)
    (fsvn-save-window-only win
      (goto-char (point-min)))
    (redisplay t)))

(defun fsvn-buffer-popup-window ()
  (save-window-excursion
    (catch 'found
      (mapc
       (lambda (w)
	 (select-window w)
	 (when (eq major-mode 'fsvn-popup-result-mode)
	   (throw 'found w)))
       (window-list))
      nil)))



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

(defun fsvn-brief-message-add-message (message)
  (with-current-buffer (get-buffer-create fsvn-brief-message-buffer-name)
    (goto-char (point-max))
    (insert message "\n")
    (fsvn-brief-message-show-popup)))

(defmacro fsvn-brief-message-showing (&rest form)
  `(save-window-excursion
     (fsvn-brief-message-clear-message)
     ,@form))



(require 'electric nil t)

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
(defvar fsvn-electric-done-function nil)
(defvar fsvn-electric-next-data-function nil)

(defconst fsvn-electric-line-select-buffer-local-variables
  '(
    (fsvn-electric-line-alist)
    (fsvn-electric-start-point)
    (fsvn-electric-end-point)
    (fsvn-electric-done-function)
    (fsvn-electric-next-data-function)
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
		    (let* ((start-point (point))
			   (first-form '(or fsvn-electric-start-point (point-min)))
			   (last-form '(or fsvn-electric-end-point 
					   (progn 
					     (goto-char (1- (point-max))) 
					     (line-beginning-position))))
			   (first (eval first-form))
			   (last (eval last-form)))
		      ;; Use start-point if it is meaningful.
		      (goto-char (if (or (< start-point first)
					 (> start-point last))
				     first
				   start-point))
		      (Electric-command-loop 'fsvn-electric-buffer-menu-select
					     nil
					     t
					     'fsvn-electric-line-select-buffer-menu-looper
					     (cons first-form last-form))))))
	(message "")))
    select))

(defun fsvn-electric-line-select-buffer-menu-looper (state condition)
  (cond 
   ((and condition
	 (not (memq (car condition) '(buffer-read-only
				      end-of-buffer
				      beginning-of-buffer))))
    (signal (car condition) (cdr condition)))
   ((< (point) (save-excursion (eval (car state))))
    (goto-char (point-min)))
   ((> (point) (save-excursion (eval (cdr state))))
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

(defun fsvn-electric-call-next-data ()
  (condition-case err
      (progn
	(message "Geting Next...")
	(funcall fsvn-electric-next-data-function))
    (error
     (setq fsvn-electric-scroll-terminate t)
     (setq fsvn-electric-next-data-function nil)
     (ding))))

(defun fsvn-electric-line-select-quit ()
  (interactive)
  (throw 'fsvn-electric-buffer-menu-select nil))

(defun fsvn-electric-line-select-select ()
  (interactive)
  (throw 'fsvn-electric-buffer-menu-select (funcall fsvn-electric-done-function)))

(defmacro fsvn-electric-scroll (scroller error next-pos)
  `(condition-case nil
       (prog1
	   ,scroller
	 (setq fsvn-electric-scroll-terminate nil))
     (,error
      (if (and fsvn-electric-scroll-terminate
	       (not (pos-visible-in-window-p ,next-pos)))
	  (goto-char ,next-pos)
	(unless fsvn-electric-next-data-function
	  (ding)))
      (setq fsvn-electric-scroll-terminate t))))

(defun fsvn-electric-scroll-down ()
  (interactive)
  (fsvn-electric-scroll (scroll-down) beginning-of-buffer (point-max)))

(defun fsvn-electric-scroll-up ()
  (interactive)
  (fsvn-electric-scroll (scroll-up) end-of-buffer (point-min))
  (when (and fsvn-electric-scroll-terminate fsvn-electric-next-data-function)
    (setq fsvn-electric-scroll-terminate nil)
    (fsvn-electric-call-next-data)))

(defun fsvn-electric-previous-line (&optional arg)
  (interactive "p")
  (forward-line (- arg)))

(defun fsvn-electric-next-line (&optional arg)
  (interactive "p")
  (forward-line arg)
  (when (and (= (point-max) (point)) fsvn-electric-next-data-function)
    (fsvn-electric-call-next-data)))



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
	(setq fsvn-electric-done-function 'fsvn-electric-select-file-done)
	(mapc
	 (lambda (file)
	   (insert "  ")
	   (insert-directory file "-ald"))
	 files)
	(setq fsvn-electric-line-alist (mapcar (lambda (f) (cons f nil)) files)))
      (font-lock-mode 1)
      (font-lock-fontify-buffer)
      (run-mode-hooks 'fsvn-electric-line-select-mode-hook))
    (fsvn-electric-line-select buffer)))

(defun fsvn-electric-select-file-done ()
  (let (p1 p2 filename)
    (setq p1 (dired-move-to-filename))
    (setq p2 (dired-move-to-end-of-filename))
    (setq filename (buffer-substring-no-properties p1 p2))
    (fsvn-expand-file filename)))



;;
;; mode line status (from psvn.el)
;;

(defvar fsvn-ui-fancy-modeline t) ; modeline mark display or not
(defvar fsvn-ui-fancy-tooltip nil) ; modeline tooltip display

(defcustom fsvn-ui-fancy-file-state-in-modeline t
  "*Show a color dot in the modeline that describes the state of the current file."
  :type 'boolean
  :group 'fsvn)

(defun fsvn-ui-fancy-modeline-picture (color)
  (propertize "    "
              'help-echo 'fsvn-ui-fancy-tooltip
              'display
              `(image :type xpm
                      :data ,(format "/* XPM */
static char * data[] = {
\"18 13 3 1\",
\"  c None\",
\"+ c #000000\",
\". c %s\",
\"                  \",
\"       +++++      \",
\"      +.....+     \",
\"     +.......+    \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"     +.......+    \",
\"      +.....+     \",
\"       +++++      \",
\"                  \"};"
                                     color)
                      :ascent center)))

(defun fsvn-ui-fancy-install-state-mark (color)
  (let ((mode `(fsvn-ui-fancy-modeline
		,(fsvn-ui-fancy-modeline-picture color))))
    (unless (assq 'fsvn-ui-fancy-modeline mode-line-format)
      (setq mode-line-format (cons mode mode-line-format)))
    (force-mode-line-update t)))

(defun fsvn-ui-fancy-uninstall-state-mark ()
  (setq mode-line-format
	(assq-delete-all 'fsvn-ui-fancy-modeline
			 mode-line-format))
  (force-mode-line-update t))

(defun fsvn-ui-fancy-update-state-mark-tooltip (tooltip)
  (setq fsvn-ui-fancy-tooltip tooltip))

(defun fsvn-ui-fancy-update-state-mark (color)
  (fsvn-ui-fancy-uninstall-state-mark)
  (fsvn-ui-fancy-install-state-mark color))

(defun fsvn-ui-fancy-redraw ()
  (if (and fsvn-ui-fancy-file-state-in-modeline
	   (fsvn-vc-mode-p))
      (fsvn-ui-fancy-update-modeline)
    (fsvn-ui-fancy-uninstall-state-mark)))

(defadvice vc-find-file-hook (after fsvn-ui-fancy-vc-find-file-hook disable)
  "vc-find-file-hook advice for synchronizing psvn with vc-svn interface"
  (fsvn-ui-fancy-redraw))

(defadvice vc-after-save (after fsvn-ui-fancy-vc-after-save disable)
  "vc-after-save advice for synchronizing psvn when saving buffer"
  (fsvn-ui-fancy-redraw))

(defadvice ediff-refresh-mode-lines
  (around fsvn-ui-fancy-ediff-modeline-fixup disable compile)
  "Fixup svn file status in the modeline when using ediff"
  (ediff-with-current-buffer ediff-buffer-A
    (fsvn-ui-fancy-uninstall-state-mark))
  (ediff-with-current-buffer ediff-buffer-B
    (fsvn-ui-fancy-uninstall-state-mark))
  ad-do-it
  (ediff-with-current-buffer ediff-buffer-A
    (fsvn-ui-fancy-update-modeline))
  (ediff-with-current-buffer ediff-buffer-B
    (fsvn-ui-fancy-update-modeline)))

(defun fsvn-ui-fancy-update-modeline ()
  "Update modeline state dot mark properly"
  (when (and buffer-file-name (fsvn-vc-mode-p))
    (fsvn-ui-fancy-update-state-mark
     (fsvn-ui-fancy-interpret-state-mode-color
      (vc-svn-state buffer-file-name)))))

(defun fsvn-ui-fancy-interpret-state-mode-color (stat)
  "Interpret vc-svn-state symbol to mode line color"
  (cond
   ((eq stat 'edited)
    "tomato")
   ((eq stat 'up-to-date)
    "GreenYellow")
   ;; what is missing here??
   ;; ('unknown  "gray"        )
   ;; ('added    "blue"        )
   ;; ('deleted  "red"         )
   ;; ('unmerged "purple"      )
   (t "red")))



(defmacro fsvn-cmd-read-subcommand-args (subcommand var)
  `(if current-prefix-arg
       (fsvn-read-svn-subcommand-args ,subcommand t ,var)
     ,var))



(provide 'fsvn-ui)

;;; fsvn-ui.el ends here
