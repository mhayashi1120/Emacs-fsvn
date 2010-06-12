;;; fsvn-proctl.el --- Process control mode for fsvn.el


;;; Commentary:
;; 

;;; Code:



(require 'fsvn-mode)



(defconst fsvn-process-control-re-mark "^[^ \n]")

(defconst fsvn-process-control-buffer-local-variables
  '(
    (fsvn-process-control-process-alist . fsvn-process-control-process-alist)
    (fsvn-process-control-showing-process)
    (fsvn-process-control-processes . fsvn-process-control-processes)
    (fsvn-process-control-timer)
    (post-command-hook . '(fsvn-process-control-after-move))
    (font-lock-defaults . '(fsvn-process-control-font-lock-keywords nil nil nil beginning-of-line))
    (revert-buffer-function . 'fsvn-process-control-revert-buffer)
    (kill-buffer-hook . kill-buffer-hook)
    ))

(defconst fsvn-process-control-buffer-name " *Fsvn Processes*")
(defconst fsvn-process-control-re-mark-format "^[%s]")
(defvar fsvn-process-control-process-alist nil)
(defvar fsvn-process-control-showing-process nil)
(defvar fsvn-process-control-processes nil)
(defvar fsvn-process-control-display-p-function 'fsvn-process-control-default-display-p)
(defvar fsvn-process-control-timer nil)
(defvar fsvn-process-control-timer-interval 1
  "Seconds of updating buffer interval.")

(defconst fsvn-process-control-column-alist
  '(
    ("M" 1)
    ("Buffer-Size" 12)
    ("Status" -8)
    ("PWD" -50)
    ("Command")))

;;TODO not works
(defvar fsvn-process-control-font-lock-keywords nil)
(setq fsvn-process-control-font-lock-keywords
      (list

       (list (concat "\\`" (car (car fsvn-process-control-column-alist)))
	     '(".+" (forward-line 0) nil (0 fsvn-header-key-face)))

       (list (concat "^-[- ]+" )
	     '(".+" (forward-line 0) nil (0 fsvn-header-face)))

       ;; Fsvn marks.
       (list fsvn-process-control-re-mark '(0 fsvn-mark-face))

       (list (format fsvn-process-control-re-mark-format (char-to-string fsvn-mark-mark-char))
       	     '(".+" (fsvn-process-control-move-to-command-line) nil (0 fsvn-marked-face)))

       (list (format fsvn-process-control-re-mark-format (char-to-string fsvn-mark-delete-char))
       	     '(".+" (fsvn-process-control-move-to-command-line) nil (0 fsvn-flagged-face)))
       ))

(defvar fsvn-process-control-mode-map nil)
(unless fsvn-process-control-mode-map
  (setq fsvn-process-control-mode-map
	(let ((map (make-sparse-keymap)))
	  (suppress-keymap map)

	  (define-key map "U" 'fsvn-process-control-unmark-all)
	  (define-key map "d" 'fsvn-process-control-put-delete)
	  (define-key map "g" 'revert-buffer)
	  (define-key map "m" 'fsvn-process-control-put-mark)
	  (define-key map "n" 'fsvn-process-control-next-process)
	  (define-key map "p" 'fsvn-process-control-previous-process)
	  (define-key map "q" 'fsvn-process-control-quit)
	  (define-key map "u" 'fsvn-process-control-unmark)
	  (define-key map "x" 'fsvn-process-control-mark-execute)
	  (define-key map "\C-cH" 'fsvn-process-control-toggle-show-all)
	  (define-key map "\C-c\C-c" 'fsvn-process-control-mark-execute)
	  (define-key map "\C-c\C-k" 'fsvn-process-control-quit)
	  (define-key map "\C-c\C-p" 'fsvn-process-control-send-password-selected)
	  (define-key map "\C-c\C-q" 'fsvn-process-control-quit)
	  (define-key map "\C-m" 'fsvn-process-control-show-buffer)
	  (define-key map "\C-n" 'fsvn-process-control-next-process)
	  (define-key map "\C-p" 'fsvn-process-control-previous-process)

	  map)))

(defcustom fsvn-process-control-mode-hook nil
  "*Run at the very end of `fsvn-process-control-mode'."
  :group 'fsvn
  :type 'hook)

(defun fsvn-process-control-mode ()
  "Major mode to control fsvn background processes.

Entry to this mode calls the value of `fsvn-process-control-mode-hook'.

Keybindings:
\\{fsvn-process-control-mode-map}"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-process-control-mode-map)
  (setq major-mode 'fsvn-process-control-mode)
  (setq mode-name "Fsvn Processes")
  (setq truncate-lines t)
  (fsvn-make-buffer-variables fsvn-process-control-buffer-local-variables)
  (fsvn-browse-setup-mode-line))

(defun fsvn-process-control-prepared-buffer ()
  (fsvn-sole-major-mode 'fsvn-process-control-mode))

(defun fsvn-process-control-get-buffer ()
  (get-buffer-create fsvn-process-control-buffer-name))

(defun fsvn-process-control-point-process ()
  (catch 'found
    (mapc
     (lambda (o)
       (let ((p (overlay-get o 'fsvn-process-control-process)))
	 (when p (throw 'found p))))
     (overlays-at (point)))
    nil))

(defmacro fsvn-process-control-retrieve-mark (column &rest form)
  `(save-excursion
     (let (overlays buffer-read-only)
       (forward-line 0)
       (forward-char ,column)
       (setq overlays (overlays-at (point)))
       (unless overlays
	 (error "No process here"))
       ,@form
       (mapc
	(lambda (o)
	  (move-overlay o (line-beginning-position) (overlay-end o)))
	overlays)
       (setq buffer-undo-list nil)
       (set-buffer-modified-p nil))))

(defun fsvn-process-control-draw-list ()
  (let ((buffer (fsvn-process-control-get-buffer))
	processes)
    (with-current-buffer buffer
      (let (buffer-read-only)
	(erase-buffer)
	(fsvn-process-control-mode)
	(mapc 'delete-overlay (overlays-in (point-min) (point-max)))
	(let (header1 header2)
	  (mapc
	   (lambda (def)
	     (let* ((key (car def))
		    (size (fsvn-process-control-column:size key))
		    (name key))
	       (when size
		 (setq name (fsvn-filled-column key size)))
	       (setq header1 (cons name header1))
	       (when size
		 (setq header2 (cons (fsvn-header-tail size) header2)))))
	   fsvn-process-control-column-alist)
	  (insert (mapconcat 'identity (nreverse header1) " ") "\n")
	  (insert (mapconcat 'identity (nreverse header2) " ") " ")
	  (fsvn-header-tail-fill-line))
	(mapc
	 (lambda (p)
	   (when (funcall fsvn-process-control-display-p-function p)
	     (fsvn-process-control-insert-process p)
	     (setq processes (cons p processes))))
	 (fsvn-union (process-list) fsvn-process-control-processes 'memq)))
      (fsvn-process-control-activate-timer)
      (setq buffer-read-only t)
      (setq fsvn-process-control-processes processes)
      (font-lock-fontify-buffer)
      (run-mode-hooks 'fsvn-process-control-mode-hook))))

(defun fsvn-process-control-insert-process (process)
  (forward-line 0)
  (let ((cmdline (mapconcat 'identity (process-command process) " "))
	(start (point))
	(buffer (process-buffer process))
	(dir "")
	dirstr size sizestr status end overlay)
    (when (string= cmdline "")
      (setq cmdline (prin1-to-string process)))
    (when (and buffer (buffer-live-p buffer))
      (setq size (buffer-size buffer))
      (setq dir (with-current-buffer buffer
		  (or
		   (and default-directory (abbreviate-file-name default-directory))
		   ""))))
    (setq sizestr (fsvn-filled-column size (fsvn-process-control-column:size "Buffer-Size")))
    (setq status (fsvn-filled-column (process-status process) (fsvn-process-control-column:size "Status")))
    (setq dir (fsvn-string-truncate dir (fsvn-process-control-column:size "PWD")))
    (setq dirstr (fsvn-string-put-property dir 'face fsvn-directory-face))
    (insert (format "  %s %s %s %s\n" sizestr status dirstr cmdline))
    (setq end (point))
    (setq overlay (make-overlay start end nil t nil))
    (overlay-put overlay 'fsvn-process-control-process process)))

(defun fsvn-process-control-deactivate-timer ()
  (cancel-function-timers 'fsvn-process-control-redraw-list)
  (setq fsvn-process-control-timer nil))

(defun fsvn-process-control-activate-timer ()
  (fsvn-process-control-deactivate-timer)
  (setq fsvn-process-control-timer
	(run-at-time t fsvn-process-control-timer-interval 'fsvn-process-control-redraw-list))
  (add-hook 'kill-buffer-hook
	    (lambda ()
	      (fsvn-process-control-deactivate-timer))))

(defun fsvn-process-control-redraw-list ()
  (let ((buffer (fsvn-process-control-prepared-buffer))
	prev)
    (when (and (buffer-live-p buffer) (eq (current-buffer) buffer))
      (with-current-buffer buffer
	(unwind-protect
	    (progn
	      (setq prev (point))
	      (mapc
	       (lambda (p)
		 (cond
		  ((fsvn-process-control-goto-process p)
		   (let ((buffer (process-buffer p))
			 size)
		     ;;TODO
		     (when (looking-at "^..\\([ 0-9]\\{12\\} [ a-z]\\{8\\}\\)")
		       (save-match-data
			 (when (buffer-live-p buffer)
			   (setq size (buffer-size buffer))))
		       (let ((sizestr (fsvn-filled-column size (fsvn-process-control-column:size "Buffer-Size")))
			     (status (fsvn-filled-column (process-status p) (fsvn-process-control-column:size "Status")))
			     buffer-read-only)
			 
			 (replace-match (format "%s %s" sizestr status) nil nil nil 1)
			 (goto-char prev)))))
		  ((funcall fsvn-process-control-display-p-function p)
		   (goto-char (point-max))
		   (let (buffer-read-only)
		     (fsvn-process-control-insert-process p)
		     (setq fsvn-process-control-processes
			   (cons p fsvn-process-control-processes))))))
	       (fsvn-union (process-list) fsvn-process-control-processes 'memq)))
	  ;;restore point
	  (goto-char prev))))))

(defun fsvn-process-control-default-display-p (process)
  (string-match "^fsvn" (process-name process)))

(defun fsvn-process-control-move-to-command-line ()
  (let ((eol (line-end-position)))
    (beginning-of-line)
    (forward-char 75)))

(defun fsvn-process-control-column:size (column)
  (let ((def (assoc column fsvn-process-control-column-alist)))
    (nth 1 def)))

(defun fsvn-process-control-goto-process (process)
  (fsvn-process-control-goto-first)
  (let (p)
    (catch 'found
      (while (and (not (eobp))
		  (setq p (fsvn-process-control-point-process)))
	(when (eq p process)
	  (throw 'found p))
	(forward-line 1)))))

(defun fsvn-process-control-goto-first ()
  (goto-char (point-min))
  (while (and (not (eobp))
	      (not (fsvn-process-control-point-process)))
    (forward-line 1)))

(defun fsvn-process-control-revert-buffer (ignore-auto noconfirm)
  (let ((p (fsvn-process-control-point-process)))
    (fsvn-process-control-draw-list)
    (when p
      (fsvn-process-control-goto-process p))))

(defun fsvn-process-control-showing-window ()
  (when fsvn-process-control-showing-process
    (let ((alist (mapcar (lambda (w) (cons (window-buffer w) w)) (window-list)))
	  (buffer (process-buffer fsvn-process-control-showing-process))
	  win)
      (when (setq win (assq buffer alist))
	(cdr win)))))

(defun fsvn-process-control-after-move ()
  (let* ((win (fsvn-process-control-showing-window))
	 (process (fsvn-process-control-point-process))
	 (buffer (when process (process-buffer process))))
    (when win
      (if (and process (buffer-live-p buffer))
	  (set-window-buffer win buffer)
	(delete-window win))
      (setq fsvn-process-control-showing-process process))))

(defun fsvn-process-control-gather-marked-processes (&optional mark)
  (let* ((marker-char (or mark fsvn-mark-mark-char))
	 (regex (concat "^" (regexp-quote (char-to-string marker-char))))
	 ret temp)
    (save-excursion
      (fsvn-process-control-goto-first)
      (while (not (eobp))
	(when (looking-at regex)
	  (setq ret (cons (fsvn-process-control-point-process) ret)))
	(forward-line 1))
      (nreverse ret))))



(defun fsvn-process-control ()
  (interactive)
  (let ((win-configure (current-window-configuration)))
    (setq fsvn-process-control-display-p-function 'fsvn-process-control-default-display-p)
    (fsvn-process-control-draw-list)
    (let ((buffer (fsvn-process-control-get-buffer)))
      (switch-to-buffer buffer)
      (setq fsvn-previous-window-configuration win-configure)
      (fsvn-process-control-goto-first))))

(defun fsvn-process-control-toggle-show-all ()
  (interactive)
  (setq fsvn-process-control-display-p-function
	(if (eq fsvn-process-control-display-p-function 'fsvn-process-control-default-display-p)
	    (lambda (p) t)
	  'fsvn-process-control-default-display-p))
  (fsvn-process-control-draw-list))

(defun fsvn-process-control-quit ()
  (interactive)
  (fsvn-restore-window-buffer
   (kill-buffer (fsvn-process-control-get-buffer))))

(defun fsvn-process-control-put-delete ()
  (interactive)
  (fsvn-process-control-retrieve-mark 0
    (delete-char 1)
    (insert fsvn-mark-delete-char))
  (fsvn-process-control-next-process))

(defun fsvn-process-control-put-mark ()
  (interactive)
  (fsvn-process-control-retrieve-mark 0
    (delete-char 1)
    (insert fsvn-mark-mark-char))
  (fsvn-process-control-next-process))

(defun fsvn-process-control-unmark ()
  (interactive)
  (fsvn-process-control-retrieve-mark 0
    (delete-char 1)
    (insert ?\s))
  (fsvn-process-control-next-process))

(defun fsvn-process-control-unmark-all (char)
  (interactive "cRemove marks (RET means all): ")
  (let ((all (eq char 13))
	(count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(when (fsvn-process-control-point-process)
	  (fsvn-process-control-retrieve-mark 0
	    (when (or all (eq (char-after) char))
	      (delete-char 1)
	      (insert ?\s)
	      (setq count (1+ count)))))
	(forward-line 1)))
    (message (if (= count 1)
		 "1 mark removed"
	       "%d marks removed") count)))

(defun fsvn-process-control-next-process (&optional arg)
  (interactive "p")
  (forward-line arg)
  (fsvn-process-control-after-move))

(defun fsvn-process-control-previous-process (&optional arg)
  (interactive "p")
  (forward-line (- arg))
  (fsvn-process-control-after-move))

(defun fsvn-process-control-show-buffer ()
  (interactive)
  (let ((process (fsvn-process-control-point-process)))
    (when process
      (let ((buffer (process-buffer process)))
	(unless buffer
	  (error "This process has not buffer"))
	(display-buffer buffer)
	(with-current-buffer buffer
	  (goto-char (point-max))
	  (recenter))))
    (setq fsvn-process-control-showing-process process)))

(defun fsvn-process-control-send-password-selected (processes password)
  (interactive (let ((procs (fsvn-process-control-gather-marked-processes)))
		 (unless procs
		   (error "No process was selected"))
		 (list procs (read-passwd "Password: "))))
  (mapc
   (lambda (p)
     (let ((buffer (process-buffer p)))
       (when (buffer-live-p buffer)
	 (with-current-buffer buffer
	   ;; remove password/passphrase prompt
	   (goto-char (point-max))
	   (forward-line 0)
	   (when (looking-at "^.*\\(?:[Pp]assword\\|[Pp]assphrase\\).*")
	     (replace-match ""))))
       (when (eq (process-status p) 'run)
	 (process-send-string p (concat password "\n")))))
   processes))

(defun fsvn-process-control-mark-execute ()
  (interactive)
  (let ((regexp (format fsvn-process-control-re-mark-format (char-to-string fsvn-mark-delete-char)))
	process buffer-read-only)
    (save-excursion
      (fsvn-process-control-goto-first)
      (while (not (eobp))
	(setq process (fsvn-process-control-point-process))
	(when (and process (looking-at regexp))
	  ;; will be updated after timer
	  (delete-process process))
	(forward-line 1)))))



(put 'fsvn-process-control-retrieve-mark 'lisp-indent-function 1)


(provide 'fsvn-proctl)

;;; fsvn-proctl.el ends here
