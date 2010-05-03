;;; fsvn-logview.el --- Subversion log view utility for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:
;;



(require 'fsvn-mode)



(defvar text-mode-map)
(defvar current-prefix-arg)



(defconst fsvn-log-view-buffer-local-variables
  '(
    (font-lock-defaults . '(fsvn-log-view-font-lock-keywords t nil nil beginning-of-line))
    (fsvn-buffer-target-file)
    (fsvn-buffer-target-directory-p)
    (fsvn-buffer-repos-root)
    (fsvn-log-view-target-path)
    (fsvn-log-view-entries)
    (fsvn-log-view-all-entries)
    (fsvn-log-view-subwindow-settings)
    (revert-buffer-function . 'fsvn-log-view-revert-buffer)
    ))

(defconst fsvn-log-view-revision-length 6)
(defconst fsvn-log-view-re-mark "^[^ \n]")
(defconst fsvn-log-view-re-terms
  "^ \\(Terms of entries:\\) \\(.+\\)")

(defconst fsvn-log-view-re-target
  "^ \\(Logs for\\) \\(.+\\)")

(defconst fsvn-log-view-user-name-length 15)
(defconst fsvn-log-view-message-length 50)
(defconst fsvn-log-view-buffer-name "Log for ")

(defvar fsvn-log-view-target-path nil)
(defvar fsvn-log-view-subwindow-settings nil)
(defvar fsvn-log-view-entries nil
  "Shown logs sorted by revision.")
(defvar fsvn-log-view-all-entries nil
  "logentry of has been shown. sorted by revision.")
(defvar fsvn-log-view-isarch-history nil)

(defvar fsvn-log-view-font-lock-keywords nil)
(setq fsvn-log-view-font-lock-keywords
      (list
       (list fsvn-log-view-re-terms '(1 fsvn-header-key-face) '(2 fsvn-header-face))
       (list fsvn-log-view-re-target '(1 fsvn-header-key-face) '(2 fsvn-header-face))
       (list "^. \\([ 0-9]+\\)" '(1 fsvn-keyname-face))
       (list fsvn-log-view-re-mark '(0 fsvn-mark-face))
       (list (concat "^[" (char-to-string fsvn-mark-mark-char) "]")
	     '(".+" (fsvn-log-view-move-to-date) nil (0 fsvn-marked-face)))
       ))

(defvar fsvn-log-view-diff-mode-map nil)
(unless fsvn-log-view-diff-mode-map
  (setq fsvn-log-view-diff-mode-map
	(let ((map (make-sparse-keymap)))
	  (suppress-keymap map)

	  (define-key map "=" 'fsvn-log-view-diff-generic)
	  (define-key map "e" 'fsvn-log-view-ediff-generic)
	  (define-key map "w" 'fsvn-log-view-diff-with-wc)

	  map)))

(defvar fsvn-log-view-mode-map nil)
(unless fsvn-log-view-mode-map
  (setq fsvn-log-view-mode-map
	(let ((map (make-sparse-keymap)))
	  (suppress-keymap map)

	  (fsvn-readonly-mode-keymap map)

	  (define-key map " " 'fsvn-log-view-scroll-message-up)
	  (define-key map "%" (make-sparse-keymap))
	  (define-key map "%m" 'fsvn-log-view-mark-searched)
	  (define-key map "<" 'fsvn-log-view-next-log)
	  (define-key map "=" fsvn-log-view-diff-mode-map)
	  (define-key map ">" 'fsvn-log-view-previous-log)
	  (define-key map "L" 'fsvn-log-view-reload-with-change-limit)
	  (define-key map "N" 'fsvn-log-view-next-mark)
	  (define-key map "P" 'fsvn-log-view-previous-mark)
	  (define-key map "R" 'fsvn-log-view-reload-with-revision)
	  (define-key map "S" 'fsvn-log-view-save)
	  (define-key map "\C-c\C-k" 'fsvn-log-view-quit)
	  (define-key map "\C-c\C-o" 'fsvn-log-switch-to-message)
	  (define-key map "\C-c\C-q" 'fsvn-log-view-quit)
	  (define-key map "\C-m" 'fsvn-log-view-show-details)
	  (define-key map "\C-n" 'fsvn-log-view-next-line)
	  (define-key map "\C-p" 'fsvn-log-view-previous-line)
	  (define-key map "\d" 'fsvn-log-view-scroll-message-down)
	  (define-key map "m" 'fsvn-log-view-mark-put-mark)
	  (define-key map "n" 'fsvn-log-view-next-line)
	  (define-key map "p" 'fsvn-log-view-previous-line)
	  (define-key map "q" 'fsvn-log-view-quit)
	  (define-key map "s" 'fsvn-log-view-isearch-text)
	  (define-key map "u" 'fsvn-log-view-mark-unmark)
	  (define-key map "v" 'fsvn-log-view-toggle-details)
	  (define-key map "w" 'fsvn-log-view-copy-urlrev)
	  
	  (define-key map "g" 'revert-buffer)

	  (define-key map "\C-c\C-r" 'fsvn-log-view-edit-revprop)
	  (define-key map "\C-co" 'fsvn-log-view-open-revision)

	  ;;todo not implement
	  ;; 	(define-key map "U" 'fsvn-log-view-mark-unmark-all)
	  ;; 	(define-key map "\C-cr" 'fsvn-log-view-revert-to-revision)
	  ;; 	(define-key map "\C-cp" 'fsvn-log-view-open-propview)

	  map)))

(defcustom fsvn-log-view-mode-hook nil
  "*Run at the very end of `fsvn-log-view-mode'."
  :group 'fsvn
  :type 'hook)

;; * fsvn-log-view-mode internal function

(defun fsvn-log-view-mode ()
  "Major mode for browsing Subversion log.

Entry to this mode calls the value of `fsvn-log-view-mode-hook'.

Keybindings:
\\{fsvn-log-view-mode-map}

"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-log-view-mode-map)
  (setq major-mode 'fsvn-log-view-mode)
  (setq mode-name "Fsvn Log View")
  (setq truncate-lines t)
  (fsvn-make-buffer-variables fsvn-log-view-buffer-local-variables))

(defmacro fsvn-log-view-each-rev (entry &rest form)
  `(save-excursion
     (fsvn-log-view-goto-first-revision)
     (let (REV)
       (while (setq REV (fsvn-log-view-point-revision))
	 (setq ,entry (fsvn-log-view-find-entry REV))
	 ,@form
	 (fsvn-log-view-next-line)))))

(defun fsvn-log-view-scroll-message-buffer (arg)
  "ARG t or nil or number. t means scroll-up with nil. nil means scroll-down with nil."
  (let ((message-buffer (get-buffer fsvn-log-message-buffer-name))
	(sibling-buffer (get-buffer fsvn-log-sibling-buffer-name))
	(origin-window (selected-window))
	scroll scroller first-buf second-buf)
    (cond
     ((eq arg t)
      (setq scroll nil
	    scroller 'fsvn-scroll-window-buffer-up))
     ((null arg)
      (setq scroll nil
	    scroller 'fsvn-scroll-window-buffer-down))
     ((plusp arg)
      (setq scroll arg
	    scroller 'fsvn-scroll-window-buffer-up))
     ((minusp arg)
      (setq scroll arg
	    scroller 'fsvn-scroll-window-buffer-down)))
    (if (eq scroller 'fsvn-scroll-window-buffer-up)
	(setq first-buf message-buffer
	      second-buf sibling-buffer)
      (setq first-buf sibling-buffer
	    second-buf message-buffer))
    (when (memq message-buffer (mapcar 'window-buffer (window-list)))
      (unless (funcall scroller first-buf scroll)
	(funcall scroller second-buf scroll)))))

(defun fsvn-log-view-draw-details (&optional revision)
  (let* ((rev (or revision (fsvn-log-view-point-revision)))
	 (dir default-directory)
	 (root fsvn-buffer-repos-root)
	 (path (fsvn-log-view-point-path))
	 (main-buffer (current-buffer))
	 logentry)
    (when rev
      (setq logentry (fsvn-log-view-find-showing-entry rev)))
    (with-current-buffer (fsvn-log-message-get-buffer)
      (let ((msg (fsvn-xml-log->logentry=>msg$ logentry)))
	(fsvn-set-default-directory dir)
	(let (buffer-read-only)
	  (erase-buffer)
	  (when msg
	    (insert msg)))
	(fsvn-log-message-mode)
	(setq buffer-read-only t)
	(setq fsvn-buffer-repos-root root)
	(setq fsvn-log-source-buffer main-buffer)
	(setq fsvn-log-message-revision rev)
	(run-hooks 'fsvn-log-message-mode-hook)))
    (with-current-buffer (fsvn-log-sibling-get-buffer)
      (fsvn-set-default-directory dir)
      (let (regexp)
	(let (buffer-read-only)
	  (erase-buffer)
	  (mapc
	   (lambda (path-entry)
	     (let ((act (fsvn-xml-log->logentry->paths->path.action path-entry))
		   (text (fsvn-xml-log->logentry->paths->path$ path-entry))
		   copied)
	       (insert (format "%s %s\n" act text))
	       ;; copy or move file
	       (when (and (string= act "A")
			  (setq copied (fsvn-xml-log->logentry->path.copyfrom-path path-entry))
			  (fsvn-url-belongings-p text path))
		 ;;FIXME regexp not correct
		 (setq regexp (format "^[AD] \\(%s\\|%s\\)$" text (fsvn-url-decode-string copied))))))
	   (fsvn-xml-log->logentry->paths logentry)))
	(fsvn-log-sibling-mode)
	(when (and path (null regexp))
	  (setq regexp (concat "^..\\(" (regexp-quote (fsvn-url-decode-string path)) "\\)\\(?:/\\|$\\)")))
	(setq fsvn-buffer-repos-root root)
	(setq fsvn-log-source-buffer main-buffer)
	(setq fsvn-log-sibling-target-path path)
	(setq fsvn-log-sibling-logentry logentry)
	(setq fsvn-buffer-revision rev)
	(setq fsvn-log-sibling-font-lock-keywords
	      (when regexp
		(list (list regexp '(1 fsvn-header-key-face))))))
      (setq buffer-read-only t)
      (run-hooks 'fsvn-log-sibling-mode-hook))))

(defun fsvn-log-view-find-showing-entry (rev)
  (fsvn-find-logs-entry rev fsvn-log-view-entries))

(defun fsvn-log-view-find-entry (rev)
  (fsvn-find-logs-entry rev fsvn-log-view-all-entries))

(defun fsvn-log-view-point-revision ()
  (save-excursion
    (forward-line 0)
    (when (looking-at "^.. *\\([0-9]+\\)")
      (string-to-number (match-string 1)))))

(defun fsvn-log-view-put-mark-this-line (&optional mark)
  (let (buffer-read-only)
    (save-excursion
      (forward-line 0)
      (delete-char 1)
      (insert (or mark fsvn-space-char)))))

(defun fsvn-log-view-revert-buffer (ignore-auto noconfirm)
  (let ((range (fsvn-log-view-current-revision-range))
	(rev (fsvn-log-view-point-revision)))
    (mapc
     (lambda (entry)
       (setq fsvn-log-view-all-entries (delq entry fsvn-log-view-all-entries)))
     fsvn-log-view-entries)
    (fsvn-open-log-view-mode fsvn-buffer-target-file fsvn-buffer-target-directory-p range)
    (when rev
      (fsvn-log-view-goto-revision rev))))

(defun fsvn-log-view-entry-revision (entry)
  (fsvn-string-lpad (number-to-string (fsvn-xml-log->logentry.revision entry))
		    fsvn-log-view-revision-length))

(defun fsvn-log-view-entry-message (entry)
  (let* ((msg (fsvn-xml-log->logentry=>msg$ entry))
	 (ret
	  (if msg
	      (fsvn-string-rtrim (fsvn-string-single-line msg) fsvn-log-view-message-length)
	    (make-string 0 0))))
    (fsvn-log-view-message-property ret)))

(defun fsvn-log-view-entry-user (entry)
  (fsvn-string-rpad (fsvn-xml-log->logentry=>author$ entry) fsvn-log-view-user-name-length))

(defun fsvn-log-view-entry-action (entry)
  (fsvn-string-rpad (fsvn-xml-log->logentry->paths->path.action entry) 5))

(defun fsvn-log-view-entry-date (entry)
  (let ((ret (format-time-string "%Y-%m-%d %H:%M:%S" (fsvn-xml-log->logentry=>date$ entry))))
    (fsvn-log-view-date-property ret)))

(defun fsvn-log-view-insert-entry (entry)
  (insert (format "  %s %s %s %s %s\n"
		  (fsvn-log-view-entry-revision entry)
		  (fsvn-log-view-entry-action entry)
		  (fsvn-log-view-entry-user entry)
		  (fsvn-log-view-entry-date entry)
		  (fsvn-log-view-entry-message entry)
		  )))

(defun fsvn-log-view-insert-header-entry (file first last)
  (insert (format " Logs for %s\n" file))
  (insert (format " Terms of entries: %s - %s\n"
		  (format-time-string "%Y-%m-%d" (fsvn-xml-log->logentry=>date$ first))
		  (format-time-string "%Y-%m-%d" (fsvn-xml-log->logentry=>date$ last))
		  ))
  (insert "\n"))

(defun fsvn-log-view-setup-detail-windows ()
  (let ((message-buffer (fsvn-log-message-get-buffer))
	(sibling-buffer (fsvn-log-sibling-get-buffer))
	first-win second-win third-win)
    (save-selected-window
      (delete-other-windows)
      (setq first-win (get-buffer-window (current-buffer)))
      (setq second-win (split-window nil (/ (window-height) 4)))
      (set-window-buffer second-win message-buffer)
      (set-frame-selected-window (selected-frame) second-win)
      (setq third-win (split-window nil (/ (window-height) 2)))
      (set-window-buffer third-win sibling-buffer))))

(defun fsvn-log-view-get-buffer (url)
  (let ((bufs (buffer-list))
	target)
    (while bufs
      (with-current-buffer (car bufs)
	(when (eq major-mode 'fsvn-log-view-mode)
	  (when (string= fsvn-buffer-target-file url)
	    (setq target (car bufs))
	    (setq bufs nil))))
      (setq bufs (cdr bufs)))
    (if target
	target
      (generate-new-buffer (concat fsvn-log-view-buffer-name
				   "[" (fsvn-file-name-nondirectory url) "]")))))

(defun fsvn-log-view-move-to-message ()
  (let ((eol (line-end-position)))
    (beginning-of-line)
    (let ((change (next-single-property-change (point) 'fsvn-log-message nil eol)))
      (cond
       ((and change (< change eol))
	(goto-char change))
       ((or (looking-at fsvn-log-view-re-target)
	    (looking-at fsvn-log-view-re-terms))
	)
       (t
	;;for none log message
	(goto-char eol))))))

(defun fsvn-log-view-move-to-date ()
  (let ((eol (line-end-position)))
    (beginning-of-line)
    (let ((change (next-single-property-change (point) 'fsvn-log-date nil eol)))
      (cond
       ((and change (< change eol))
	(goto-char change))))))

(defun fsvn-log-view-message-property (message)
  (fsvn-string-put-property message 'fsvn-log-message t))

(defun fsvn-log-view-date-property (date)
  (fsvn-string-put-property date 'fsvn-log-date t))

(defun fsvn-log-view-subwindow-display-p ()
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (or (memq (get-buffer fsvn-log-message-buffer-name) buffers)
	(memq (get-buffer fsvn-log-sibling-buffer-name) buffers))))

(defun fsvn-log-view-goto-first-revision ()
  (let ((saved (point)))
    (goto-char (point-min))
    (if (catch 'found
	  (while (not (eobp))
	    (when (fsvn-log-view-point-revision)
	      (throw 'found t))
	    (forward-line 1)))
	(fsvn-log-view-move-to-message)
      (goto-char saved))))

(defun fsvn-log-view-goto-revision (rev)
  (let ((point (catch 'found
		 (save-excursion
		   (fsvn-log-view-goto-first-revision)
		   (let (cur)
		     (while (setq cur (fsvn-log-view-point-revision))
		       (when (= cur rev)
			 (fsvn-log-view-move-to-message)
			 (throw 'found (point)))
		       (forward-line 1)))))))
    (when point
      (goto-char point))))

(defun fsvn-log-view-after-move-line ()
  (fsvn-log-view-move-to-message)
  (when (fsvn-log-view-subwindow-display-p)
    (fsvn-log-view-draw-details)
    (fsvn-log-view-set-subwindow-config)))

(defun fsvn-log-view-cmd-diff-arg ()
  (list
   (if current-prefix-arg
       (fsvn-read-svn-subcommand-args "diff" t fsvn-default-args-diff)
     fsvn-default-args-diff)))

(defun fsvn-log-view-matched-entries (text)
  (let (ret)
    (mapc
     (lambda (entry)
       (when (fsvn-xml-text-matched entry text)
	 (setq ret (cons entry ret))))
     fsvn-log-view-entries)
    (nreverse ret)))

(defun fsvn-log-view-current-revision-range ()
  (let ((first (car fsvn-log-view-entries))
	(last (car (last fsvn-log-view-entries))))
    (cons (fsvn-xml-log->logentry.revision first)
	  (fsvn-xml-log->logentry.revision last))))

(defun fsvn-log-view-diff-with-region (args)
  (let ((revs (fsvn-log-view-region-revision))
	diff-args)
    (setq diff-args (list
		     (format "--new=%s" (cdr revs))
		     (format "--old=%s" (car revs))))
    (fsvn-diff-call-process diff-args args)))

(defun fsvn-log-view-ediff-with-region ()
  (let* ((revs (fsvn-log-view-region-revision))
	 (tmpfile1 (fsvn-ediff-make-temp-file (car revs)))
	 (tmpfile2 (fsvn-ediff-make-temp-file (cdr revs))))
    (unless (and (fsvn-save-file (cdr revs) tmpfile1 t)
		 (fsvn-save-file (car revs) tmpfile2 t))
      (error "Error occur while saving remote file"))
    (fsvn-ediff-files tmpfile1 tmpfile2)))

(defun fsvn-log-view-region-revision ()
  (let ((beg (region-beginning))
	(fin (region-end))
	(path fsvn-log-view-target-path)
	(root fsvn-buffer-repos-root)
	from-rev to-rev
	from-urlrev to-urlrev)
    (save-excursion
      (goto-char beg)
      (setq from-rev (fsvn-log-view-point-revision))
      (goto-char fin)
      (setq to-rev (fsvn-log-view-point-revision)))
    (unless (and (numberp from-rev)
		 (numberp to-rev))
      (error "Region terminated by unrevisioned line"))
    (when (> from-rev to-rev)
      (fsvn-swap from-rev to-rev))
    (setq from-urlrev (fsvn-log-view-revision-path root path from-rev))
    (setq to-urlrev (fsvn-log-view-revision-path root path to-rev))
    (cons from-urlrev to-urlrev)))

(defun fsvn-log-view-setup-window ()
  (delete-other-windows)
  (setq fsvn-default-window-configuration (current-window-configuration)))

(defun fsvn-log-view-revision-path (root path rev)
  (let ((found (fsvn-log-view-find-path rev path)))
    ;; todo found is path. currently this is works
    (fsvn-url-urlrev (fsvn-expand-url (fsvn-urlrev-url found) root) rev)))

(defun fsvn-logview-buffer-url ()
  (fsvn-expand-url fsvn-log-view-target-path fsvn-buffer-repos-root))

(defun fsvn-log-view-point-urlrev ()
  (let ((rev (fsvn-log-view-point-revision))
	(url (fsvn-log-view-point-url)))
    (when url
      (fsvn-url-urlrev url rev))))

(defun fsvn-log-view-point-url ()
  (let ((root fsvn-buffer-repos-root)
	(target-path (fsvn-log-view-point-path)))
    (when target-path
      (fsvn-expand-url target-path root))))

(defun fsvn-log-view-point-path ()
  (let ((path fsvn-log-view-target-path)
	(rev (fsvn-log-view-point-revision)))
    (when rev
      (fsvn-log-view-find-path rev path))))

(defun fsvn-log-view-find-path (rev path)
  (fsvn-log-chain-find rev path fsvn-log-view-all-entries))

(defun fsvn-log-view-set-subwindow-config ()
  (let* ((subconf fsvn-log-view-subwindow-settings)
	 (prevconf fsvn-default-window-configuration)
	 (root fsvn-buffer-repos-root))
    (with-current-buffer (fsvn-log-sibling-get-buffer)
      (setq fsvn-default-window-configuration subconf)
      (setq fsvn-previous-window-configuration prevconf))
    (with-current-buffer (fsvn-log-message-get-buffer)
      (setq fsvn-default-window-configuration subconf)
      (setq fsvn-previous-window-configuration prevconf)
      (when (and (fsvn-config-tortoise-property-use root)
		 (fsvn-url-local-p default-directory))
	(fsvn-tortoise-fontify-buffer)))))

(defun fsvn-log-view-cmd (urlrev root rev-range count)
  (let (real-count real-range matcher default-range-start)
    ;; limit must be
    (setq real-count
	  (cond
	   ((null count)
	    (fsvn-config-log-limit-count root))
	   ((eq t count) nil)
	   (t count)))
    (setq default-range-start (fsvn-urlrev-revision urlrev))
    (setq real-range (or rev-range (cons default-range-start 0)))
    (catch 'done
      (let (searched-entries tmp)
	(setq searched-entries (copy-sequence fsvn-log-view-all-entries))
	(cond
	 ((and (numberp (car real-range)) (numberp (cdr real-range))
	       (> (car real-range) (cdr real-range)))
	  (setq matcher (lambda (rev) (and (<= rev (car real-range))
					   (>= rev (cdr real-range))))))
	 ((and (numberp (car real-range)) (numberp (cdr real-range)))
	  (setq searched-entries (nreverse searched-entries))
	  (setq matcher (lambda (rev) (and (<= rev (cdr real-range))
					   (>= rev (car real-range))))))
	 ((and (numberp (car real-range)) (null (cdr real-range)))
	  (setq matcher (lambda (rev) (<= rev (car real-range)))))
	 ((and (null (car real-range)) (numberp (cdr real-range)))
	  (setq searched-entries (nreverse searched-entries))
	  (setq matcher (lambda (rev) (>= rev (cdr real-range))))))
	(when matcher
	  (mapc
	   (lambda (entry)
	     (let ((rev (fsvn-xml-log->logentry.revision entry)))
	       (when (funcall matcher rev)
		 (setq tmp (cons entry tmp))
		 (when (= (length tmp) real-count)
		   (throw 'done tmp)))))
	   searched-entries)))
      (with-temp-buffer
	(let (tmp)
	  (unless (= (fsvn-call-command
		      "log" (current-buffer)
		      "--xml"
		      "--verbose"
		      (when real-count
			(list "--limit" real-count))
		      "--revision" (fsvn-log-view-revision-range real-range)
		      urlrev) 0)
	    (error "Error while executing `svn log'"))
	  (setq tmp (fsvn-xml-parse-logentry))
	  ;; when rev-range: (nil . 100) -> "HEAD:100" but repository has rev.99
	  ;; this case return only 99 revision.
	  (when (and (= (length tmp) 1)
		     (not (funcall matcher (fsvn-xml-log->logentry.revision (car tmp)))))
	    (throw 'done nil))
	  tmp)))))

(defun fsvn-log-view-revision-range (range)
  "Ordered by revision descendant."
  (let (start end ret)
    (setq start (fsvn-get-revision-string (car range))
	  end (fsvn-get-revision-string (cdr range)))
    (cond
     ((and (null (car range)) (null (cdr range)))
      nil)
     ((and (car range) (null (cdr range)))
      (format "%s:%s" start "0"))
     ((and (null (car range)) (cdr range))
      (format "%s:%s" "HEAD" end))
     (t
      (format "%s:%s" start end)))))

;; * fsvn-log-view-mode interactive command

(defun fsvn-log-view-reload-with-change-limit ()
  (interactive)
  (let (count)
    (setq count
	  (fsvn-read-number "Log limit count: " (fsvn-config-log-limit-count fsvn-buffer-repos-root)))
    ;;todo consider rev-range arg
    (fsvn-open-log-view-mode fsvn-buffer-target-file fsvn-buffer-target-directory-p nil count)))

(defun fsvn-log-view-reload-with-revision (&optional range)
  (interactive (list (fsvn-completing-read-revision-range (fsvn-log-view-current-revision-range))))
  (fsvn-open-log-view-mode fsvn-buffer-target-file fsvn-buffer-target-directory-p range t))

(defun fsvn-log-view-save ()
  (interactive)
  (when fsvn-buffer-target-directory-p
    (error "\"%s\" is directory." fsvn-buffer-target-file))
  (let* ((url (fsvn-logview-buffer-url))
	 (rev (fsvn-log-view-point-revision))
	 (entry (fsvn-log-view-find-showing-entry rev))
	 file)
    (when (setq file (fsvn-log-read-save-file url rev))
      (fsvn-save-file-background url file rev))))

(defun fsvn-log-view-next-line (&optional arg)
  "Move down lines then position at log message.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line arg)
  (fsvn-log-view-after-move-line))

(defun fsvn-log-view-previous-line (&optional arg)
  "Move up lines then position at log message.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line (- arg))
  (fsvn-log-view-after-move-line))

(defun fsvn-log-view-next-mark ()
  (interactive)
  (when (re-search-forward fsvn-log-view-re-mark nil t)
    (fsvn-log-view-after-move-line)))

(defun fsvn-log-view-previous-mark ()
  (interactive)
  (let ((p
	 (save-excursion
	   (forward-line 0)
	   (when (re-search-backward fsvn-log-view-re-mark nil t)
	     (fsvn-log-view-after-move-line)
	     (point)))))
    (when p
      (goto-char p))))

(defun fsvn-log-view-mark-put-mark ()
  (interactive)
  (fsvn-log-view-put-mark-this-line fsvn-mark-mark-char)
  (fsvn-log-view-next-line))

(defun fsvn-log-view-mark-unmark ()
  (interactive)
  (fsvn-log-view-put-mark-this-line)
  (fsvn-log-view-next-line))

(defun fsvn-log-view-show-details ()
  (interactive)
  (let ((rev (fsvn-log-view-point-revision)))
    (if (not rev)
	(message "This line has no revision.")
      (fsvn-log-view-draw-details rev)
      (fsvn-log-view-setup-detail-windows)
      (setq fsvn-log-view-subwindow-settings (current-window-configuration))
      (fsvn-log-view-set-subwindow-config))))

(defun fsvn-log-view-scroll-message-up ()
  "When scroll reached to the top of sibling buffer, scroll the message buffer."
  (interactive)
  (fsvn-log-view-scroll-message-buffer t))

(defun fsvn-log-view-scroll-message-down ()
  "When scroll reached to the bottom of log message, scroll the sibling buffer."
  (interactive)
  (fsvn-log-view-scroll-message-buffer nil))

(defun fsvn-log-view-diff-generic (&optional args)
  "Diff current line with anything following case.
Mark is activated diff for region terminated revisions.
Otherwise diff at point revision with working copy file or directory.
"
  (interactive (fsvn-log-view-cmd-diff-arg))
  (cond
   (mark-active
    (fsvn-log-view-diff-with-region args))
   ((fsvn-url-repository-p fsvn-buffer-target-file)
    (error "This buffer has non working copy"))
   (t
    (fsvn-log-view-diff-with-wc args))))

(defun fsvn-log-view-ediff-generic ()
  (interactive)
  (when fsvn-buffer-target-directory-p
    (error "Cannot execute ediff.  This log list target is directory"))
  (cond
   (mark-active
    (fsvn-log-view-ediff-with-region))
   ((fsvn-url-repository-p fsvn-buffer-target-file)
    (error "This buffer has non working copy"))
   (t
    (fsvn-log-view-ediff-with-wc))))

(defun fsvn-log-view-diff-with-wc (&optional args)
  (interactive (fsvn-log-view-cmd-diff-arg))
  (let ((file fsvn-buffer-target-file)
	(rev (fsvn-log-view-point-revision))
	buffer diff-args)
    (setq diff-args (list "--revision" rev file))
    (fsvn-diff-call-process diff-args args)))

(defun fsvn-log-view-ediff-with-wc ()
  (interactive)
  (let* ((file fsvn-buffer-target-file)
	 (urlrev (fsvn-log-view-point-urlrev))
	 (tmpfile (fsvn-ediff-make-temp-file urlrev)))
    (when (fsvn-save-file urlrev tmpfile t)
      (fsvn-ediff-files tmpfile file))))

(defun fsvn-log-view-previous-log ()
  (interactive)
  (let ((range (fsvn-log-view-current-revision-range))
	new-range)
    (setq new-range
	  (cons (1- (cdr range)) nil))
    (fsvn-open-log-view-mode fsvn-buffer-target-file fsvn-buffer-target-directory-p new-range)))

(defun fsvn-log-view-next-log ()
  (interactive)
  (let ((range (fsvn-log-view-current-revision-range))
	new-range)
    (setq new-range
	  (cons nil (1+ (car range))))
    (fsvn-open-log-view-mode fsvn-buffer-target-file fsvn-buffer-target-directory-p new-range)))

(defun fsvn-log-view-toggle-details ()
  (interactive)
  (if (fsvn-log-view-subwindow-display-p)
      (delete-other-windows)
    (fsvn-log-view-show-details)))

(defun fsvn-log-view-isearch-text (text)
  (interactive
   (let* ((prompt (format "Search Text (%s): " (or (car fsvn-log-view-isarch-history) "")))
	  (readed (read-from-minibuffer prompt
					nil nil nil 'fsvn-log-view-isarch-history)))
     (list (if (string= readed "") (car fsvn-log-view-isarch-history) readed))))
  (let ((saved (point))
	(lst fsvn-log-view-entries)
	rev entry)
    (unless (catch 'exit
	      (fsvn-log-view-next-line)
	      (while (setq rev (fsvn-log-view-point-revision))
		(setq entry (fsvn-log-view-find-entry rev))
		(when (fsvn-xml-text-matched entry text)
		  (throw 'exit t))
		(fsvn-log-view-next-line)))
      (goto-char saved)
      (message "No message was matched."))))

(defun fsvn-log-view-mark-searched (text)
  (interactive (list (read-from-minibuffer
		      "Search Text: " nil nil nil 'fsvn-log-view-isarch-history)))
  (let ((entries (fsvn-log-view-matched-entries text)))
    (save-excursion
      (mapc
       (lambda (entry)
	 (when (fsvn-log-view-goto-revision (fsvn-xml-log->logentry.revision entry))
	   (fsvn-log-view-put-mark-this-line fsvn-mark-mark-char)))
       entries))))

;; FIXME quick hack function
(defun fsvn-log-view-edit-revprop ()
  (interactive)
  (let* ((urlrev (fsvn-log-view-point-urlrev))
	 (revprop (fsvn-read-revprop))
	 (value (read-from-minibuffer "Revision Property: " (fsvn-get-revprop revprop urlrev))))
    (when (y-or-n-p "Really change revision property? ")
      (fsvn-set-revprop-value urlrev revprop value))))

(defun fsvn-log-view-open-revision (urlrev)
  (interactive (list (fsvn-log-view-point-urlrev)))
  (fsvn-browse-switch-directory-buffer urlrev))

(defun fsvn-log-view-quit ()
  (interactive)
  (fsvn-restore-window-buffer
   (kill-buffer (current-buffer))))

(defun fsvn-log-view-copy-urlrev ()
  (interactive)
  (let ((urlrev (fsvn-log-view-point-urlrev)))
    (kill-new urlrev)
    (message urlrev)))


(defconst fsvn-log-sibling-buffer-name "*Fsvn Sibling*")
(defvar fsvn-log-sibling-paths nil)

(defvar fsvn-log-sibling-diff-map nil)
(unless fsvn-log-sibling-diff-map
  (setq fsvn-log-sibling-diff-map
	(let ((map (make-sparse-keymap)))
	  (suppress-keymap map)

	  (define-key map "=" 'fsvn-log-sibling-diff-previous)
	  (define-key map "e" 'fsvn-log-sibling-ediff-previous)

	  map)))

(defvar fsvn-log-sibling-mode-map nil)
(unless fsvn-log-sibling-mode-map
  (setq fsvn-log-sibling-mode-map
	(let ((map (make-sparse-keymap)))
	  (suppress-keymap map)

	  (fsvn-readonly-mode-keymap map)

	  (define-key map "=" fsvn-log-sibling-diff-map)
	  (define-key map "S" 'fsvn-log-sibling-save)
	  (define-key map "\C-c\C-d" 'fsvn-log-sibling-open-this-directory)
	  (define-key map "\C-c\C-k" 'fsvn-restore-previous-window-setting)
	  (define-key map "\C-c\C-l" 'fsvn-restore-default-window-display)
	  (define-key map "\C-c\C-o" 'fsvn-log-subwindow-switch-to-view)
	  (define-key map "\C-cl" 'fsvn-log-sibling-log-this)
	  (define-key map "\C-m" 'fsvn-log-sibling-open-this)
	  (define-key map "\C-n" 'fsvn-log-sibling-next-line)
	  (define-key map "\C-p" 'fsvn-log-sibling-previous-line)
	  (define-key map "C" 'fsvn-log-sibling-copy-this)
	  (define-key map "l" 'fsvn-log-sibling-log-this)
	  (define-key map "n" 'fsvn-log-sibling-next-line)
	  (define-key map "p" 'fsvn-log-sibling-previous-line)
	  map)))

(defcustom fsvn-log-sibling-mode-hook nil
  "*Run at the very end of `fsvn-log-sibling-mode'."
  :group 'fsvn
  :type 'hook)

(defvar fsvn-log-sibling-font-lock-keywords nil)
(setq fsvn-log-sibling-font-lock-keywords
      (list
       ))

(defconst fsvn-log-sibling-buffer-local-variables
  '(
    (font-lock-defaults . '(fsvn-log-sibling-font-lock-keywords t nil nil beginning-of-line))
    (fsvn-log-sibling-paths)
    (fsvn-buffer-repos-root)
    (fsvn-log-source-buffer)
    (fsvn-log-sibling-target-path)
    (fsvn-log-sibling-logentry)
    (fsvn-buffer-revision)
    (fsvn-log-sibling-font-lock-keywords)
    ))

(defvar fsvn-log-sibling-target-path nil)
(defvar fsvn-log-sibling-logentry nil)

(defun fsvn-log-sibling-mode ()
  "Major mode for browsing Subversion log siblings.

Entry to this mode calls the value of `fsvn-log-sibling-mode-hook'.

Keybindings:
\\{fsvn-log-sibling-mode-map}

"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-log-sibling-mode-map)
  (setq major-mode 'fsvn-log-sibling-mode)
  (setq mode-name "Fsvn Log Sibling")
  (setq truncate-lines t)
  (fsvn-make-buffer-variables fsvn-log-sibling-buffer-local-variables))

(defmacro fsvn-log-sibling-only-file (&rest form)
  `(if (null (fsvn-log-sibling-point-url))
       (message "No file on this line.")
     ,@form))

(defmacro fsvn-log-sibling-diffable (&rest form)
  `(let ((URLREV (fsvn-log-sibling-point-urlrev))
	 (PREV-URLREV (fsvn-log-sibling-point-prev-urlrev)))
     (if (or (null URLREV) (null PREV-URLREV))
	 (message "This line has no previous version.")
       ,@form)))

(defun fsvn-log-sibling-get-local-filename ()
  "Guessed working copy file."
  (let* ((point (fsvn-log-sibling-point-url)))
    (when (and (fsvn-url-local-p default-directory)
	       (fsvn-directory-versioned-p default-directory))
      (let* ((info (fsvn-get-info-entry default-directory))
	     ;;todo big bug. fsvn-xml-info->entry=>url$ contains space url is encoded.
	     (current (fsvn-xml-info->entry=>url$ info))
	     (filename (fsvn-url-relative-name current point)))
	(when (file-exists-p filename)
	  (fsvn-expand-file filename))))))

(defun fsvn-log-sibling-get-buffer ()
  (get-buffer-create fsvn-log-sibling-buffer-name))

(defun fsvn-log-sibling-point-path ()
  (save-excursion
    (forward-line 0)
    (when (looking-at "^..\\(.+\\)")
      (match-string-no-properties 1))))

(defun fsvn-log-sibling-point-status ()
  (save-excursion
    (forward-line 0)
    (char-after)))

(defun fsvn-log-sibling-point-url ()
  (let ((path (fsvn-log-sibling-point-path))
	(root (directory-file-name fsvn-buffer-repos-root)))
    (when path
      (concat root path))))

(defun fsvn-log-sibling-point-urlrev ()
  (let ((url (fsvn-log-sibling-point-url))
	(rev fsvn-buffer-revision))
    (when url
      (fsvn-url-urlrev url rev))))

(defun fsvn-log-sibling-point-prev-urlrev ()
  (cond
   ((memq (fsvn-log-sibling-point-status) '(?M))
    (let* ((urlrev (fsvn-log-sibling-point-urlrev))
	  (prev (1- fsvn-buffer-revision))
	  (prev-urlrev (fsvn-url-urlrev (fsvn-log-sibling-point-url) prev)))
      prev-urlrev))
   ((memq (fsvn-log-sibling-point-status) '(?A)) ; moved file
    ;; path-entry must be found.
    (let* ((path-entry (fsvn-find-logentry-path (fsvn-log-sibling-point-path) fsvn-log-sibling-logentry))
	   (path (fsvn-xml-log->logentry->path.copyfrom-path path-entry))
	   (rev (fsvn-xml-log->logentry->paths->path.copyfrom-rev path-entry))
	   url)
      (when (and path rev)
	(setq url (fsvn-expand-url path fsvn-buffer-repos-root))
	(fsvn-url-urlrev url rev))))))

(defun fsvn-log-sibling-target-file ()
  (with-current-buffer fsvn-log-source-buffer
    fsvn-buffer-target-file))

(defun fsvn-log-sibling-target-directory-p ()
  (with-current-buffer fsvn-log-source-buffer
    fsvn-buffer-target-directory-p))

(defun fsvn-log-sibling-cmd-read-copy-file ()
  (let ((from (fsvn-log-sibling-point-urlrev))
	filename target to args initial)
    (unless from
      (error "No file on this line"))
    (setq filename (fsvn-urlrev-filename from))
    (setq target (fsvn-log-sibling-target-file))
    (setq initial 
	  (fsvn-expand-file 
	   filename
	   (cond
	    ((not (fsvn-url-local-p target))
	     "~/")
	    ((fsvn-log-sibling-target-directory-p)
	     target)
	    (t
	     (fsvn-file-name-directory2 target)))))
    (setq to (fsvn-read-file-under-versioned "Copy To: " initial))
    (setq args (fsvn-browse-cmd-copy/move-read-args "copy" 'fsvn-default-args-copy))
    (list from to args)))

;; * fsvn-log-sibling-mode interactive command

(defun fsvn-log-sibling-next-line (&optional arg)
  "Move to next line."
  (interactive "p")
  (forward-line arg))

(defun fsvn-log-sibling-previous-line (&optional arg)
  "Move to previous line."
  (interactive "p")
  (forward-line (- arg)))

(defun fsvn-log-sibling-log-this (&optional arg)
  "Open file log as `fsvn-log-view-mode'.
If guessed working copy file exists, this file as log target.
Optional ARG means force to access repository by certainly correct url."
  (interactive "P")
  (fsvn-log-sibling-only-file
   (let ((local-file (fsvn-log-sibling-get-local-filename)))
     (if (and (null arg) local-file)
	 (fsvn-open-log-view-mode local-file (fsvn-file-exact-directory-p local-file))
       (let* ((urlrev (fsvn-log-sibling-point-urlrev))
	      (info (fsvn-get-info-entry urlrev)))
	 (fsvn-open-log-view-mode urlrev (eq (fsvn-xml-info->entry.kind info) 'dir)))))))

(defun fsvn-log-sibling-save ()
  "Save current point url to local file."
  (interactive)
  (fsvn-log-sibling-only-file
   (let* ((url (fsvn-log-sibling-point-url))
	  (rev fsvn-buffer-revision)
	  filename file)
     (setq file (fsvn-log-read-save-file url rev))
     (when file
       (fsvn-save-file-background (fsvn-url-urlrev url rev) file)))))

(defun fsvn-log-sibling-diff-previous (&optional args)
  "Diff with previous version."
  (interactive (fsvn-log-view-cmd-diff-arg))
  (fsvn-log-sibling-diffable
   (let* (diff-args)
     (setq diff-args (list (format "--new=%s" URLREV)
			   (format "--old=%s" PREV-URLREV)))
     (fsvn-diff-call-process diff-args args))))

(defun fsvn-log-sibling-ediff-previous ()
  "Ediff with previous version."
  (interactive)
  (fsvn-log-sibling-diffable
   (let* ((file1 (fsvn-ediff-make-temp-file URLREV))
	  (file2 (fsvn-ediff-make-temp-file PREV-URLREV)))
     (unless (and (fsvn-save-file URLREV file1 t)
		  (fsvn-save-file PREV-URLREV file2 t))
       (error "Error occur while saving remote file"))
     (fsvn-ediff-files file1 file2))))

(defun fsvn-log-sibling-open-this ()
  "Open guessed working copy file."
  (interactive)
  ;;TODO open repository file when prev revision?
  (fsvn-log-sibling-only-file
   (let* ((filename (fsvn-log-sibling-get-local-filename)))
     (unless filename
       (error "File not exists."))
     (find-file filename))))

(defun fsvn-log-sibling-open-this-directory ()
  "Open guessed working copy file's directory."
  (interactive)
  (fsvn-log-sibling-only-file
   (let* ((filename (fsvn-log-sibling-get-local-filename)))
     (unless filename
       (error "File not exists."))
     (fsvn-working-copy (file-name-directory filename)))))

(defun fsvn-log-sibling-copy-this (src-urlrev dest-file &optional args)
  "Execute `copy' for point SRC-URLREV to DEST-FILE.
Optional ARGS (with prefix arg) means read svn subcommand arguments.
"
  (interactive (fsvn-log-sibling-cmd-read-copy-file))
  (fsvn-start-copy/move-process-with-popup "copy" (list src-urlrev) dest-file args))



(defconst fsvn-log-message-buffer-name "*Fsvn Message*")
(defconst fsvn-log-message-buffer-local-variables
  '(
    (fsvn-buffer-repos-root)
    (fsvn-log-source-buffer)
    (fsvn-log-message-revision)
    ))

(defvar fsvn-log-message-font-lock-keywords nil)
(setq fsvn-log-message-font-lock-keywords
      (list
       ))

(defvar fsvn-log-message-revision nil)

(defvar fsvn-log-message-mode-map nil)
(unless fsvn-log-message-mode-map
  (setq fsvn-log-message-mode-map
	(let ((map (make-sparse-keymap)))
	  (set-keymap-parent map text-mode-map)

	  (define-key map "\C-cj" 'fsvn-log-message-browse-this)
	  (define-key map "\C-c\C-k" 'fsvn-restore-previous-window-setting)
	  (define-key map "\C-c\C-l" 'fsvn-restore-default-window-display)
	  (define-key map "\C-c\C-o" 'fsvn-log-switch-to-sibling)
	  (define-key map "\C-c\C-c" 'fsvn-log-message-commit)
	  (define-key map "\C-c\C-e" 'fsvn-log-message-start-edit)
	  (define-key map "\C-c\C-q" 'fsvn-log-message-quit-edit)

	  map)))

(defcustom fsvn-log-message-mode-hook nil
  "*Run at the very end of `fsvn-log-message-mode'."
  :group 'fsvn
  :type 'hook)

;; * fsvn-log-message-mode internal function

(defun fsvn-log-message-mode ()
  "Major mode for viewing and editing Subversion log message.

Entry to this mode calls the value of `fsvn-log-message-mode-hook'.

Keybindings:
\\{fsvn-log-message-mode-map}

"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-log-message-mode-map)
  (setq major-mode 'fsvn-log-message-mode)
  (setq mode-name "Fsvn Log Message")
  (setq truncate-lines t)
  (fsvn-make-buffer-variables fsvn-log-message-buffer-local-variables)
  (font-lock-mode 0))

(defun fsvn-log-message-get-buffer ()
  (get-buffer-create fsvn-log-message-buffer-name))

;; * fsvn-log-message-mode interactive command

(defun fsvn-log-message-browse-this ()
  (interactive)
  (let ((prop (get-text-property (point) 'fsvn-url-link)))
    (unless prop
      (error "No url here."))
    (browse-url prop)))

(defun fsvn-log-message-start-edit ()
  (interactive)
  (setq buffer-read-only nil)
  (force-mode-line-update)
  (message
   (substitute-command-keys (concat "Type \\[fsvn-log-message-commit] to finish edit, "))))

(defun fsvn-log-message-quit-edit ()
  (interactive)
  (when (buffer-modified-p)
    (error "Log message unchanged."))
  (setq buffer-read-only t)
  (force-mode-line-update))

(defun fsvn-log-message-commit ()
  "Commit changed log message."
  (interactive)
  (cond
   ((not (buffer-modified-p))
    (message "Message was not changed."))
   ((not (y-or-n-p "Really commit changed log? "))
    )
   (t
    (let ((tmpfile (fsvn-get-prop-temp-file "svn:log" (buffer-substring (point-min) (point-max)))))
      (fsvn-call-process-with-popup "propset"
				    "--file" tmpfile
				    "--revprop" "svn:log"
				    "--revision" fsvn-log-message-revision
				    fsvn-buffer-repos-root))
    )))



;; fsvn-log-*-mode utility

(defvar fsvn-log-source-buffer nil)

(defun fsvn-log-read-save-file (url rev)
  (let (filename rev-name file)
    (setq filename (fsvn-url-decode-string (fsvn-urlrev-filename url)))
    (setq rev-name (fsvn-file-name-as-revisioned filename rev))
    (setq file (read-file-name "Save as: " nil nil nil rev-name))
    (when (or (not (file-exists-p file))
	      (y-or-n-p "File exists. overwrite? "))
      (fsvn-expand-file file))))

(defun fsvn-log-subwindow-switch-to-view ()
  (interactive)
  (fsvn-switch-buffer-window fsvn-log-source-buffer t))

(defun fsvn-log-switch-to-sibling ()
  (interactive)
  (fsvn-switch-buffer-window (get-buffer fsvn-log-sibling-buffer-name) t))

(defun fsvn-log-switch-to-message ()
  (interactive)
  (fsvn-switch-buffer-window (get-buffer fsvn-log-message-buffer-name) t))

(put 'fsvn-log-view-each-rev 'lisp-indent-function 1)

(provide 'fsvn-logview)

;;; fsvn-log.el ends here
