;;; fsvn-logview.el --- Subversion log view utility for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-mode)



(defvar text-mode-map)
(defvar current-prefix-arg)



;; variable definition of log-list, log-sibling, log-message

(defvar fsvn-logview-target-urlrev nil)
(defvar fsvn-logview-target-directory-p nil)



(defconst fsvn-log-list-buffer-local-variables
  '(
    (font-lock-defaults . '(fsvn-log-list-font-lock-keywords t nil nil beginning-of-line))
    (fsvn-buffer-repos-root)
    (fsvn-logview-target-urlrev)
    (fsvn-logview-target-directory-p)
    (fsvn-log-list-target-path)
    (fsvn-log-list-entries)
    (fsvn-log-list-all-entries)
    (fsvn-log-list-subwindow-settings)
    (revert-buffer-function . 'fsvn-log-list-revert-buffer)
    ))

(defconst fsvn-log-list-revision-length 7)
(defconst fsvn-log-list-re-mark "^[^ \n]")
(defconst fsvn-log-list-re-terms
  "^ \\(Terms of entries:\\) \\(.+\\)")

(defconst fsvn-log-list-re-target
  "^ \\(Logs for\\) \\(.+\\)")

(defconst fsvn-log-list-user-name-length 15)
(defconst fsvn-log-list-message-length 50)
(defconst fsvn-log-list-buffer-name-prefix "Log for ")

(defvar fsvn-log-list-target-path nil)
(defvar fsvn-log-list-subwindow-settings nil)
(defvar fsvn-log-list-entries nil
  "Shown logs sorted by revision.")
(defvar fsvn-log-list-all-entries nil
  "logentry of has been shown. sorted by revision.")
(defvar fsvn-log-list-isarch-history nil)

(defvar fsvn-log-list-font-lock-keywords nil)
(setq fsvn-log-list-font-lock-keywords
      (list
       (list fsvn-log-list-re-terms '(1 fsvn-header-key-face) '(2 fsvn-header-face))
       (list fsvn-log-list-re-target '(1 fsvn-header-key-face) '(2 fsvn-header-face))
       (list "^. \\( *[0-9]+\\)" '(1 fsvn-keyname-face))
       (list fsvn-log-list-re-mark '(0 fsvn-mark-face))
       (list (concat "^[" (char-to-string fsvn-mark-mark-char) "]")
	     '(".+" (fsvn-log-list-move-to-date) nil (0 fsvn-marked-face)))
       ))

(defvar fsvn-log-list-diff-mode-map nil)
(unless fsvn-log-list-diff-mode-map
  (setq fsvn-log-list-diff-mode-map
	(let ((map (make-sparse-keymap)))
	  (suppress-keymap map)

	  (define-key map "=" 'fsvn-log-list-diff-generic)
	  (define-key map "e" 'fsvn-log-list-ediff-generic)
	  (define-key map "w" 'fsvn-log-list-diff-with-wc)

	  map)))

(defvar fsvn-log-list-mode-map nil)
(unless fsvn-log-list-mode-map
  (setq fsvn-log-list-mode-map
	(let ((map (make-sparse-keymap)))
	  (suppress-keymap map)

	  (fsvn-readonly-mode-keymap map)

	  (define-key map " " 'fsvn-log-list-scroll-message-up)
	  (define-key map "%" (make-sparse-keymap))
	  (define-key map "%m" 'fsvn-log-list-mark-searched)
	  (define-key map "<" 'fsvn-log-list-next-log)
	  (define-key map "=" fsvn-log-list-diff-mode-map)
	  (define-key map ">" 'fsvn-log-list-previous-log)
	  (define-key map "L" 'fsvn-log-list-reload-with-change-limit)
	  (define-key map "N" 'fsvn-log-list-next-mark)
	  (define-key map "P" 'fsvn-log-list-previous-mark)
	  (define-key map "R" 'fsvn-log-list-reload-with-revision)
	  (define-key map "S" 'fsvn-log-list-save-this)
	  (define-key map "\C-c\C-k" 'fsvn-log-list-quit)
	  (define-key map "\C-c\C-m" 'fsvn-log-list-open-revision)
	  (define-key map "\C-c\C-o" 'fsvn-log-switch-to-message)
	  (define-key map "\C-c\C-q" 'fsvn-log-list-quit)
	  (define-key map "\C-c\C-r" 'fsvn-log-list-edit-revprop)
	  (define-key map "\C-m" 'fsvn-log-list-show-details)
	  (define-key map "\C-n" 'fsvn-log-list-next-line)
	  (define-key map "\C-p" 'fsvn-log-list-previous-line)
	  (define-key map "\d" 'fsvn-log-list-scroll-message-down)
	  (define-key map "m" 'fsvn-log-list-mark-put-mark)
	  (define-key map "n" 'fsvn-log-list-next-line)
	  (define-key map "p" 'fsvn-log-list-previous-line)
	  (define-key map "q" 'fsvn-log-list-quit)
	  (define-key map "s" 'fsvn-log-list-isearch-text)
	  (define-key map "u" 'fsvn-log-list-mark-unmark)
	  (define-key map "v" 'fsvn-log-list-toggle-details)
	  (define-key map "w" 'fsvn-log-list-copy-urlrev)
	  
	  (define-key map "g" 'revert-buffer)

	  ;;todo not implement
	  ;; 	(define-key map "U" 'fsvn-log-list-mark-unmark-all)
	  ;; 	(define-key map "z\C-p" 'fsvn-log-list-open-propview)

	  map)))

(defcustom fsvn-log-list-mode-hook nil
  "*Run at the very end of `fsvn-log-list-mode'."
  :group 'fsvn
  :type 'hook)

;; * fsvn-log-list-mode internal function

(defun fsvn-log-list-mode ()
  "Major mode for browsing Subversion log.

Entry to this mode calls the value of `fsvn-log-list-mode-hook'.

Keybindings:
\\{fsvn-log-list-mode-map}

"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-log-list-mode-map)
  (setq major-mode 'fsvn-log-list-mode)
  (setq mode-name "Fsvn Log View")
  (setq truncate-lines t)
  (setq buffer-undo-list t)
  (fsvn-make-buffer-variables fsvn-log-list-buffer-local-variables))

(defmacro fsvn-log-list-each-rev (entry &rest form)
  `(save-excursion
     (fsvn-log-list-goto-first-revision)
     (let (REV)
       (while (setq REV (fsvn-log-list-point-revision))
	 (setq ,entry (fsvn-log-list-find-entry REV))
	 ,@form
	 (fsvn-log-list-next-line)))))

(defun fsvn-log-list-scroll-message-buffer (arg)
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
     ((> arg 0)
      (setq scroll arg
	    scroller 'fsvn-scroll-window-buffer-up))
     ((< arg 0)
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

(defun fsvn-log-list-draw-details (&optional revision)
  (let* ((rev (or revision (fsvn-log-list-point-revision)))
	 (dir default-directory)
	 (root fsvn-buffer-repos-root)
	 (path (fsvn-log-list-point-path))
	 (main-buffer (current-buffer))
	 logentry)
    (when rev
      (setq logentry (fsvn-log-list-find-showing-entry rev)))
    (with-current-buffer (fsvn-log-message-get-buffer)
      (let ((msg (fsvn-xml-log->logentry=>msg$ logentry)))
      	(fsvn-set-default-directory dir)
      	(let (buffer-read-only)
      	  (erase-buffer)
      	  (when msg
      	    (insert msg)))
      	(fsvn-log-message-mode)
      	(setq fsvn-buffer-repos-root root)
      	(setq fsvn-log-source-buffer main-buffer)
      	(setq fsvn-log-message-revision rev)
      	(setq buffer-read-only t)
      	(run-mode-hooks 'fsvn-log-message-mode-hook)))
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
      	  		  (fsvn-url-contains-p text path))
      	  	 ;;FIXME regexp not correct
      	  	 (setq regexp (format "^[AD] \\(%s\\|%s\\)$" text (fsvn-url-decode-string copied))))))
      	   (fsvn-log-sibling-sorted-paths logentry)))
      	(fsvn-log-sibling-mode)
      	(when (and path (null regexp))
      	  (setq regexp (concat "^..\\(" (regexp-quote (fsvn-url-decode-string path)) "\\)\\(?:/\\|$\\)")))
      	(setq fsvn-buffer-repos-root root)
      	(setq fsvn-log-source-buffer main-buffer)
      	(setq fsvn-log-sibling-target-path path)
      	(setq fsvn-log-sibling-logentry logentry)
      	(setq fsvn-log-sibling-revision rev)
      	(setq fsvn-log-sibling-font-lock-keywords
      	      (when regexp
      		(list (list regexp '(1 fsvn-header-key-face))))))
      (setq buffer-read-only t)
      (run-mode-hooks 'fsvn-log-sibling-mode-hook))))

(defun fsvn-log-list-find-showing-entry (rev)
  (fsvn-logs-find-logentry fsvn-log-list-entries rev))

(defun fsvn-log-list-find-entry (rev)
  (fsvn-logs-find-logentry fsvn-log-list-all-entries rev))

(defun fsvn-log-list-point-revision ()
  (save-excursion
    (forward-line 0)
    (when (looking-at "^.. *\\([0-9]+\\)")
      (string-to-number (match-string 1)))))

(defun fsvn-log-list-put-mark-this-line (&optional mark)
  (let (buffer-read-only)
    (save-excursion
      (forward-line 0)
      (delete-char 1)
      (insert (or mark fsvn-space-char)))))

(defun fsvn-log-list-revert-buffer (ignore-auto noconfirm)
  (let ((range (fsvn-log-list-current-revision-range))
	(rev (fsvn-log-list-point-revision)))
    (mapc
     (lambda (entry)
       (setq fsvn-log-list-all-entries (delq entry fsvn-log-list-all-entries)))
     fsvn-log-list-entries)
    (fsvn-open-log-view-mode fsvn-logview-target-urlrev fsvn-logview-target-directory-p range)
    (when rev
      (fsvn-log-list-goto-revision rev))))

(defun fsvn-log-list-entry-revision (entry)
  (fsvn-string-lpad (number-to-string (fsvn-xml-log->logentry.revision entry))
		    fsvn-log-list-revision-length))

(defun fsvn-log-list-entry-message (entry)
  (let* ((msg (fsvn-xml-log->logentry=>msg$ entry))
	 (ret
	  (if msg
	      (fsvn-string-truncate (fsvn-string-single-line msg) fsvn-log-list-message-length t)
	    (make-string 0 0))))
    (fsvn-log-list-message-property ret)))

(defun fsvn-log-list-entry-user (entry)
  (fsvn-string-rpad (fsvn-xml-log->logentry=>author$ entry) fsvn-log-list-user-name-length))

(defun fsvn-log-list-entry-action (entry)
  (fsvn-string-rpad (fsvn-xml-log->logentry->paths->path.action entry) 5))

(defun fsvn-log-list-entry-date (entry)
  (let ((ret (format-time-string "%Y-%m-%d %H:%M:%S" (fsvn-xml-log->logentry=>date$ entry))))
    (fsvn-log-list-date-property ret)))

(defun fsvn-log-list-insert-entry (entry)
  (insert (format "  %s %s %s %s %s\n"
		  (fsvn-log-list-entry-revision entry)
		  (fsvn-log-list-entry-action entry)
		  (fsvn-log-list-entry-user entry)
		  (fsvn-log-list-entry-date entry)
		  (fsvn-log-list-entry-message entry)
		  )))

(defun fsvn-log-list-insert-header-entry (file first last)
  (insert (format " Logs for %s\n" file))
  (insert (format " Terms of entries: %s - %s\n"
		  (format-time-string "%Y-%m-%d" (fsvn-xml-log->logentry=>date$ first))
		  (format-time-string "%Y-%m-%d" (fsvn-xml-log->logentry=>date$ last))
		  ))
  (insert "\n"))

(defun fsvn-log-list-setup-detail-windows ()
  (let ((message-buffer (fsvn-log-message-get-buffer))
	(sibling-buffer (fsvn-log-sibling-get-buffer))
	first-win second-win third-win)
    (delete-other-windows)
    (setq first-win (get-buffer-window (current-buffer)))
    (setq second-win (split-window first-win (/ (window-height first-win) 4)))
    (set-window-buffer second-win message-buffer)
    (setq third-win (split-window second-win (/ (window-height second-win) 2)))
    (set-window-buffer third-win sibling-buffer)))

(defun fsvn-log-list-get-buffer (urlrev)
  (let ((bufs (buffer-list))
	target)
    (while bufs
      (with-current-buffer (car bufs)
	(when (eq major-mode 'fsvn-log-list-mode)
	  (when (string= fsvn-logview-target-urlrev urlrev)
	    (setq target (car bufs))
	    (setq bufs nil))))
      (setq bufs (cdr bufs)))
    (if target
	target
      (generate-new-buffer (concat fsvn-log-list-buffer-name-prefix
				   "[" (fsvn-url-filename urlrev) "]")))))

(defun fsvn-log-list-move-to-message ()
  (let ((eol (line-end-position)))
    (beginning-of-line)
    (let ((change (next-single-property-change (point) 'fsvn-log-message nil eol)))
      (cond
       ((and change (< change eol))
	(goto-char change))
       ((or (looking-at fsvn-log-list-re-target)
	    (looking-at fsvn-log-list-re-terms))
	)
       (t
	;;for none log message
	(goto-char eol))))))

(defun fsvn-log-list-move-to-date ()
  (let ((eol (line-end-position)))
    (beginning-of-line)
    (let ((change (next-single-property-change (point) 'fsvn-log-date nil eol)))
      (cond
       ((and change (< change eol))
	(goto-char change))))))

(defun fsvn-log-list-message-property (message)
  (fsvn-string-put-property message 'fsvn-log-message t))

(defun fsvn-log-list-date-property (date)
  (fsvn-string-put-property date 'fsvn-log-date t))

(defun fsvn-log-list-subwindow-display-p ()
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (or (memq (get-buffer fsvn-log-message-buffer-name) buffers)
	(memq (get-buffer fsvn-log-sibling-buffer-name) buffers))))

(defun fsvn-log-list-goto-first-revision ()
  (let ((saved (point)))
    (goto-char (point-min))
    (if (catch 'found
	  (while (not (eobp))
	    (when (fsvn-log-list-point-revision)
	      (throw 'found t))
	    (forward-line 1)))
	(fsvn-log-list-move-to-message)
      (goto-char saved))))

(defun fsvn-log-list-goto-revision (rev)
  (let ((point (catch 'found
		 (save-excursion
		   (fsvn-log-list-goto-first-revision)
		   (let (cur)
		     (while (setq cur (fsvn-log-list-point-revision))
		       (when (= cur rev)
			 (fsvn-log-list-move-to-message)
			 (throw 'found (point)))
		       (forward-line 1)))))))
    (when point
      (goto-char point))))

(defun fsvn-log-list-after-move-line ()
  (fsvn-log-list-move-to-message)
  (when (fsvn-log-list-subwindow-display-p)
    (fsvn-log-list-draw-details)
    (fsvn-log-list-set-subwindow-config)))

(defun fsvn-logview-cmd-read-diff-args ()
  (list (fsvn-cmd-read-subcommand-args "diff" fsvn-default-args-diff)))

(defun fsvn-log-list-matched-entries (text)
  (let (ret)
    (mapc
     (lambda (entry)
       (when (fsvn-xml-text-matched entry text)
	 (setq ret (cons entry ret))))
     fsvn-log-list-entries)
    (nreverse ret)))

(defun fsvn-log-list-current-revision-range ()
  (let ((first (car fsvn-log-list-entries))
	(last (car (last fsvn-log-list-entries))))
    (cons (fsvn-xml-log->logentry.revision first)
	  (fsvn-xml-log->logentry.revision last))))

(defun fsvn-log-list-diff-with-region (args)
  (let ((revs (fsvn-log-list-region-revision))
	diff-args)
    (setq diff-args (list
		     (format "--new=%s" (cdr revs))
		     (format "--old=%s" (car revs))))
    (fsvn-diff-start-process diff-args args)))

(defun fsvn-log-list-ediff-with-region ()
  (let* ((revs (fsvn-log-list-region-revision))
	 (tmpfile1 (fsvn-ediff-make-temp-file (car revs)))
	 (tmpfile2 (fsvn-ediff-make-temp-file (cdr revs))))
    (unless (and (fsvn-save-file (cdr revs) tmpfile1 t)
		 (fsvn-save-file (car revs) tmpfile2 t))
      (error "Error occur while saving remote file"))
    (fsvn-ediff-files tmpfile1 tmpfile2)))

(defun fsvn-log-list-region-revision ()
  (let ((beg (region-beginning))
	(fin (region-end))
	(path fsvn-log-list-target-path)
	(root fsvn-buffer-repos-root)
	from-rev to-rev
	from-urlrev to-urlrev)
    (save-excursion
      (goto-char beg)
      (setq from-rev (fsvn-log-list-point-revision))
      (goto-char fin)
      (setq to-rev (fsvn-log-list-point-revision)))
    (unless (and (numberp from-rev)
		 (numberp to-rev))
      (error "Region terminated by unrevisioned line"))
    (when (> from-rev to-rev)
      (fsvn-swap from-rev to-rev))
    (setq from-urlrev (fsvn-log-list-revision-path root path from-rev))
    (setq to-urlrev (fsvn-log-list-revision-path root path to-rev))
    (cons from-urlrev to-urlrev)))

(defun fsvn-log-list-setup-window ()
  (delete-other-windows)
  (setq fsvn-default-window-configuration (current-window-configuration)))

(defun fsvn-log-list-revision-path (root path rev)
  (let ((found (fsvn-log-list-find-path rev path)))
    ;; todo found is path. currently this is works
    (fsvn-url-urlrev (fsvn-expand-url (fsvn-urlrev-url found) root) rev)))

(defun fsvn-log-list-repository-url ()
  (fsvn-expand-url fsvn-log-list-target-path fsvn-buffer-repos-root))

(defun fsvn-log-list-point-urlrev ()
  (let ((rev (fsvn-log-list-point-revision))
	(url (fsvn-log-list-point-url)))
    (when url
      (fsvn-url-urlrev url rev))))

(defun fsvn-log-list-point-url ()
  (let ((root fsvn-buffer-repos-root)
	(target-path (fsvn-log-list-point-path)))
    (when target-path
      (fsvn-expand-url target-path root))))

(defun fsvn-log-list-point-path ()
  (let ((path fsvn-log-list-target-path)
	(rev (fsvn-log-list-point-revision)))
    (when rev
      (fsvn-log-list-find-path rev path))))

(defun fsvn-log-list-find-path (rev path)
  (fsvn-logs-chain-find fsvn-log-list-all-entries rev path))

(defun fsvn-log-list-set-subwindow-config ()
  (let* ((subconf fsvn-log-list-subwindow-settings)
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

(defun fsvn-log-list-cmd (urlrev root rev-range count)
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
	(setq searched-entries (copy-sequence fsvn-log-list-all-entries))
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
		      "--revision" (fsvn-log-list-revision-range real-range)
		      urlrev) 0)
	    (error "Error while executing `svn log'"))
	  (setq tmp (fsvn-xml-parse-logentry))
	  ;; when rev-range: (nil . 100) -> "HEAD:100" but repository has rev.99
	  ;; this case return only 99 revision.
	  (when (and (= (length tmp) 1)
		     (not (funcall matcher (fsvn-xml-log->logentry.revision (car tmp)))))
	    (throw 'done nil))
	  tmp)))))

(defun fsvn-log-list-revision-range (range)
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

(defun fsvn-log-list-cmd-revision ()
  (let ((rev (fsvn-log-list-point-revision)))
    (unless rev
      (error "This line has no revision."))
    rev))

(defun fsvn-log-list-cmd-urlrev ()
  (let ((urlrev (fsvn-log-list-point-urlrev)))
    (unless urlrev
      (error "This line has no revision."))
    urlrev))

(defun fsvn-log-list-cmd-read-revert-to-revision ()
  (let ((path fsvn-logview-target-urlrev)
	(urlrev (fsvn-log-list-cmd-urlrev)))
    (unless (fsvn-url-local-p path)
      (error "This log has no local relation."))
    (unless (y-or-n-p (format 
		       "Revert `%s' to revision %s? " 
		       (fsvn-file-name-nondirectory path)
		       (fsvn-urlrev-revision urlrev)))
      (error "quit"))
    (list urlrev path)))
  
(defun fsvn-log-list-cmd-read-urlrev ()
  (list (fsvn-log-list-cmd-urlrev)))

(defun fsvn-log-list-cmd-read-revision ()
  (list (fsvn-log-list-cmd-revision)))

(defun fsvn-log-list-cmd-read-save-this ()
  (when fsvn-logview-target-directory-p
    (error "\"%s\" is directory." fsvn-logview-target-urlrev))
  (let* ((urlrev fsvn-logview-target-urlrev) ;;BUG consider using urlrev
	 (rev (fsvn-log-list-point-revision))
	 (file (fsvn-log-read-save-file (fsvn-urlrev-url urlrev) rev)))
    (list urlrev file rev)))

(defun fsvn-log-list-cmd-read-reload-with-change-limit ()
  (let (count)
    (setq count
	  (fsvn-read-number "Log limit count: " (fsvn-config-log-limit-count fsvn-buffer-repos-root)))
    (list count)))

(defun fsvn-log-list-cmd-read-reload-with-revision ()
  (let* ((range (fsvn-log-list-current-revision-range))
	 (new-range (fsvn-completing-read-revision-range range)))
    (list new-range)))

(defun fsvn-log-list-cmd-read-merged-import ()
  (let ((url (fsvn-completing-read-url "URL import from: " nil t))
	from to)
    (fsvn-brief-message-showing 
     (fsvn-brief-message-add-message (format "URL: %s" url))
     (setq from (fsvn-completing-read-revision "Revision from: " nil nil url))
     (fsvn-brief-message-add-message (format "Revision from: %s" (fsvn-get-revision-string from)))
     (setq to (fsvn-completing-read-revision "Revision to: " nil nil url))
     (list url (cons from to)))))

;; * fsvn-log-list-mode interactive command

(defun fsvn-log-list-reload-with-change-limit (count)
  (interactive (fsvn-log-list-cmd-read-reload-with-change-limit))
  ;;todo consider rev-range arg
  (fsvn-open-log-view-mode fsvn-logview-target-urlrev fsvn-logview-target-directory-p nil count))

(defun fsvn-log-list-reload-with-revision (&optional range)
  (interactive (fsvn-log-list-cmd-read-reload-with-revision))
  (fsvn-open-log-view-mode fsvn-logview-target-urlrev fsvn-logview-target-directory-p range t))

(defun fsvn-log-list-save-this (urlrev file revision)
  "Save current point REVISION to FILE."
  (interactive (fsvn-log-list-cmd-read-save-this))
  (fsvn-save-file-background urlrev file revision))

(defun fsvn-log-list-next-line (&optional arg)
  "Move down lines then position at log message.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line arg)
  (fsvn-log-list-after-move-line))

(defun fsvn-log-list-previous-line (&optional arg)
  "Move up lines then position at log message.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line (- arg))
  (fsvn-log-list-after-move-line))

(defun fsvn-log-list-next-mark ()
  (interactive)
  (when (re-search-forward fsvn-log-list-re-mark nil t)
    (fsvn-log-list-after-move-line)))

(defun fsvn-log-list-previous-mark ()
  (interactive)
  (let ((p
	 (save-excursion
	   (forward-line 0)
	   (when (re-search-backward fsvn-log-list-re-mark nil t)
	     (fsvn-log-list-after-move-line)
	     (point)))))
    (when p
      (goto-char p))))

(defun fsvn-log-list-mark-put-mark ()
  (interactive)
  (fsvn-log-list-put-mark-this-line fsvn-mark-mark-char)
  (fsvn-log-list-next-line))

(defun fsvn-log-list-mark-unmark ()
  (interactive)
  (fsvn-log-list-put-mark-this-line)
  (fsvn-log-list-next-line))

(defun fsvn-log-list-show-details (rev)
  (interactive (fsvn-log-list-cmd-read-revision))
  (fsvn-log-list-draw-details rev)
  (fsvn-log-list-setup-detail-windows)
  (setq fsvn-log-list-subwindow-settings (current-window-configuration))
  (fsvn-log-list-set-subwindow-config))

(defun fsvn-log-list-scroll-message-up ()
  "When scroll reached to the top of sibling buffer, scroll the message buffer."
  (interactive)
  (fsvn-log-list-scroll-message-buffer t))

(defun fsvn-log-list-scroll-message-down ()
  "When scroll reached to the bottom of log message, scroll the sibling buffer."
  (interactive)
  (fsvn-log-list-scroll-message-buffer nil))

(defun fsvn-log-list-diff-generic (&optional args)
  "Diff current line with anything following case.
Mark is activated diff for region terminated revisions.
Otherwise diff at point revision with working copy file or directory.
"
  (interactive (fsvn-logview-cmd-read-diff-args))
  (cond
   (mark-active
    (fsvn-log-list-diff-with-region args))
   ((fsvn-url-repository-p fsvn-logview-target-urlrev)
    (error "This buffer has non working copy"))
   (t
    (fsvn-log-list-diff-with-wc args))))

(defun fsvn-log-list-ediff-generic ()
  (interactive)
  (when fsvn-logview-target-directory-p
    (error "Cannot execute ediff.  This log list target is directory"))
  (cond
   (mark-active
    (fsvn-log-list-ediff-with-region))
   ((fsvn-url-repository-p fsvn-logview-target-urlrev)
    (error "This buffer has non working copy"))
   (t
    (fsvn-log-list-ediff-with-wc))))

(defun fsvn-log-list-diff-with-wc (&optional args)
  (interactive (fsvn-logview-cmd-read-diff-args))
  (let ((file fsvn-logview-target-urlrev)
	(rev (fsvn-log-list-point-revision))
	buffer diff-args)
    (setq diff-args (list "--revision" rev file))
    (fsvn-diff-start-process diff-args args)))

(defun fsvn-log-list-ediff-with-wc ()
  (interactive)
  (let* ((file fsvn-logview-target-urlrev)
	 (urlrev (fsvn-log-list-point-urlrev))
	 (tmpfile (fsvn-ediff-make-temp-file urlrev)))
    (when (fsvn-save-file urlrev tmpfile t)
      (fsvn-ediff-files tmpfile file))))

(defun fsvn-log-list-previous-log ()
  (interactive)
  (let ((range (fsvn-log-list-current-revision-range))
	new-range)
    (setq new-range
	  (cons (1- (cdr range)) nil))
    (fsvn-open-log-view-mode fsvn-logview-target-urlrev fsvn-logview-target-directory-p new-range)))

(defun fsvn-log-list-next-log ()
  (interactive)
  (let ((range (fsvn-log-list-current-revision-range))
	new-range)
    (setq new-range
	  (cons nil (1+ (car range))))
    (fsvn-open-log-view-mode fsvn-logview-target-urlrev fsvn-logview-target-directory-p new-range)))

(defun fsvn-log-list-toggle-details ()
  (interactive)
  (if (fsvn-log-list-subwindow-display-p)
      (delete-other-windows)
    (fsvn-log-list-show-details (fsvn-log-list-cmd-revision))))

(defun fsvn-log-list-isearch-text (text)
  (interactive
   (let* ((prompt (format "Search Text (%s): " (or (car fsvn-log-list-isarch-history) "")))
	  (readed (read-from-minibuffer prompt
					nil nil nil 'fsvn-log-list-isarch-history)))
     (list (if (string= readed "") (car fsvn-log-list-isarch-history) readed))))
  (let ((saved (point))
	(lst fsvn-log-list-entries)
	rev entry)
    (unless (catch 'exit
	      (fsvn-log-list-next-line)
	      (while (setq rev (fsvn-log-list-point-revision))
		(setq entry (fsvn-log-list-find-entry rev))
		(when (fsvn-xml-text-matched entry text)
		  (throw 'exit t))
		(fsvn-log-list-next-line)))
      (goto-char saved)
      (message "No message was matched."))))

(defun fsvn-log-list-mark-searched (text)
  (interactive (list (read-from-minibuffer
		      "Search Text: " nil nil nil 'fsvn-log-list-isarch-history)))
  (let ((entries (fsvn-log-list-matched-entries text)))
    (save-excursion
      (mapc
       (lambda (entry)
	 (when (fsvn-log-list-goto-revision (fsvn-xml-log->logentry.revision entry))
	   (fsvn-log-list-put-mark-this-line fsvn-mark-mark-char)))
       entries))))

;; FIXME quick hack function
(defun fsvn-log-list-edit-revprop ()
  (interactive)
  (let* ((urlrev (fsvn-log-list-point-urlrev))
	 (revprop (fsvn-read-revprop))
	 (value (read-from-minibuffer "Revision Property: " (fsvn-get-revprop revprop urlrev))))
    (when (y-or-n-p "Really change revision property? ")
      (fsvn-set-revprop-value urlrev revprop value))))

(defun fsvn-log-list-open-revision ()
  (interactive)
  (let* ((rev (fsvn-log-list-point-revision))
	 (url (fsvn-log-list-point-url))
	 dir-urlrev)
    (setq dir-urlrev
	  (if fsvn-logview-target-directory-p
	      (fsvn-url-urlrev url rev)
	    (fsvn-url-urlrev (fsvn-url-dirname url) rev)))
    (fsvn-browse-switch-directory-buffer dir-urlrev)))

(defun fsvn-log-list-quit ()
  (interactive)
  (fsvn-restore-window-buffer
   (kill-buffer (current-buffer))))

(defun fsvn-log-list-copy-urlrev ()
  (interactive)
  (let ((urlrev (fsvn-log-list-point-urlrev)))
    (kill-new urlrev)
    (message urlrev)))

(defun fsvn-log-list-merged-import (src-url revision-range)
  "Merged import SRC-URL to current repository with REVISION-RANGE."
  (interactive (fsvn-log-list-cmd-read-merged-import))
  (let ((dest-url (fsvn-log-list-repository-url)))
    (fsvn-merged-import-with-log src-url revision-range dest-url)))



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
	  (define-key map "S" 'fsvn-log-sibling-save-this)
	  (define-key map "\C-c\C-d" 'fsvn-log-sibling-open-this-directory)
	  (define-key map "\C-c\C-k" 'fsvn-restore-previous-window-setting)
	  (define-key map "\C-c\C-l" 'fsvn-restore-default-window-display)
	  (define-key map "\C-c\C-o" 'fsvn-log-subwindow-switch-to-view)
	  (define-key map "\C-m" 'fsvn-log-sibling-open-this)
	  (define-key map "\C-n" 'fsvn-log-sibling-next-line)
	  (define-key map "\C-p" 'fsvn-log-sibling-previous-line)
	  (define-key map "C" 'fsvn-log-sibling-copy-this)
	  (define-key map "l" 'fsvn-log-sibling-log-this)
	  (define-key map "n" 'fsvn-log-sibling-next-line)
	  (define-key map "p" 'fsvn-log-sibling-previous-line)
	  (define-key map "zl" 'fsvn-log-sibling-log-this)
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
    (fsvn-log-sibling-revision)
    (fsvn-log-sibling-font-lock-keywords)
    ))

(defvar fsvn-log-sibling-target-path nil)
(defvar fsvn-log-sibling-logentry nil)
(defvar fsvn-log-sibling-revision nil)

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
  (setq buffer-undo-list t)
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
  (let* ((url (fsvn-log-sibling-point-url)))
    (when url
      (when (and (fsvn-url-local-p default-directory)
		 (fsvn-directory-versioned-p default-directory))
	(let* ((info (fsvn-get-info-entry default-directory))
	       ;;todo big bug. fsvn-xml-info->entry=>url$ contains space url is encoded.
	       (current (fsvn-xml-info->entry=>url$ info))
	       (filename (fsvn-url-relative-name current url)))
	  (when (file-exists-p filename)
	    (fsvn-expand-file filename)))))))

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
	(rev fsvn-log-sibling-revision))
    (when url
      (fsvn-url-urlrev url rev))))

(defun fsvn-log-sibling-point-prev-urlrev ()
  (cond
   ((memq (fsvn-log-sibling-point-status) '(?M))
    (let* ((urlrev (fsvn-log-sibling-point-urlrev))
	   (prev (1- fsvn-log-sibling-revision))
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

(defun fsvn-log-sibling-target-urlrev ()
  (with-current-buffer fsvn-log-source-buffer
    fsvn-logview-target-urlrev))

(defun fsvn-log-sibling-target-directory-p ()
  (with-current-buffer fsvn-log-source-buffer
    fsvn-logview-target-directory-p))

(defun fsvn-log-sibling-sorted-paths (logentry)
  (sort (copy-sequence (fsvn-xml-log->logentry->paths logentry))
	(lambda (p1 p2)
	  (string-lessp 
	   (fsvn-xml-log->logentry->paths->path$ p1)
	   (fsvn-xml-log->logentry->paths->path$ p2)))))

(defun fsvn-log-sibling-cmd-read-copy-file ()
  (let ((from (fsvn-log-sibling-point-urlrev))
	filename target to args initial)
    (unless from
      (error "No file on this line"))
    (setq filename (fsvn-urlrev-filename from))
    (setq target (fsvn-log-sibling-target-urlrev))
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
    (setq args (fsvn-cmd-read-subcommand-args "copy" fsvn-default-args-copy))
    (list from to args)))

(defun fsvn-log-sibling-cmd-this-local-file ()
  (let ((file (fsvn-log-sibling-get-local-filename)))
    (unless file
      (error "No file on this line"))
    file))

(defun fsvn-log-sibling-cmd-this-url ()
  (let ((url (fsvn-log-sibling-point-url)))
    (unless url
      (error "No file on this line"))
    url))

(defun fsvn-log-sibling-cmd-this-urlrev ()
  (let ((url (fsvn-log-sibling-cmd-this-url)))
    (fsvn-url-urlrev url fsvn-log-sibling-revision)))

(defun fsvn-log-sibling-cmd-read-this-local-file ()
  (list (fsvn-log-sibling-cmd-this-local-file)))

(defun fsvn-log-sibling-cmd-read-this-urlrev ()
  (list (fsvn-log-sibling-cmd-this-urlrev)))

(defun fsvn-log-sibling-cmd-read-save-this ()
  (let* ((url (fsvn-log-sibling-cmd-this-url))
	 (rev fsvn-log-sibling-revision)
	 file)
    (setq file (fsvn-log-read-save-file url rev))
    (list (fsvn-url-urlrev url rev) file)))

;; * fsvn-log-sibling-mode interactive command

(defun fsvn-log-sibling-next-line (&optional arg)
  "Move to next line."
  (interactive "p")
  (forward-line arg))

(defun fsvn-log-sibling-previous-line (&optional arg)
  "Move to previous line."
  (interactive "p")
  (forward-line (- arg)))

(defun fsvn-log-sibling-log-this (urlrev)
  "Open file log by `fsvn-log-list-mode'."
  (interactive (fsvn-log-sibling-cmd-read-this-urlrev))
  (let* ((urlrev (fsvn-log-sibling-point-urlrev))
	 (info (fsvn-get-info-entry urlrev)))
    (fsvn-open-log-view-mode urlrev (eq (fsvn-xml-info->entry.kind info) 'dir))))

(defun fsvn-log-sibling-save-this (urlrev file)
  "Save current point URL to local file."
  (interactive (fsvn-log-sibling-cmd-read-save-this))
  (fsvn-save-file-background urlrev file))

(defun fsvn-log-sibling-diff-previous (&optional args)
  "Diff with previous version."
  (interactive (fsvn-logview-cmd-read-diff-args))
  (fsvn-log-sibling-diffable
   (let* (diff-args)
     (setq diff-args (list (format "--new=%s" URLREV)
			   (format "--old=%s" PREV-URLREV)))
     (fsvn-diff-start-process diff-args args))))

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

(defun fsvn-log-sibling-open-this (filename)
  "Open guessed working copy file."
  (interactive (fsvn-log-sibling-cmd-read-this-local-file))
  (find-file filename))

(defun fsvn-log-sibling-open-this-directory (filename)
  "Open guessed working copy file's directory."
  (interactive (fsvn-log-sibling-cmd-read-this-local-file))
  (fsvn-working-copy (file-name-directory filename)))

(defun fsvn-log-sibling-copy-this (src-urlrev dest-file &optional args)
  "Execute `copy' for point SRC-URLREV to DEST-FILE.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-log-sibling-cmd-read-copy-file))
  (fsvn-popup-start-copy/move-process "copy" (list src-urlrev) dest-file args))



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

	  (define-key map "\C-c\C-m" 'fsvn-log-message-browse-this)
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
  (setq buffer-undo-list nil)
  (fsvn-make-buffer-variables fsvn-log-message-buffer-local-variables)
  (font-lock-mode 0))

(defun fsvn-log-message-get-buffer ()
  (get-buffer-create fsvn-log-message-buffer-name))

(defun fsvn-log-message-cmd-read-commit ()
  (cond
   ((not (buffer-modified-p))
    (error "Message was not changed."))
   ((not (y-or-n-p "Really commit changed log? "))
    (error "quit"))))

(defun fsvn-log-message-cmd-read-quit-edit ()
  ;;TODO consider specific
  (when (buffer-modified-p)
    (error "Log message was changed.")))

(defun fsvn-log-message-cmd-read-browse-this ()
  (let ((link (get-text-property (point) 'fsvn-url-link)))
    (unless link
      (error "No url here."))
    (list link)))

;; * fsvn-log-message-mode interactive command

(defun fsvn-log-message-browse-this (link)
  "Ask a WWW browser to load URL if exists on the point."
  (interactive (fsvn-log-message-cmd-read-browse-this))
  (browse-url link))

(defun fsvn-log-message-start-edit ()
  "Prepare editing commited log message."
  (interactive)
  (setq buffer-read-only nil)
  (force-mode-line-update)
  (message
   (substitute-command-keys 
    (concat "Type \\[fsvn-log-message-commit] to finish edit, \
\\[fsvn-log-message-quit-edit] to quit edit."))))

(defun fsvn-log-message-quit-edit ()
  "Discard changes."
  (interactive (fsvn-log-message-cmd-read-quit-edit))
  (setq buffer-read-only t)
  (force-mode-line-update))

(defun fsvn-log-message-commit ()
  "Commit changed log message."
  (interactive (fsvn-log-message-cmd-read-commit))
  (let ((tmpfile (fsvn-get-prop-temp-file "svn:log" (buffer-substring (point-min) (point-max)))))
    (fsvn-popup-call-process "propset"
			     "--file" tmpfile
			     "--revprop" "svn:log"
			     "--revision" fsvn-log-message-revision
			     fsvn-buffer-repos-root)))



;; fsvn-log-*-mode utility

(defvar fsvn-log-source-buffer nil)

(defun fsvn-log-read-save-file (url rev)
  (let (filename rev-name file)
    (setq filename (fsvn-url-decode-string (fsvn-urlrev-filename url)))
    (setq rev-name (fsvn-file-name-as-revisioned filename rev))
    (setq file (read-file-name "Save as: " nil nil nil rev-name))
    (when (and (file-exists-p file)
	       (not (y-or-n-p "File exists. overwrite? ")))
      (error "quit"))
    (fsvn-expand-file file)))

(defun fsvn-log-subwindow-switch-to-view ()
  (interactive)
  (fsvn-switch-buffer-window fsvn-log-source-buffer t))

(defun fsvn-log-switch-to-sibling ()
  (interactive)
  (fsvn-switch-buffer-window (get-buffer fsvn-log-sibling-buffer-name) t))

(defun fsvn-log-switch-to-message ()
  (interactive)
  (fsvn-switch-buffer-window (get-buffer fsvn-log-message-buffer-name) t))



(defconst fsvn-electric-log-list-buffer-name " *Fsvn Electric Log* ")

(defun fsvn-electric-select-log (urlrev)
  (let* ((buffer (get-buffer-create fsvn-electric-log-list-buffer-name))
	 info root entries)
    (message "Getting info...")
    (setq info (fsvn-get-info-entry urlrev))
    (setq root (fsvn-xml-info->entry=>repository=>root$ info))
    (message "Getting log from repository...")
    (setq entries (fsvn-log-list-cmd urlrev root nil nil))
    (with-current-buffer buffer
      (set (make-local-variable 'font-lock-defaults)
	   '(fsvn-log-list-font-lock-keywords t nil nil beginning-of-line))
      (let (buffer-read-only)
	(fsvn-log-list-mode)
	(erase-buffer)
	(mapc
	 (lambda (entry)
	   (fsvn-log-list-insert-entry entry))
	 entries)
	(setq fsvn-logview-target-urlrev urlrev)
	(setq fsvn-buffer-repos-root root)
	(setq fsvn-log-list-all-entries entries)
	(setq fsvn-log-list-entries entries)
	(setq fsvn-log-list-target-path 
	      (fsvn-repository-path root (fsvn-xml-info->entry=>url$ info)))
	(fsvn-electric-line-select-mode 1)
	(setq fsvn-electric-next-data-function 'fsvn-electric-select-log-next-data)
	(setq fsvn-electric-done-function 'fsvn-electric-select-log-done)
	(font-lock-mode 1)
	(font-lock-fontify-buffer)))
    (fsvn-electric-line-select buffer)))

(defun fsvn-electric-select-log-done ()
  (fsvn-log-list-point-urlrev))

(defun fsvn-electric-select-log-next-data ()
  (save-excursion
    (let* ((range (fsvn-log-list-current-revision-range))
	   (new-range (cons (1- (cdr range)) 0))
	   (prev-entries fsvn-log-list-all-entries)
	   buffer-read-only entries)
      (setq entries (fsvn-log-list-cmd fsvn-logview-target-urlrev fsvn-buffer-repos-root new-range nil))
      (setq fsvn-log-list-all-entries (fsvn-logs-unique-merge entries prev-entries))
      (setq fsvn-log-list-entries fsvn-log-list-all-entries)
      (goto-char (point-max))
      (mapc
       (lambda (entry)
	 (fsvn-log-list-insert-entry entry))
       entries)
      (font-lock-fontify-buffer))))



(put 'fsvn-log-list-each-rev 'lisp-indent-function 1)

(provide 'fsvn-logview)

;;; fsvn-log.el ends here
