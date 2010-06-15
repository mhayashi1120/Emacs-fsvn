;;; fsvn-blame.el --- svn subcommand `blame' utility


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



;; TODO after-change-functions

(require 'fsvn-mode)



(defvar minor-mode-alist)



;; todo highlight specific revisions.
;;    fsvn-blame-minor-highlight-specific-revisions
;;    fsvn-blame-minor-highlight-log-regexp

(defvar fsvn-blame-subwindow-mode-map nil)

(unless fsvn-blame-subwindow-mode-map
  (setq fsvn-blame-subwindow-mode-map
	(let ((map (make-sparse-keymap)))
	  (suppress-keymap map)

	  map)))

(defcustom fsvn-blame-subwindow-mode-hook nil
  "*Run at the very end of `fsvn-blame-subwindow-mode'."
  :group 'fsvn
  :type 'hook)

(defconst fsvn-blame-subwindow-buffer-name " *Fsvn Blame Control*")

(defcustom fsvn-blame-subwindow-height 10
  "*Blame minor mode control window height."
  :group 'fsvn
  :type 'integer)

(defconst fsvn-blame-subwindow-buffer-local-variables
  `(
    (font-lock-defaults . '(fsvn-blame-subwindow-font-lock-keywords t nil nil beginning-of-line))
    (font-lock-verbose)
    ))

(defvar fsvn-blame-subwindow-font-lock-keywords nil)

(setq fsvn-blame-subwindow-font-lock-keywords
      (list
       (list "^\\(\\(?:Revision\\|Date\\|Author\\):\\) \\(.*\\)" '(1 fsvn-header-key-face) '(2 fsvn-header-face))
       ))

(defun fsvn-blame-subwindow-mode ()
  "Major mode for viewing Subversion log message that is related `fsvn-blame-minor-mode'.

Entry to this mode calls the value of `fsvn-blame-subwindow-mode-hook'.

Keybindings:
\\{fsvn-blame-subwindow-mode-map}

"
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-blame-subwindow-mode-map)
  (setq major-mode 'fsvn-blame-subwindow-mode)
  (setq mode-name "Fsvn Blame Control")
  (fsvn-make-buffer-variables fsvn-blame-subwindow-buffer-local-variables)
  ;;FIXME too late fontify.
  (font-lock-fontify-buffer))

(defun fsvn-blame-subwindow-insert-message (line string)
  (with-current-buffer (fsvn-blame-get-subwindow-buffer)
    (let (buffer-read-only remain)
      (if line
	  (progn
	    (goto-char (point-min))
	    (setq remain (forward-line line))
	    (insert (make-string remain ?\n))
	    (delete-region (point) (save-excursion (forward-line 1) (point))))
	(erase-buffer))
      (insert string ?\n))
    (set-buffer-modified-p nil)))

(defconst fsvn-blame-minor-buffer-local-variables
  '(
    (fsvn-blame-minor-mode . t)
    (fsvn-blame-blame-logs)
    (fsvn-blame-previous-revision)
    (fsvn-blame-blame-data)
    (fsvn-blame-log-data)
    (kill-buffer-hook . kill-buffer-hook)
    (fsvn-blame-spent-time . (cons (float-time) nil))
    ))

(defvar fsvn-blame-minor-mode nil)
(defvar fsvn-blame-blame-logs nil)
(defvar fsvn-blame-blame-data nil)
(defvar fsvn-blame-log-data nil)
(defvar fsvn-blame-previous-revision nil)
(defvar fsvn-blame-timer nil)
(defvar fsvn-blame-spent-time nil)
(defvar fsvn-blame-subwindow-buffer nil)

;;todo slow!!
(defun fsvn-blame-minor-mode ()
  "Minor mode for visualized Subversion annotate/blame/praise output.

Keybindings: none

"
  (interactive)
  (unless (buffer-file-name)
    (error "This buffer has no associated file"))
  (let ((cell (assq 'fsvn-blame-minor-mode minor-mode-alist)))
    (unless cell
      (setq cell (list 'fsvn-blame-minor-mode nil))
      (setq minor-mode-alist
	    (cons cell minor-mode-alist)))
    (setcdr cell (list " [Fsvn Blame]"))
    (if fsvn-blame-minor-mode
	(fsvn-blame-minor-mode-quit)
      (fsvn-blame-minor-mode-start))))

(defun fsvn-blame-minor-mode-start ()
  (fsvn-blame-file-logs)
  (fsvn-make-buffer-variables-internal fsvn-blame-minor-buffer-local-variables)
  (fsvn-blame-activate-timer)
  (add-hook 'kill-buffer-hook 'fsvn-blame-kill-buffer))

(defun fsvn-blame-minor-mode-quit ()
  (interactive)
  (fsvn-kill-buffer-variables-internal fsvn-blame-minor-buffer-local-variables)
  (fsvn-blame-deactivate-timer)
  (fsvn-blame-clear-all-overlay)
  (fsvn-blame-tidy-up-subwindow)
  (fsvn-blame-cleanup-process))

(defun fsvn-blame-activate-timer ()
  (unless fsvn-blame-timer
    (setq fsvn-blame-timer (run-at-time t 0.5 'fsvn-blame-highlight-in-timer))))

(defun fsvn-blame-deactivate-timer ()
  (when (and fsvn-blame-timer
	     (not (catch 'found
		    (mapc
		     (lambda (b)
		       (with-current-buffer b
			 (when fsvn-blame-minor-mode
			   (throw 'found b))))
		     (buffer-list))
		    nil)))
    (cancel-timer fsvn-blame-timer)
    (setq fsvn-blame-timer nil)))

(defun fsvn-blame-kill-buffer ()
  (when fsvn-blame-minor-mode
    (fsvn-blame-minor-mode-quit)))

(defun fsvn-blame-highlight-in-timer ()
  (if (not fsvn-blame-minor-mode)
      (fsvn-blame-tidy-up-subwindow)
    (let* ((overlay
	    (catch 'found
	      (mapc (lambda (o)
		      (when (overlay-get o 'fsvn-blame-revision)
			(throw 'found o)))
		    (overlays-at (point)))))
	   (data fsvn-blame-blame-logs)
	   (prev-rev fsvn-blame-previous-revision)
	   (start (car fsvn-blame-spent-time))
	   (control-buffer (fsvn-blame-get-subwindow-buffer))
	   (target-buffer (current-buffer))
	   rev)
      (with-current-buffer control-buffer
	(condition-case err
	    (progn
	      (let (buffer-read-only)
		(cond
		 ((null data)
		  (if (null (fsvn-blame-get-processes target-buffer))
		      (fsvn-blame-subwindow-insert-message nil "Process exited.")
		    (fsvn-blame-subwindow-insert-message 0 (format "Spent %d seconds." (- (float-time) start)))))
		 ((or (null overlay)
		      (null (setq rev (overlay-get overlay 'fsvn-blame-revision))))
		  (erase-buffer)
		  (insert "No revision here.\n"))
		 ((eq prev-rev rev)) ;; do nothing
		 (t
		  (erase-buffer)
		  (let ((entry (fsvn-logs-find-logentry data rev))
			msg date)
		    (setq msg (fsvn-xml-log->logentry=>msg$ entry))
		    (setq date (format-time-string fsvn-generic-datetime-format (fsvn-xml-log->logentry=>date$ entry)))
		    (insert (format "Revision: %d\n" rev))
		    (insert (format "Author: %s\n" (fsvn-xml-log->logentry=>author$ entry)))
		    (insert (format "Date: %s\n" date))
		    (insert "\n")
		    (when msg
		      (insert msg))))))
	      (set-buffer-modified-p nil)
	      (setq buffer-read-only t)
	      (fsvn-blame-minor-setup-subwindow control-buffer))
	  ;;FIXME if error occur
	  (error (insert (format "%s" err)))))
      (if rev
	  (fsvn-blame-highlight-revision rev)
	(fsvn-blame-dehighlight-all))
      (setq fsvn-blame-previous-revision rev))))

(defun fsvn-blame-highlight-revision (rev)
  (fsvn-blame-dehighlight-all)
  (mapc
   (lambda (o)
     (when (eq (overlay-get o 'fsvn-blame-revision) rev)
       (overlay-put o 'face 'highlight)))
   (overlays-in (point-min) (point-max))))

(defun fsvn-blame-dehighlight-all ()
  (mapc
   (lambda (o)
     (when (overlay-get o 'fsvn-blame-revision)
       (overlay-put o 'face nil)))
   (overlays-in (point-min) (point-max))))

(defun fsvn-blame-make-buffer-overlay (blame-logs diff-alist)
  (let ((colors (defined-colors))
	(line 0)
	foreground background
	face-alist)
    (fsvn-blame-clear-all-overlay)
    (mapc
     (lambda (entry)
       (let ((rev (fsvn-xml-log->logentry.revision entry)))
	 (unless (assq rev face-alist)
	   (setq foreground (nth (% rev (length colors)) colors))
	   (setq background (fsvn-get-background-color foreground))
	   (setq face-alist
		 (cons
		  (cons rev (list
			     (cons 'foreground-color foreground)
			     (cons 'background-color background)))
		  face-alist)))))
     blame-logs)
    (fsvn-blame-group-by-revision blame-logs face-alist diff-alist)))

(defun fsvn-blame-group-by-revision (blame-logs face-alist diff-alist)
  (let ((wc-line 1)
	(blame-line 1)
	entry prev-end
	curr-rev prev-rev flg loop)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(setq prev-end (point-min))
	(while (not (eobp))
	  (cond
	   ;; line added in wc
	   ((assq wc-line diff-alist)
	    (when flg
	      (fsvn-blame-create-overlay-internal prev-end (save-excursion (forward-line -1) (point)) prev-rev face-alist)
	      (setq flg nil))
	    (setq prev-end (point)))
	   (t
	    (setq flg t)
	    ;; line removed in wc
	    (setq loop (if (rassq blame-line diff-alist) 2 1))
	    (fsvn-loop loop
	      (setq entry (car blame-logs))
	      (setq blame-logs (cdr blame-logs))
	      (setq blame-line (1+ blame-line))
	      (setq curr-rev (fsvn-xml-log->logentry.revision entry)))
	    (when (or (null blame-logs) (and prev-rev (/= curr-rev prev-rev)))
	      (fsvn-blame-create-overlay-internal prev-end (point) prev-rev face-alist)
	      (setq prev-end (point)))))
	  (forward-line 1)
	  (setq wc-line (1+ wc-line))
	  (setq prev-rev curr-rev))
	(fsvn-blame-create-overlay-internal prev-end (point-max) prev-rev face-alist)))))

(defun fsvn-blame-minor-setup-subwindow (subwindow-buffer)
  (let (newwin)
    (when (and (> (window-height) (* fsvn-blame-subwindow-height 2))
	       (not (memq subwindow-buffer (mapcar 'window-buffer (window-list)))))
      (setq newwin (split-window nil (- (window-height) fsvn-blame-subwindow-height)))
      (set-window-buffer newwin subwindow-buffer))))

(defun fsvn-blame-tidy-up-subwindow ()
  (let (cwindow)
    (catch 'unable
      (mapc
       (lambda (win)
	 (let ((buffer (window-buffer win)))
	   (with-current-buffer buffer
	     (when fsvn-blame-minor-mode
	       (throw 'unable t))
	     (when (eq major-mode 'fsvn-blame-subwindow-mode)
	       (let (buffer-read-only)
		 (erase-buffer))
	       (setq cwindow win)))))
       (window-list))
      (when cwindow
	(delete-window cwindow)))))

(defun fsvn-blame-create-overlay-internal (start end rev face-alist)
  (let (overlay-face overlay)
    (setq overlay-face (cdr (assq rev face-alist)))
    (setq overlay (make-overlay start end nil t t))
;;     (overlay-put overlay 'face overlay-face)
    (overlay-put overlay 'fsvn-blame-name 'fsvn-blame-face-overlay)
    (overlay-put overlay 'fsvn-blame-revision rev)))

(defun fsvn-blame-clear-all-overlay ()
  (save-restriction
    (widen)
    (mapc
     (lambda (o)
       (when (eq (overlay-get o 'fsvn-blame-name) 'fsvn-blame-face-overlay)
	 (delete-overlay o)))
     (overlays-in (point-min) (point-max)))))

(defun fsvn-blame-cleanup-process ()
  (mapc
   (lambda (p)
     (delete-process p))
   (fsvn-blame-get-processes)))

(defun fsvn-blame-file-logs (&optional rev-range)
  "Execute `log' and `blame' asynchronous process."
  (let ((log (fsvn-make-temp-buffer))
	(blame (fsvn-make-temp-buffer))
	(urlrev (fsvn-blame-buffer-urlrev))
	(buffer (current-buffer))
	range-arg
	log-proc blame-proc)
    (with-current-buffer (fsvn-blame-get-subwindow-buffer)
      (let (buffer-read-only)
	(erase-buffer)))
    (setq range-arg
	  (when rev-range
	    (list "--revision" (fsvn-revision-range-to-string rev-range))))
    (setq log-proc (fsvn-start-command "log" log "--xml" "--verbose" range-arg urlrev))
    (setq blame-proc (fsvn-start-command "blame" blame "--xml" range-arg urlrev))
    (mapc
     (lambda (list)
       (let ((p (nth 0 list))
	     (name (nth 1 list))
	     (line (nth 2 list)))
	 (set-process-filter p 'fsvn-blame-process-filter)
	 (process-put p 'fsvn-blame-file-buffer buffer)
	 (process-put p 'fsvn-blame-process-name name)
	 (process-put p 'fsvn-blame-process-line line)
	 (fsvn-blame-subwindow-insert-message line (format "%s Received %d" name 0))))
     (list (list blame-proc "Blame" 1) (list log-proc "Log" 2)))
    (set-process-sentinel log-proc
			  (fsvn-blame-create-process-sentinel
			   (fsvn-xml-parse-logentry)
			   fsvn-blame-log-data))
    (set-process-sentinel blame-proc
			  (fsvn-blame-create-process-sentinel
			   (car (fsvn-xml-parse-blame))
			   fsvn-blame-blame-data))))

(defmacro fsvn-blame-create-process-sentinel (parser-form var-symbol)
  `(lambda (proc event)
     (fsvn-process-exit-handler proc event
       (if (= (process-exit-status proc) 0)
	   (let ((data ,parser-form)
		 (file-buffer (process-get proc 'fsvn-blame-file-buffer)))
	     (when (buffer-live-p file-buffer)
	       (with-current-buffer file-buffer
		 (setq ,var-symbol data)
		 (fsvn-blame-merge-and-activate)))
	     (kill-buffer (current-buffer)))
	 (let ((line (process-get proc 'fsvn-blame-process-line))
	       (message (format "Process exited status %d" (process-exit-status proc))))
	   (fsvn-blame-subwindow-insert-message line message))))))

(defun fsvn-blame-process-filter (proc event)
  (fsvn-process-event-handler proc event
    (goto-char (point-max))
    (insert event))
  (when (and fsvn-blame-minor-mode
	     (eq (process-get proc 'fsvn-blame-file-buffer) (current-buffer)))
    (let ((size (buffer-size (process-buffer proc)))
	  (name (process-get proc 'fsvn-blame-process-name))
	  (line (process-get proc 'fsvn-blame-process-line)))
      (fsvn-blame-subwindow-insert-message line (format "%s Received %d." name size)))))

(defun fsvn-blame-merge-and-activate ()
  (cond
   ((and fsvn-blame-blame-data
	 fsvn-blame-log-data)
    (let ((logs fsvn-blame-log-data)
	  (blame fsvn-blame-blame-data)
	  (urlrev (fsvn-blame-buffer-urlrev))
	  blame-data diff)
      (setq blame-data
	    (mapcar
	     (lambda (entry)
	       (let ((rev (fsvn-xml-blame->target->entry=>commit.revision entry)))
		 (if rev
		     (fsvn-logs-find-logentry logs rev)
		   nil)))
	     (fsvn-xml-blame->target->entries blame)))
      (when (fsvn-url-local-p urlrev)
	(setq diff (fsvn-diff-file-alist urlrev)))
      (fsvn-blame-make-buffer-overlay blame-data diff)
      (setq fsvn-blame-blame-logs blame-data)
      (setcdr fsvn-blame-spent-time (float-time))))))

(defun fsvn-blame-get-subwindow-buffer ()
  (let ((exists fsvn-blame-subwindow-buffer))
    (when (and exists (buffer-live-p exists))
      (with-current-buffer exists
	(fsvn-blame-subwindow-mode)))
    (setq fsvn-blame-subwindow-buffer
	  (or exists (generate-new-buffer fsvn-blame-subwindow-buffer-name)))))

(defun fsvn-blame-get-processes (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (fsvn-mapitem
     (lambda (p)
       (when (eq (process-get p 'fsvn-blame-file-buffer) buffer)
	 p))
     (process-list))))

(defun fsvn-blame-buffer-urlrev ()
  (cond
   ((fsvn-url-local-p (buffer-file-name))
    (buffer-file-name))
   ((fsvn-magic-file-name-absolute-p (buffer-file-name))
    (fsvn-magic-parse-file-name (buffer-file-name)))))



(provide 'fsvn-blame)

;;; fsvn-blame.el ends here
