;;; fsvn-msgedit.el --- Subversion message edit mode for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

(require 'fsvn-mode)



(defvar text-mode-map)
(defvar last-command)
(defvar dired-re-no-dot)



(fsvn-defstruct log-edit-message
  file region)

(defconst fsvn-message-edit-buffer-name "Fsvn Log Message")
(defconst fsvn-message-edit-buffer-local-variables
  '(
    (fsvn-message-edit-last-message)
    (fsvn-buffer-repos-root)
    (fsvn-message-edit-target-files)
    (fsvn-message-edit-subcommand-args)
    (fsvn-message-edit-no-unlock)
    (fsvn-message-edit-keep-changelist)
    ))

(defvar fsvn-message-edit-done nil)
(defvar fsvn-message-edit-target-files nil)
(defvar fsvn-message-edit-subcommand-args nil)
(defvar fsvn-message-edit-no-unlock nil)
(defvar fsvn-message-edit-keep-changelist nil)

(defvar fsvn-message-edit-last-message nil)
(defvar fsvn-message-edit-mode-map nil)
(setq fsvn-message-edit-mode-map
      (let ((map (make-sparse-keymap)))
	(set-keymap-parent map text-mode-map)

	(define-key map "\C-c\C-c" 'fsvn-message-edit-done)
	(define-key map "\C-c\C-k" 'fsvn-message-edit-quit)
	(define-key map "\C-c\C-l" 'fsvn-restore-default-window-setting)
	(define-key map "\C-c\C-o" 'fsvn-message-edit-switch-window)
	(define-key map "\C-c\C-q" 'fsvn-message-edit-quit)
	(define-key map "\en" 'fsvn-message-edit-next-message)
	(define-key map "\ep" 'fsvn-message-edit-previous-message)
	(define-key map "\C-cN" 'fsvn-message-edit-toggle-no-unlock)
	(define-key map "\C-cK" 'fsvn-message-edit-toggle-keep-changelist)
	(define-key map "\er" 'fsvn-message-edit-re-search-message-forward)
	(define-key map "\e\C-r" 'fsvn-message-edit-re-search-message-backward)

	map))

(defcustom fsvn-message-edit-mode-hook nil
  "*Run at the very end of `fsvn-message-edit-mode'."
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-message-edit-before-commit-hook nil
  "*"
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-message-edit-after-commit-hook nil
  "*"
  :group 'fsvn
  :type 'hook)

;; * fsvn-message-edit-mode internal function

(defun fsvn-message-edit-mode ()
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-message-edit-mode-map)
  (setq major-mode 'fsvn-message-edit-mode)
  (setq mode-name '("Fsvn Log Edit" (:eval fsvn-message-edit-no-unlock) (:eval fsvn-message-edit-keep-changelist)))
  (fsvn-make-buffer-variables fsvn-message-edit-buffer-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer))

(defun fsvn-message-edit-prepared-buffer ()
  (fsvn-sole-major-mode 'fsvn-message-edit-mode))

(defun fsvn-message-edit-get-buffer ()
  (get-buffer-create fsvn-message-edit-buffer-name))

(defun fsvn-message-edit-substitute-subcommand-arg (arg value)
  (setq fsvn-message-edit-subcommand-args
	(cond
	 ((and (null value) (member arg fsvn-message-edit-subcommand-args))
	  (fsvn-delete arg fsvn-message-edit-subcommand-args))
	 ((and value (not (member arg fsvn-message-edit-subcommand-args)))
	  (cons arg fsvn-message-edit-subcommand-args))
	 (t
	  fsvn-message-edit-subcommand-args))))

(defun fsvn-message-edit-insert-log-file (file)
  (let ((start (point-marker))
	end)
    (save-excursion
      (let ((coding-system-for-read fsvn-log-message-file-encoding))
	(forward-char (cadr (insert-file-contents file)))
	(setq end (point-marker))))
    (setq fsvn-message-edit-last-message
	  (fsvn-struct-log-edit-message-make :file file :region (cons start end)))))

(defun fsvn-message-edit-delete-if-repeated ()
  (when (and (fsvn-message-edit-repeated-command-p)
	     fsvn-message-edit-last-message)
    (let ((region (fsvn-struct-log-edit-message-get-region fsvn-message-edit-last-message)))
      (delete-region (car region) (cdr region)))))

(defun fsvn-message-edit-repeated-command-p ()
  (memq last-command 
	'(fsvn-message-edit-previous-message 
	  fsvn-message-edit-next-message
	  fsvn-message-edit-re-search-message-backward
	  fsvn-message-edit-re-search-message-forward)))

(defun fsvn-message-edit-find-file (reverse)
  (let* ((message fsvn-message-edit-last-message)
	 (find-list (fsvn-message-edit-message-files))
	 (len (length find-list))
	 (i 0)
	 file)
    (if (null message)
	(car (last find-list))
      (when reverse
	(setq find-list (nreverse find-list)))
      (setq file (fsvn-struct-log-edit-message-get-file message))
      (catch 'found
	(mapc
	 (lambda (f)
	   (when (fsvn-file= file f)
	     (throw 'found (nth (mod (1- i) len) find-list)))
	   (setq i (1+ i)))
	 find-list)
	nil))))

(defun fsvn-message-edit-search-file (regexp reverse)
  (let* ((message fsvn-message-edit-last-message)
	 (find-list (fsvn-message-edit-message-files)))
    (if (null message)
	(car (last find-list))
      (when reverse
	(setq find-list (nreverse find-list)))
      (catch 'found
	(let ((coding-system-for-read fsvn-log-message-file-encoding))
	  (mapc
	   (lambda (f)
	     (with-temp-buffer
	       (insert-file-contents f)
	       (goto-char (point-min))
	       (when (re-search-forward regexp nil t)
		 (throw 'found f))))
	   find-list)
	  nil)))))

(defun fsvn-message-edit-message-files ()
  (let* ((dir (fsvn-message-edit-get-message-directory))
	 (ret (directory-files-and-attributes dir t dired-re-no-dot)))
    (setq ret
	  (sort ret (lambda (x y)
		      (time-less-p (nth 5 (cdr x)) (nth 5 (cdr y))))))
    (mapcar 'car ret)))

(defun fsvn-message-edit-commit ()
  (run-hooks 'fsvn-message-edit-before-commit-hook)
  (let ((files (fsvn-message-edit-gather-marked-files))
	(buffer (fsvn-popup-result-create-buffer))
	msg)
    (if (= (length files) 0)
	(message "No file to be commited.")
      (fsvn-message-edit-commit-check files)
      (fsvn-restore-window-buffer
       (fsvn-message-edit-commit-internal files buffer))
      (fsvn-buffer-popup-as-information buffer))))

(defun fsvn-message-edit-commit-show-message ()
  (let (done-command quit-command cycle-command)
    (cond
     ((eq major-mode 'fsvn-select-file-mode)
      (setq done-command 'fsvn-select-file-done)
      (setq quit-command 'fsvn-select-file-quit)
      (setq cycle-command 'fsvn-select-file-switch-window))
     (t
      (setq done-command 'fsvn-message-edit-done)
      (setq quit-command 'fsvn-message-edit-quit)
      (setq cycle-command 'fsvn-message-edit-switch-window)))
    (message
     (substitute-command-keys (concat "Type \\[" (symbol-name done-command) "] to finish edit, \
\\[" (symbol-name quit-command) "] to quit edit, \
\\[" (symbol-name cycle-command) "] to cycle window.")))))

(defun fsvn-message-edit-commit-internal (arg-files arg-buffer)
  (fsvn-async-let ((targets (fsvn-make-targets-file arg-files))
		   (message (fsvn-message-edit-create-message-file))
		   (no-unlock fsvn-message-edit-no-unlock)
		   (args fsvn-message-edit-subcommand-args)
		   (output-size (buffer-size arg-buffer))
		   (files arg-files)
		   (buffer arg-buffer)
		   proc
		   locked locked-targets
		   unversioned unversiond-targets)
    (setq unversioned (fsvn-message-edit-choice-unversioned files))
    (when (> (length unversioned) 0)
      (setq unversiond-targets (fsvn-make-targets-file unversioned))
      (prog1
	  (setq proc (fsvn-start-command-display "add" buffer "--targets" unversiond-targets "--non-recursive"))
	(set-process-sentinel proc (lambda (proc event)
				     (fsvn-process-exit-handler proc event
				       (fsvn-parse-result-cmd-add buffer output-size)
				       (goto-char (point-max))
				       (insert "\n")
				       (setq output-size (buffer-size (current-buffer))))))))
    (unless no-unlock
      (when (setq locked (fsvn-message-edit-choice-just-locked files))
	(setq locked-targets (fsvn-make-targets-file locked))
	(prog1
	    (setq proc (fsvn-start-command-display "unlock" buffer "--targets" locked-targets))
	  (set-process-sentinel proc (lambda (proc event)
				       (fsvn-process-exit-handler proc event
					 (fsvn-parse-result-cmd-unlock buffer output-size)
					 (goto-char (point-max))
					 (insert "\n")
					 (setq output-size (buffer-size (current-buffer)))))))))
    (prog1
	(setq proc (fsvn-start-command-display "commit" buffer
				       "--targets" targets
				       (if message
					   (list "--file" message)
					 (list "--message" ""))
				       "--encoding" (fsvn-coding-system-name fsvn-log-message-file-encoding)
				       args))
      (set-process-sentinel proc 'fsvn-message-edit-commit-sentinel)
      (set-process-filter proc 'fsvn-process-filter-popup-buffer))
    (process-put proc 'fsvn-process-start-point output-size)))

(defun fsvn-message-edit-commit-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (= (process-exit-status proc) 0)
      (let ((output-size (process-get proc 'fsvn-process-start-point)))
	(fsvn-parse-result-cmd-commit (current-buffer) output-size)
	(let ((lst (list (fsvn-message-edit-get-buffer) (fsvn-select-file-get-buffer))))
	  (mapc
	   (lambda (x)
	     (when (and x (bufferp x) (buffer-live-p x))
	       (kill-buffer x)))
	   lst)))
      (run-hooks 'fsvn-message-edit-after-commit-hook)
      (fsvn-run-recursive-status (fsvn-find-most-top-buffer-directory default-directory)))))

(defun fsvn-message-edit-delete ()
  (run-hooks 'fsvn-message-edit-before-commit-hook)
  (let ((buffer (fsvn-popup-result-create-buffer))
	(targets (fsvn-make-targets-file fsvn-message-edit-target-files))
	(message (fsvn-message-edit-create-message-file))
	proc)
    (fsvn-restore-window-buffer
     (fsvn-message-edit-delete-internal targets buffer message))
    (fsvn-buffer-popup-as-information buffer)))

(defun fsvn-message-edit-delete-internal (targets buffer message)
  (let (proc)
    (setq proc (fsvn-start-command "delete" buffer
				   "--targets" targets
				   (if message
				       (list "--file" message)
				     (list "--message" ""))
				   "--encoding" (fsvn-coding-system-name fsvn-log-message-file-encoding)
				   fsvn-message-edit-subcommand-args))
    (set-process-sentinel proc 'fsvn-message-edit-delete-sentinel)
    (set-process-filter proc 'fsvn-process-filter-popup-buffer)))

(defun fsvn-message-edit-delete-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (= (process-exit-status proc) 0)
      (let ((x (fsvn-message-edit-prepared-buffer)))
	(fsvn-parse-result-cmd-delete (current-buffer))
	(when (and x (bufferp x) (buffer-live-p x))
	  (kill-buffer x))))))

(defun fsvn-message-edit-import ()
  (run-hooks 'fsvn-message-edit-before-commit-hook)
  (let ((buffer (fsvn-popup-result-create-buffer)))
    (fsvn-restore-window-buffer
     (fsvn-message-edit-import-internal buffer))
    (fsvn-buffer-popup-as-information buffer)))

(defun fsvn-message-edit-import-internal (buffer)
  (let ((message (fsvn-message-edit-create-message-file))
	proc)
    (setq proc (fsvn-start-command "import" buffer
				   (if message
				       (list "--file" message)
				     (list "--message" ""))
				   "--encoding" (fsvn-coding-system-name fsvn-log-message-file-encoding)
				   fsvn-message-edit-subcommand-args))
    (set-process-sentinel proc 'fsvn-message-edit-import-sentinel)
    (set-process-filter proc 'fsvn-process-filter-popup-buffer)
    proc))

(defun fsvn-message-edit-import-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (= (process-exit-status proc) 0)
      (kill-buffer (fsvn-message-edit-get-buffer))
      (run-hooks 'fsvn-message-edit-after-commit-hook))))

(defun fsvn-message-edit-mkdir ()
  (run-hooks 'fsvn-message-edit-before-commit-hook)
  (let ((buffer (fsvn-popup-result-create-buffer))
	(message (fsvn-message-edit-create-message-file))
	proc)
    (fsvn-restore-window-buffer
     (fsvn-message-edit-mkdir-internal buffer message))
    (fsvn-buffer-popup-as-information buffer)))

(defun fsvn-message-edit-mkdir-internal (buffer message)
  (let (proc)
    (setq proc (fsvn-start-command "mkdir" buffer
				   (if message
				       (list "--file" message)
				     (list "--message" ""))
				   "--encoding" (fsvn-coding-system-name fsvn-log-message-file-encoding)
				   fsvn-message-edit-subcommand-args))
    (set-process-sentinel proc 'fsvn-message-edit-mkdir-sentinel)
    (set-process-filter proc 'fsvn-process-filter-popup-buffer)
    proc))

(defun fsvn-message-edit-mkdir-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (when (= (process-exit-status proc) 0)
      (kill-buffer (fsvn-message-edit-get-buffer))
      (run-hooks 'fsvn-message-edit-after-commit-hook))))

(defun fsvn-message-edit-lock ()
  (fsvn-parse-result-cmd-lock
   (fsvn-call-process-multi-with-popup "lock" fsvn-message-edit-subcommand-args)))

(defun fsvn-message-edit-gather-marked-files ()
  (fsvn-message-edit-in-file-select-buffer 'fsvn-select-file-gather-marked-files))

(defun fsvn-message-edit-choice-unversioned (files)
  (fsvn-message-edit-in-file-select-buffer 'fsvn-select-file-choice-unversioned files))

(defun fsvn-message-edit-choice-just-locked (files)
  (fsvn-message-edit-in-file-select-buffer 'fsvn-select-file-choice-just-locked files))

(defun fsvn-message-edit-in-file-select-buffer (function &rest args)
  (unless (fsvn-select-file-prepared-buffer)
    (error "Collateral file select buffer deleted"))
  (with-current-buffer (fsvn-select-file-get-buffer)
    (apply function args)))

(defun fsvn-message-edit-get-message ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun fsvn-message-edit-get-message-directory ()
  (let ((dir (fsvn-expand-file (md5 fsvn-buffer-repos-root) (fsvn-logmessage-directory))))
    (unless (fsvn-file-exact-directory-p dir)
      (make-directory dir t))
    dir))

(defun fsvn-message-edit-create-message-file ()
  (unless (eq major-mode 'fsvn-message-edit-mode)
    (error "Can't execute this function in this mode"))
  (let (tmpfile)
    (if (= (buffer-size) 0)
	(when (fsvn-config-log-empty-warnings fsvn-buffer-repos-root)
	  (unless (y-or-n-p "Log message is empty.  Really commit? ")
	    (error "No log messages")))
      (setq tmpfile (fsvn-message-edit-make-message-file))
      (let ((coding-system-for-write fsvn-log-message-file-encoding))
	(write-region (point-min) (point-max) tmpfile nil 'no-msg))
      tmpfile)))

(defun fsvn-message-edit-commit-check (files)
  (let ((dir default-directory))
    (when (fsvn-config-tortoise-property-use fsvn-buffer-repos-root)
      (fsvn-tortoise-commit-check files dir))
    ))

(defun fsvn-message-edit-make-message-file ()
  (let* ((dir (fsvn-message-edit-get-message-directory))
	 (temporary-file-directory dir))
    (make-temp-file (format-time-string "%s"))))

;; * fsvn-message-edit-mode interactive command

(defun fsvn-message-edit-done ()
  (interactive)
  (funcall fsvn-message-edit-done))

(defun fsvn-message-edit-previous-message ()
  (interactive)
  (let (file)
    (fsvn-message-edit-delete-if-repeated)
    (when (setq file (fsvn-message-edit-find-file nil))
      (fsvn-message-edit-insert-log-file file))))

(defun fsvn-message-edit-next-message ()
  (interactive)
  (let (file)
    (fsvn-message-edit-delete-if-repeated)
    (when (setq file (fsvn-message-edit-find-file t))
      (fsvn-message-edit-insert-log-file file))))

(defun fsvn-message-edit-re-search-message-forward (regexp)
  (interactive (list (read-from-minibuffer "Regexp: ")))
  (let (file)
    (unless (setq file (fsvn-message-edit-search-file regexp nil))
      (error "No matched file."))
    (fsvn-message-edit-delete-if-repeated)
    (fsvn-message-edit-insert-log-file file)))

(defun fsvn-message-edit-re-search-message-backward (regexp)
  (interactive (list (read-from-minibuffer "Regexp: ")))
  (let (file)
    (unless (setq file (fsvn-message-edit-search-file regexp t))
      (error "No matched file."))
    (fsvn-message-edit-delete-if-repeated)
    (fsvn-message-edit-insert-log-file file)))

(defun fsvn-message-edit-switch-window ()
  (interactive)
  (fsvn-restore-default-window-setting)
  (fsvn-switch-buffer-window (fsvn-select-file-prepared-buffer)))

(defun fsvn-message-edit-quit ()
  (interactive)
  ;;todo confirm here and file-select done
  ;;   when `fsvn-browse-add-file-select'
  (fsvn-restore-window-buffer
   (kill-buffer (fsvn-select-file-get-buffer))
   (kill-buffer (current-buffer))))

(defun fsvn-message-edit-toggle-no-unlock (&optional arg)
  (interactive "P")
  (let ((value (fsvn-toggle-mode-line-variable
		arg 'fsvn-message-edit-no-unlock
		" (No Unlock)" "no unlock")))
    (fsvn-message-edit-substitute-subcommand-arg "--no-unlock" value)))

(defun fsvn-message-edit-toggle-keep-changelist (&optional arg)
  (interactive "P")
  (let ((value (fsvn-toggle-mode-line-variable
		arg 'fsvn-message-edit-keep-changelist
		" (Keep Changelist)" "keep changelist")))
    (fsvn-message-edit-substitute-subcommand-arg "--keep-changelist" value)))



(provide 'fsvn-msgedit)

;;; fsvn-msgedit.el ends here
