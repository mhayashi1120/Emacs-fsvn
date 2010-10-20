;;; fsvn-pub.el --- Fsvn public utilities


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'dired)

(require 'fsvn-popup)
(require 'fsvn-browse)
(require 'fsvn-debug)
(require 'fsvn-magic)
(require 'fsvn-ui)
(require 'fsvn-cmd)
(require 'fsvn-env)
(require 'fsvn-minibuf)
(require 'fsvn-fs)
(require 'fsvn-logview)



(defvar iswitchb-buffer-ignore)
(defvar auto-mode-alist)
(defvar current-prefix-arg)
(defvar find-directory-functions)
(defvar file-name-handler-alist)
(defvar system-type)



(defconst fsvn-advised-alist
  '((dired around fsvn-dired-mode)
    (dired-goto-file around fsvn-dired-goto-file-ad)
    (after-find-file around fsvn-after-find-file)
    (vc-find-file-hook after fsvn-ui-fancy-vc-find-file-hook)
    (vc-find-file-hook after fsvn-ui-fancy-vc-find-file-hook-revert)
    (vc-after-save after fsvn-ui-fancy-vc-after-save)
    (ediff-refresh-mode-lines around fsvn-ui-fancy-ediff-modeline-fixup)
    ))

;; global command
(defun fsvn-cleanup-log-message ()
  "Cleanup cached log messages."
  (interactive)
  (let ((topdir (fsvn-logmessage-directory))
	time file
	(deleted 0)
	(renamed 0))
    (mapc
     (lambda (d)
       (when (file-directory-p d)
	 (let (messages msg)
	   (mapc
	    (lambda (f)
	      (setq msg (fsvn-get-file-contents f))
	      (cond
	       ((fsvn-string-assoc msg messages nil)
		(delete-file f)
		(setq deleted (1+ deleted)))
	       (t
		(setq messages (cons msg messages))
		(setq time (format-time-string "%s" (nth 5 (file-attributes f))))
		(unless (string-match (concat "^" time) (fsvn-file-name-nondirectory f))
		  (while (file-exists-p (setq file (make-temp-name (fsvn-expand-file time d)))))
		  (rename-file f file)
		  (setq renamed (1+ renamed))))))
	    (directory-files d t dired-re-no-dot)))))
     (directory-files topdir t dired-re-no-dot))
    (message "%d renamed %d deleted." renamed deleted)))

(defun fsvn-show-svn-help (subcommand)
  "Show SUBCOMMAND help."
  (interactive (list (fsvn-read-svn-subcommand)))
  (let ((fsvn-process-environment-lang fsvn-help-locale))
    (fsvn-popup-call-process "help" subcommand)))

(defun fsvn-global-cleanup-buffer ()
  "Cleanup popuped non-active buffers."
  (interactive)
  (when (y-or-n-p "Cleanup waste (non-active) buffer? ")
    (let ((count 0))
      (setq count (+ count (fsvn-cleanup-temp-buffer)))
      (setq count (+ count (fsvn-cleanup-result-buffer)))
      (cond
       ((= count 0)
	(message "No buffer was killed."))
       ((= count 1)
	(message "killed a buffer."))
       (t
	(message "killed %d buffers." count))))))

(defun fsvn-forward-popup-result-buffer ()
  (interactive)
  (fsvn-cycle-popup-result-buffer))

(defun fsvn-backward-popup-result-buffer ()
  (interactive)
  (fsvn-cycle-popup-result-buffer t))

(defun fsvn-cycle-popup-result-buffer (&optional backward)
  (let ((buffers (fsvn-popup-result-buffer-list))
	comparator)
    (if (null buffers)
	(message "No popup buffer.")
      (if backward
	  ;; FIXME howto clean?
	  (setq comparator (lambda (x y) (not (string-lessp (buffer-name x) (buffer-name y)))))
	(setq comparator (lambda (x y) (string-lessp (buffer-name x) (buffer-name y)))))
      (setq buffers (sort buffers comparator))
      (let (exists next window)
	(cond
	 ((= (length (window-list)) 1)
	  (setq next (car buffers)))
	 ((setq exists (catch 'found
			 (mapc
			  (lambda (b)
			    (when (memq b (mapcar 'window-buffer (window-list)))
			      (throw 'found b)))
			  buffers)
			 nil))
	  (unless (setq next (cadr (memq exists buffers)))
	    (setq next (car buffers))))
	 (t
	  (setq next (car buffers))))
	(cond
	 ((= (length (window-list)) 1)
	  (split-window)
	  (setq window (cadr (window-list))))
	 (exists
	  (setq window (get-buffer-window exists)))
	 (t
	  (setq window (cadr (window-list)))))
	(set-window-buffer window next)))))

(defun fsvn-browse-wc-noselect (directory)
  (save-excursion
    (let ((dir (directory-file-name (fsvn-expand-file directory))))
      (when (fsvn-directory-versioned-p dir)
	(fsvn-browse-draw-local-directory dir)
	(set-visited-file-modtime (current-time))
	(setq buffer-read-only t)
	(run-mode-hooks 'fsvn-browse-mode-hook)
	(current-buffer)))))

(defun fsvn-save-file (urlrev file &optional no-msg revision)
  "Save URLREV as FILE.
Optional argument NO-MSG suppress message.
Optional argument REVISION means point of URLREV log chain."
  (with-temp-buffer
    (if (= (fsvn-call-command "export" (current-buffer)
			      "--quiet"
			      urlrev (when revision (list "--revision" revision))
			      file) 0)
	(progn
	  (unless no-msg
	    (message "Save done."))
	  ;; return
	  t)
      (when (file-exists-p file)
	(delete-file file))
      (unless no-msg
	(message "Save failed."))
      ;; return
      nil)))

(defun fsvn-save-file-background (urlrev file &optional revision)
  "Save URLREV as FILE in background.
Optional argument REVISION means point of URLREV log chain."
  (let* ((buffer (fsvn-make-temp-buffer))
	 proc)
    (setq proc (fsvn-start-command "export" buffer
				   "--quiet"
				   urlrev (when revision (list "--revision" revision))
				   file))
    (process-put proc 'fsvn-save-file-name file)
    (set-process-sentinel proc 'fsvn-save-file-sentinel)
    proc))

(defun fsvn-save-file-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (let ((file (process-get proc 'fsvn-save-file-name)))
      (if (and (= (process-exit-status proc) 0)
	       (fsvn-save-file-validate-buffer))
	  (message "Save done. \"%s\"" file)
	(when (file-exists-p file)
	  (delete-file file))
	(message "Save failed. \"%s\"" file))
      (kill-buffer (current-buffer)))))

(defun fsvn-save-file-validate-buffer ()
  "Validate after cat stderr."
  (save-excursion
    (goto-char (point-min))
    (not (re-search-forward "^svn: warning:" nil t))))

(defun fsvn-cleanup-temp-buffer ()
  (fsvn-cleanup-buffer fsvn-temp-buffer-p))

(defun fsvn-cleanup-result-buffer ()
  (fsvn-cleanup-buffer fsvn-popup-result-buffer-p))

(defvar fsvn-password-prompt-accessible-p t)

(defvar fsvn-authenticate-password-prompt-shown nil)

(defun fsvn-authenticate-repository (repository)
  "Authenticate by `svn' to REPOSITORY."
  (interactive (list (fsvn-completing-read-url "Authenticate URL: " nil t)))
  (if (not fsvn-password-prompt-accessible-p)
      ;;TODO FIXME not works prompt on windows binary.
      (funcall 'fsvn-win-authenticate-repository repository)
    (let ((buffer (fsvn-make-temp-buffer))
	  (coding-system-for-write 'unix)
	  proc)
      (setq fsvn-authenticate-password-prompt-shown nil)
      (setq proc (fsvn-start-command "info" buffer repository))
      (set-process-sentinel proc 'fsvn-authenticate-sentinel)
      (set-process-filter proc 'fsvn-authenticate-filter)
      (while (eq (process-status proc) 'run)
	(discard-input)
	(sit-for 0.5))
      proc)))

(defun fsvn-authenticate-filter (proc event)
  (fsvn-process-event-handler proc event
    (goto-char (point-max))
    (insert event)
    (let ((prompt (fsvn-parse-result-if-auth-prompt proc)))
      (when prompt
	(setq fsvn-authenticate-password-prompt-shown t)))))

(defun fsvn-authenticate-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (kill-buffer (current-buffer))
    (cond
     ((/= (process-exit-status proc) 0)
      (message "Failed authenticate."))
     ((not fsvn-authenticate-password-prompt-shown)
      (message "Already authenticated."))
     (t
      (message "Authenticated.")))))



;; global map command
(defun fsvn-checkout (url &optional args)
  "Execute `checkout' URL to current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-cmd-read-checkout-args))
  (let ((dir (fsvn-expand-file default-directory)))
    (when (or (= (length (fsvn-directory-files dir)) 0)
	      (y-or-n-p "This directory is not empty.  Really checkout? "))
      (fsvn-popup-start-process "checkout" args url dir))))

(defun fsvn-start (repository &optional rev)
  (interactive (let ((url (fsvn-completing-read-url))
		     rev)
		 (when current-prefix-arg
		   (setq rev (fsvn-completing-read-revision nil nil nil url)))
		 (list url rev)))
  (fsvn-browse-switch-directory-buffer (fsvn-url-urlrev repository rev)))

(defun fsvn-import (file url &optional args)
  "Execute `import' FILE to URL.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-cmd-read-import-args))
  (let ((root (fsvn-get-root-upward url))
	(browse-buffer (current-buffer))
	(win-configure (current-window-configuration))
	(msgedit-buffer (fsvn-message-edit-generate-buffer)))
    (unless root
      (error "Unable to get root repository"))
    (with-current-buffer msgedit-buffer
      (fsvn-message-edit-mode)
      (setq fsvn-buffer-repos-root root)
      (fsvn-parasite-import-mode 1)
      (setq fsvn-parasite-import-target-path file)
      (setq fsvn-parasite-import-target-url url)
      (setq fsvn-parasite-import-subcommand-args args)
      (setq fsvn-previous-window-configuration win-configure)
      (run-mode-hooks 'fsvn-message-edit-mode-hook))
    (fsvn-parasite-setup-message-edit-window msgedit-buffer)))

(defun fsvn-open-repository (urlrev)
  "Open URLREV by repository browser."
  (interactive (list (fsvn-completing-read-urlrev)))
  (fsvn-browse-switch-directory-buffer urlrev))

(defun fsvn-debug-toggle (&optional arg no-msg)
  "Toggle debug output enable/disable.

\(fn ARG)"
  (interactive "P")
  (setq fsvn-debug-enabled
	(fsvn-toggle-command-boolean arg fsvn-debug-enabled))
  (unless no-msg
    (message "fsvn debug %s." (if fsvn-debug-enabled "enabled" "disabled"))))

(defun fsvn-command (subcommand args)
  "Execute `svn SUBCOMMAND ARGS'"
  (interactive (let* ((subcommand (fsvn-read-svn-subcommand))
		      (args (fsvn-read-svn-subcommand-args subcommand)))
		 (list subcommand args)))
  (fsvn-popup-start-process subcommand args))

(defun fsvn-toggle-feature (&optional arg no-msg)
  "Toggle `fsvn' feature enable/disable.

\(fn ARG)"
  (interactive "P")
  (let* ((featured (memq 'fsvn-browse-wc-noselect find-directory-functions))
	 (feature (fsvn-toggle-command-boolean arg featured))
	 file-handler auto-mode ignore-buffers)
    (mapc
     (lambda (x)
       (let (enabler activator)
	 (if feature
	     (setq enabler 'ad-enable-advice
		   activator 'ad-activate)
	   (setq enabler 'ad-disable-advice
		 activator 'ad-deactivate))
	 (funcall enabler (nth 0 x) (nth 1 x) (nth 2 x))
	 (funcall activator (nth 0 x))))
     fsvn-advised-alist)
    (setq file-handler (assoc fsvn-magic-file-name-regexp file-name-handler-alist))
    (setq auto-mode (list (concat "@\\(?:" fsvn-revision-regexp "\\)$") 'ignore t))
    (setq ignore-buffers
	  (list
	   (concat "^" (regexp-quote fsvn-log-sibling-buffer-name) "$")
	   (concat "^" (regexp-quote fsvn-log-message-buffer-name) "$")))
    (unless (boundp 'iswitchb-buffer-ignore)
      (setq iswitchb-buffer-ignore nil))
    (cond
     (feature
      ;; for ediff
      (add-to-list 'auto-mode-alist auto-mode)
      ;; update status display
      (add-hook 'after-save-hook 'fsvn-after-save-hook)
      ;; for bookmark or else
      (add-to-list 'find-directory-functions 'fsvn-browse-wc-noselect)
      ;; for magic utility
      (unless file-handler
	(setq file-handler (cons fsvn-magic-file-name-regexp 'fsvn-magic-file-name-handler))
	(add-to-list 'file-name-handler-alist file-handler))
      (add-hook 'pre-command-hook 'fsvn-magic-clear-cache-if-toplevel)
      ;; iswitchb ignore buffers
      (mapc
       (lambda (regexp)
	 (add-to-list 'iswitchb-buffer-ignore regexp))
       ignore-buffers))
     (t
      (setq auto-mode-alist (delete auto-mode auto-mode-alist))
      (remove-hook 'after-save-hook 'fsvn-after-save-hook)
      (setq find-directory-functions (delq 'fsvn-browse-wc-noselect find-directory-functions))
      (setq file-name-handler-alist (delq file-handler file-name-handler-alist))
      (remove-hook 'pre-command-hook 'fsvn-magic-clear-cache-if-toplevel)
      (mapc
       (lambda (regexp)
	 (setq iswitchb-buffer-ignore (delete regexp iswitchb-buffer-ignore)))
       ignore-buffers)))
    (unless no-msg
      (message "Now fsvn feature `%s'" (if feature "ON" "OFF")))))



(defun fsvn-cmd-read-patch-file ()
  (let* ((patch (fsvn-read-file-name "Patch file: ")))
    (when (file-exists-p patch)
      (unless (y-or-n-p "File exists. Overwrite? ")
	(signal 'quit nil)))
    (list (fsvn-expand-file patch))))

(defun fsvn-cmd-read-checkout-args ()
  (let (url args)
    (fsvn-brief-message-showing
     (setq url (fsvn-completing-read-url "Checkout URL: "))
     (fsvn-brief-message-add-message (format "Checkout: %s" url)))
    (setq args (fsvn-cmd-read-subcommand-args "checkout" fsvn-default-args-checkout))
    (list url args)))

(defun fsvn-cmd-read-import-args ()
  (let (file url args)
    (fsvn-brief-message-showing
     (setq file (fsvn-read-file-name "Imported file: " nil nil t))
     (fsvn-brief-message-add-message (format "Imported: %s" file))
     (setq url (fsvn-completing-read-url "Import to URL: "))
     (fsvn-brief-message-add-message (format "Import to: %s" url)))
    (setq args (fsvn-cmd-read-subcommand-args "import" fsvn-default-args-import))
    (list file url args)))


;; * vc like global utility.

(defun fsvn-vc-print-log ()
  "Execute `log' for current file."
  (interactive)
  (unless buffer-file-name
    (error "Buffer is not associated with a file"))
  (unless (fsvn-meta-file-registered-p buffer-file-name)
    (error "Buffer file is not under versioned"))
  (fsvn-open-logview-mode buffer-file-name nil))

(defun fsvn-vc-commit (&optional arg)
  "Prepare `commit' buffer for buffer file."
  (interactive (list (fsvn-cmd-read-subcommand-args "commit" fsvn-default-args-commit)))
  (unless buffer-file-name
    (error "Buffer is not associated with a file"))
  (when (and (buffer-modified-p)
	     (y-or-n-p "Buffer modified. Save? "))
    (save-buffer nil))
  (let ((fsvn-buffer-repos-root (fsvn-get-root default-directory)))
    (unless fsvn-buffer-repos-root
      (error "Buffer file is not under versioned"))
    (fsvn-browse-commit-mode (list buffer-file-name) arg)))



(defvar fsvn-initialize-function nil)

(defun fsvn-initialize-loading ()
  (fsvn-set-command-information)
  (unless (file-directory-p fsvn-home-directory)
    (make-directory fsvn-home-directory t))
  (mapc
   (lambda (dir)
     (let ((dirname (fsvn-expand-file dir fsvn-home-directory)))
       (unless (file-directory-p dirname)
	 (make-directory dirname))))
   fsvn-temp-directory-dirs)
  (fsvn-cleanup-temp-directory)
  (fsvn-build-subcommand)
  (when fsvn-initialize-function
    (funcall fsvn-initialize-function))
  (fsvn-toggle-feature t 'no-msg))

(defun fsvn-toggle-command-boolean (optional-arg current-value)
  (cond
   ((and (numberp optional-arg) (= optional-arg 0))
    nil)
   (optional-arg
    t)
   (t
    (not current-value))))

(defun fsvn-working-copy (directory &optional force-reload)
  "Open directory as `fsvn-browse-mode'."
  (let ((canon (directory-file-name (fsvn-expand-file directory))))
    (fsvn-browse-switch-directory-buffer canon)))

(defun fsvn-after-save-hook ()
  (condition-case err
      (when (buffer-file-name)
	(let* ((file (buffer-file-name))
	       (base (fsvn-meta-text-base-file file))
	       size1 size2)
	  (fsvn-save-browse-file-excursion file
	    (if (or (null base)
		    (string= 
		     (downcase (or (fsvn-meta-get-property "svn:eol-style" file) ""))
		     "native"))
		(fsvn-browse-draw-file-status file)
	      (setq size1 (fsvn-file-size file)
		    size2 (fsvn-file-size base))
	      (if (and size1 size2 (/= size1 size2))
		  ;; changed file size means certainly modified.
		  (fsvn-browse-put-status-if-weak-internal file ?M 0)
		;; delegate to `status' subcommand.
		(fsvn-browse-draw-file-status file))))))
    (error nil)))

(defun fsvn-get-exists-browse-buffer (urlrev)
  (catch 'found
    (cond
     ((fsvn-url-repository-p urlrev)
      (fsvn-each-browse-buffer
       (let ((url (fsvn-urlrev-url urlrev)))
	 (when (string-match (concat "^" (regexp-quote fsvn-buffer-repos-root) "\\(.*\\)") url)
	   (let ((regexp (format fsvn-browse-re-format-subdir (match-string 1 url))))
	     (save-excursion
	       (goto-char (point-min))
	       (when (re-search-forward regexp nil t)
		 (throw 'found (current-buffer)))))))))
     (t
      (let ((regexp (format fsvn-browse-re-format-subdir (regexp-quote urlrev))))
	(fsvn-each-browse-buffer
	 (save-excursion
	   (goto-char (point-min))
	   (when (re-search-forward regexp nil t)
	     (throw 'found (current-buffer))))))))
    nil))

(defun fsvn-local-directory-buffer (directory)
  (let ((dir (directory-file-name (fsvn-expand-file directory))))
    (catch 'found
      (fsvn-each-browse-buffer
       (mapc
	(lambda (subdir)
	  (when (string= (car subdir) dir)
	    (throw 'found (current-buffer))))
	fsvn-browse-subdir-alist))
      nil)))

(defun fsvn-find-buffer-by-variable (var value)
  (save-excursion
    (catch 'found
      (mapc
       (lambda (b)
	 (set-buffer b)
	 (when (equal (eval var) value)
	   (throw 'found b)))
       (buffer-list))
      nil)))

(defun fsvn-open-logview-mode (urlrev directory-p &optional rev-range count)
  "Open URLREV log buffer.
Argument REV-RANGE revision range cons cell `(start . end)'
Argument COUNT max count of log. If ommited use `fsvn-repository-alist' settings.
"
  (let ((root (or (and fsvn-buffer-repos-root
		       (fsvn-url-contains-p fsvn-buffer-repos-root urlrev)
		       fsvn-buffer-repos-root)
		  (fsvn-get-root urlrev)))
	entries buffer win-config prev-entries)
    (setq buffer (fsvn-log-list-get-buffer urlrev))
    (cond
     ((eq buffer (current-buffer))
      (setq win-config fsvn-previous-window-configuration)
      (setq prev-entries fsvn-log-list-all-entries))
     (t
      (setq win-config (current-window-configuration))))
    (setq entries (fsvn-log-list-cmd urlrev root rev-range count))
    (if (null entries)
	(if prev-entries
	    (message "No more log entry.")
	  (message "No log entry."))
      (set-buffer buffer)
      (let ((first (car (last entries)))
	    (last (car entries))
	    buffer-read-only)
	(when (> (fsvn-xml-log->logentry.revision first)
		 (fsvn-xml-log->logentry.revision last))
	  (setq entries (nreverse entries))
	  (setq first (car (last entries))
		last (car entries)))
	(fsvn-log-list-mode)
	(setq fsvn-logview-target-directory-p directory-p)
	(setq fsvn-logview-target-urlrev urlrev)
	(setq fsvn-buffer-repos-root root)
	(setq fsvn-previous-window-configuration win-config)
	(setq fsvn-log-list-all-entries (fsvn-logs-unique-merge entries prev-entries))
	(setq fsvn-log-list-target-path
	      (if (fsvn-url-local-p urlrev)
		  (fsvn-wc-file-repository-path urlrev)
		(fsvn-repository-path root urlrev)))
	(setq fsvn-log-list-entries entries)
	(erase-buffer)
	(fsvn-log-list-insert-header-entry urlrev first last)
	(mapc
	 (lambda (entry)
	   (fsvn-log-list-insert-entry entry))
	 entries)
	(fsvn-log-list-goto-first-revision))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (switch-to-buffer buffer)
      (fsvn-log-list-setup-window)))
  (run-mode-hooks 'fsvn-log-list-mode-hook))



(defun fsvn-open-propview-mode (root urlrev directory-p working-dir)
  (let ((win-configure (current-window-configuration)))
    ;; for proplist mode
    (with-current-buffer (fsvn-proplist-get-buffer)
      (fsvn-proplist-mode)
      (setq fsvn-buffer-repos-root root)
      (setq fsvn-propview-target-urlrev urlrev)
      (setq fsvn-propview-target-directory-p directory-p)
      (setq fsvn-previous-window-configuration win-configure)
      (setq fsvn-proplist-target-mode 'properties)
      (fsvn-set-default-directory working-dir)
      (fsvn-proplist-setup-window)
      (setq fsvn-default-window-configuration (current-window-configuration))
      (setq buffer-read-only t)
      (fsvn-proplist-draw-list urlrev)
      (fsvn-proplist-goto-first-property)
      (fsvn-proplist-draw-value (fsvn-proplist-current-propname))
      (run-mode-hooks 'fsvn-proplist-mode-hook))
    (switch-to-buffer (fsvn-proplist-get-buffer))))



(defun fsvn-run-recursive-status (directory)
  "Execute recursive `status', and set subordinate directory."
  (let (proc buffer)
    (setq proc (fsvn-recursive-status-running-process directory))
    (if proc
	(when (eq major-mode 'fsvn-browse-mode)
	  (setq fsvn-browse-buffer-directories-status-process proc))
      (setq buffer (fsvn-make-temp-buffer))
      (with-current-buffer buffer
	(make-local-variable 'fsvn-recursive-status-parsed))
      (setq proc (fsvn-start-command "status" buffer directory))
      (set-process-sentinel proc 'fsvn-recursive-status-sentinel)
      (set-process-filter proc 'fsvn-recursive-status-filter)
      (process-put proc 'fsvn-recursive-status-top-directory directory)
      (fsvn-recursive-status-set-subordinate-process directory proc)
      proc)))



(provide 'fsvn-pub)

;;; fsvn-pub.el ends here
