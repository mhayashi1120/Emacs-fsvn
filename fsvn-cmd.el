;;; fsvn-cmd.el --- Subversion batch command utilities for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(defun fsvn-get-prop-value-alist (urlrev)
  (mapcar
   (lambda (propname)
     (cons propname (fsvn-get-propget propname urlrev)))
   (fsvn-get-proplist urlrev)))

(defun fsvn-get-propget (propname url)
  (with-temp-buffer
    ;; don't use --xml because 1.4.x not supported.
    (when (= (fsvn-call-command "propget" t propname url) 0)
      (fsvn-buffer-string-propget propname))))

(defun fsvn-get-revprop (propname urlrev)
  (let ((rev (fsvn-urlrev-revision urlrev))
	(url (fsvn-urlrev-url urlrev)))
    (with-temp-buffer
      (when (= (fsvn-call-command
		"propget" t
		propname
		"--revprop"
		"--revision" (or rev "HEAD")
		url)
	       0)
	(fsvn-buffer-string-propget propname)))))

(defun fsvn-get-propget-file (propname url)
  (let ((value (fsvn-get-propget propname url))
	(file (fsvn-make-temp-file)))
    (write-region value nil file nil 'no-msg)
    file))

(defun fsvn-get-proplist (urlrev)
  (with-temp-buffer
    (if (fsvn-svn-subcommand-accepted-argument "proplist" "--xml")
	;;FIXME proplist --xml accept --verbose arg this makes all values get.
	(when (= (fsvn-call-command "proplist" t "--xml" urlrev) 0)
	  (mapcar
	   (lambda (node)
	     (fsvn-xml-properties->target->property.name node))
	   (fsvn-xml-properties->target->properties (car (fsvn-xml-parse-proplist)))))
      ;; for svn 1.4.x
      (when (= (fsvn-call-command "proplist" t urlrev) 0)
	;; first line is header info so `cdr'
	(mapcar 'fsvn-string-rm-lspace (cdr (fsvn-text-buffer-line-as-list)))))))

(defun fsvn-get-revprops (urlrev)
  (let ((rev (fsvn-urlrev-revision urlrev))
	(url (fsvn-urlrev-url urlrev)))
    (with-temp-buffer
      (if (fsvn-svn-subcommand-accepted-argument "proplist" "--xml")
	  ;;FIXME proplist --xml accept --verbose arg this makes all values get.
	  (when (= (fsvn-call-command "proplist" t "--xml" "--revprop" "--revision" (or rev "HEAD") url) 0)
	    (mapcar
	     (lambda (node)
	       (fsvn-xml-properties->revprops->property.name node))
	     (fsvn-xml-properties->revprops->properties (car (fsvn-xml-parse-revprops)))))
	;; for svn 1.4.x
	(when (= (fsvn-call-command "proplist" t "--revprop" "--revision" (or rev "HEAD") url) 0)
	  ;; first line is header info so `cdr'
	  (mapcar 'fsvn-string-rm-lspace (cdr (fsvn-text-buffer-line-as-list))))))))

(defun fsvn-get-svn:ignore-as-list (directory)
  "get `svn:ignore' property as list."
  (with-temp-buffer
    (let (args)
      (setq args (list "svn:ignore" directory))
      (when (= (apply 'fsvn-call-command "propget" t args) 0)
	(fsvn-text-buffer-line-as-list)))))

(defun fsvn-get-ls (urlrev)
  (with-temp-buffer
    (let (args)
      (setq args (list urlrev "--xml"))
      (when (= (apply 'fsvn-call-command "list" t args) 0)
	(fsvn-xml-parse-lists-entries)))))

(defun fsvn-get-root (url)
  (let ((info (fsvn-get-info-entry url)))
    (fsvn-xml-info->entry=>repository=>root$ info)))

(defun fsvn-get-root-upward (url)
  (let ((tmp url)
	root)
    (while (and tmp (null root))
      (condition-case err
	  (setq root (fsvn-xml-info->entry=>repository=>root$ (fsvn-get-info-entry tmp)))
	(error))
      (setq tmp (fsvn-url-dirname tmp)))
    root))

(defun fsvn-get-ls-entry (urlrev)
  (let ((entries (fsvn-get-ls (fsvn-urlrev-dirname urlrev))))
    (fsvn-find-first
     (lambda (key item)
       (string= (fsvn-xml-lists->list->entry=>name$ item) key))
     (fsvn-urlrev-filename urlrev)
     entries)))

(defun fsvn-get-info-entry (urlrev)
  "URLREV is string or list of string."
  (car (fsvn-get-info-list (list urlrev))))

(defun fsvn-get-info-list (urlrev-list)
  "URLREV-LIST is string or list of string."
  (with-temp-buffer
    (let (target args)
      (setq target (fsvn-make-targets-file urlrev-list))
      (setq args (list "--targets" target "--xml"))
      (when (= (apply 'fsvn-call-command "info" t args) 0)
	(fsvn-xml-parse-info)))))

(defun fsvn-get-directory-files-status (directory)
  (with-temp-buffer
    (let ((args (list "--xml" "--verbose" "--non-recursive" "--no-ignore" directory)))
      (when (= (apply 'fsvn-call-command "status" t args) 0)
	(fsvn-xml-status->entries (fsvn-xml-parse-status))))))

(defun fsvn-get-file-status (file)
  (with-temp-buffer
    (let ((args (list "--xml" "--verbose" "--non-recursive" "--no-ignore" file))
	  entries)
      (when (= (apply 'fsvn-call-command "status" t args) 0)
	(setq entries (fsvn-xml-status->entries (fsvn-xml-parse-status)))
	(fsvn-find-status-entry entries file)))))

(defun fsvn-get-directory-status (directory)
  (with-temp-buffer
    (let ((args (list "--xml" directory)))
      (when (= (apply 'fsvn-call-command "status" t args) 0)
	(fsvn-xml-status->entries (fsvn-xml-parse-status))))))

(defun fsvn-get-file-revision-log (file rev)
  (with-temp-buffer
    (let ((args (list "--revision" rev "--xml" "--verbose" file)))
      (when (= (apply 'fsvn-call-command "log" t args) 0)
	(car (fsvn-xml-parse-logentry))))))

(defun fsvn-get-file-parent-property (file propname &optional with-dir)
  (if (fsvn-file-exact-directory-p file)
      (fsvn-get-directory-parent-property file propname with-dir)
    (let ((dir (file-name-directory (directory-file-name file))))
      (fsvn-get-directory-parent-property dir propname with-dir))))

(defun fsvn-get-directory-parent-property (directory propname &optional with-dir)
  (let ((dir directory))
    (catch 'found
      (while (and (fsvn-directory-versioned-p dir)
		  (not (fsvn-file-name-root-p dir)))
	(let (value)
	  (when (setq value (fsvn-meta-get-property propname dir))
	    (if with-dir
		(throw 'found (cons dir value))
	      (throw 'found value))))
	(setq dir (fsvn-file-name-directory2 dir)))
      nil)))

(defun fsvn-get-file-logs (file &optional rev-range)
  (with-temp-buffer
    (let ((args (list "--xml" "--verbose")))
      (when rev-range
	(setq args (append args (list "--revision" (fsvn-revision-range-to-string rev-range)))))
      (setq args (append args (list file)))
      (when (= (apply 'fsvn-call-command "log" t args) 0)
	(fsvn-xml-parse-logentry)))))

(defun fsvn-get-files-logs (files &optional rev-range)
  (let (entries)
    (mapc
     (lambda (file)
       (setq entries (cons (fsvn-get-file-logs file rev-range) entries)))
     files)
    ;; change sort same as `fsvn-get-file-logs'
    (nreverse (apply 'fsvn-logs-unique-merge entries))))

(defun fsvn-get-file-blame (file &optional rev-range)
  (with-temp-buffer
    (let ((args (list "--xml")))
      (when rev-range
	(setq args (append args (list "--revision" (fsvn-revision-range-to-string rev-range)))))
      (setq args (append args (list file)))
      (when (= (apply 'fsvn-call-command "blame" t args) 0)
	(car (fsvn-xml-parse-blame))))))

(defun fsvn-get-file-blame-logs (file &optional rev-range)
  (let ((logs (fsvn-get-file-logs file rev-range))
	(blame (fsvn-get-file-blame file rev-range)))
    (mapcar
     (lambda (entry)
       (let ((rev (fsvn-xml-blame->target->entry=>commit.revision entry)))
	 (if rev
	     (fsvn-logs-find-logentry logs rev)
	   nil)))
     (fsvn-xml-blame->target->entries blame))))

(defun fsvn-get-prop-svn:needs-lock (file)
  (fsvn-get-boolean-prop-value file "svn:needs-lock"))

(defun fsvn-get-boolean-prop-value (file propname)
  (not (not (fsvn-get-propget propname file))))

(defun fsvn-get-temporary-wc (urlrev)
  (with-temp-buffer
    (let ((dir (fsvn-make-temp-directory)))
      (unless (= (fsvn-call-command "checkout" t "--non-recursive" urlrev dir) 0)
	(error "Error while svn `checkout' subcommand"))
      dir)))

(defun fsvn-get-cat-buffer (urlrev)
  (let ((std-buf (fsvn-make-temp-buffer))
	(err-file (fsvn-make-temp-file))
	(args (list urlrev)))
    (when (= (apply 'fsvn-call-command "cat" (cons std-buf err-file) args) 0)
      (when (= (nth 7 (file-attributes err-file)) 0)
	(with-current-buffer std-buf
	  (save-excursion
	    (goto-char (point-min))
	    (rename-buffer (fsvn-urlrev-filename urlrev) t)
	    (setq buffer-file-name urlrev)
	    (set-buffer-modified-p nil)
	    (set-auto-mode)
	    std-buf))))))



;; with side effect svn subcommand

(defun fsvn-asap-add-file (file dest-url &optional filename)
  "DEST-URL is destination.
FILENAME non-nil means ignore DEST-URL filename section."
  (let* ((dest-info (fsvn-get-info-entry dest-url))
	 (dest-dir (fsvn-url-dirname dest-url))
	 dest)
    (unless filename
      (setq filename (fsvn-file-name-nondirectory file)))
    (cond
     ((null dest-info)
      (setq dest dest-url))
     ((eq (fsvn-xml-info->entry.kind dest-info) 'dir)
      (setq dest (fsvn-expand-url filename dest-url)))
     (t
      (signal 'file-error (list "Svn Repository file already exists" dest-url))))
    (fsvn-call-command-discard "import"
			       "--message" (fsvn-config-magic-remote-commit-message dest-url)
			       file dest)
    t))

(defun fsvn-asap-delete-url (url)
  (fsvn-call-command-discard "delete"
			     "--message" (fsvn-config-magic-remote-commit-message url)
			     url))

(defun fsvn-asap-modify-url-from-buffer (buffer url)
  "Substitute URL(file) contents as BUFFER
"
  (let ((tmpfile (fsvn-make-temp-file)))
    (with-current-buffer buffer
      (write-region (point-min) (point-max) tmpfile nil 'no-msg))
    (fsvn-asap-modify-url-from-file tmpfile url)))

(defun fsvn-asap-modify-url-from-file (file url)
  "Substitute URL(file) contents as FILE
"
  (let* ((wc (fsvn-get-temporary-wc (fsvn-urlrev-dirname url)))
	 (filename (fsvn-url-filename url))
	 (tmpfile (fsvn-expand-file filename wc)))
    (unless (file-exists-p tmpfile)
      (error "Repository was modified"))
    (copy-file file tmpfile t)
    (fsvn-call-command-discard "commit"
			       "--message" (fsvn-config-magic-remote-commit-message url)
			       tmpfile)))



(defun fsvn-set-prop-value (file propname value)
  (let ((tmpfile (fsvn-get-prop-temp-file propname value)))
    (fsvn-call-command-discard "propset" propname "--file" tmpfile file)))

(defun fsvn-set-revprop-value (urlrev propname value)
  (let ((url (fsvn-urlrev-url urlrev))
	(rev (fsvn-urlrev-revision urlrev))
	(tmpfile (fsvn-get-prop-temp-file propname value)))
    (fsvn-call-command-discard "propset" propname
			       "--file" tmpfile
			       "--revprop"
			       "--revision" rev
			       url)))

(defun fsvn-set-prop-delete (file propname)
  (fsvn-call-command-discard "propdel" propname file))

(defun fsvn-duplicate-all-properties (from-file to-file)
  "Overwrite TO-FILE properties by FROM-FILE properties with ignoring all conflict."
  (mapc
   (lambda (p)
     (fsvn-set-prop-delete to-file p))
   (fsvn-get-proplist to-file))
  (mapc
   (lambda (p)
     (fsvn-set-prop-value to-file (car p) (cdr p)))
   (fsvn-get-prop-value-alist from-file)))

(defun fsvn-add-prop-svn:ignore (dir files)
  "DIR is parent of FILES
FILES accept a file as string."
  (let ((current (fsvn-get-svn:ignore-as-list dir))
	values)
    (setq values current)
    (mapc
     (lambda (file)
       (unless (member file values)
	 (setq values (cons (fsvn-url-filename file) values))))
     (cond
      ((stringp files) (list files))
      ((listp files) files)))
    (fsvn-set-prop-value dir "svn:ignore" (mapconcat 'identity values "\n"))))

(defun fsvn-set-prop-svn:executable (file value)
  (fsvn-set-boolean-prop-value file "svn:executable" value))

(defun fsvn-set-prop-svn:needs-lock (file value)
  (fsvn-set-boolean-prop-value file "svn:needs-lock" value))

(defun fsvn-set-boolean-prop-value (file propname value)
  (if value
      (fsvn-set-prop-value file propname "*")
    (fsvn-set-prop-delete file propname)))



;; todo svn bug? move to some file...?
(defun fsvn-rename-case-missing-file (file)
  (let* ((tmp (fsvn-make-temp-filename file))
	 (dir (fsvn-file-name-directory file))
	 (orig-name (fsvn-file-name-nondirectory file))
	 (case-fold-search t)
	 (target-entry
	  (catch 'found
	    (mapc
	     (lambda (ls-entry)
	       (when (string-match (format "^%s$" orig-name) (fsvn-xml-lists->list->entry=>name$ ls-entry))
		 (throw 'found ls-entry)))
	     (fsvn-get-ls dir))))
	 new-name)
    (if (or (null target-entry) 
	    (and (setq new-name (fsvn-xml-lists->list->entry=>name$ target-entry))
		 (string= new-name orig-name)))
	(message "Nothing to do.")
      (rename-file file tmp)
      (rename-file tmp (fsvn-expand-file new-name dir))
      (message "Renaming done.(%s -> %s)" orig-name new-name))))



;; utilities for command arguments.

(defun fsvn-buffer-string-propget (propname)
  (if (= (point-min) (point-max))
      nil
    ;;FIXME
    (fsvn-string-convert-cs
     (buffer-substring-no-properties (point-min) (1- (point-max)))
     (terminal-coding-system)
     (fsvn-prop-file-coding-system propname))))

(defvar fsvn-targets-file-converter 'identity
  "File name converter for function `--targets' argument.
Default value is `identity'
Usefull for cygwin version `svn'")

(defun fsvn-make-targets-file (files)
  "Create --targts argument file.
Argument FILES ."
  (let ((tmpfile (fsvn-make-temp-file)))
    (with-temp-buffer
      (let ((coding-system-for-write (fsvn-file-name-coding-system)))
	(mapc
	 (lambda (f)
	   (insert (funcall fsvn-targets-file-converter f) "\n"))
	 files)
	(write-region (point-min) (point-max) tmpfile nil 'no-msg)))
    tmpfile))

(defun fsvn-get-prop-temp-file (propname value)
  (let ((tmpfile (fsvn-make-temp-file))
	(coding-system-for-write (fsvn-prop-file-coding-system propname)))
    (write-region value nil tmpfile nil 'no-msg)
    tmpfile))






(provide 'fsvn-cmd)

;;; func-cmd.el ends here
