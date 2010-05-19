;;; fsvn-dev.el --- Experimental implementation.

;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



;; destination url -> msgedit
;; mv -> switch
(defun fsvn-browse-move-this-in-repository (src-file to-url &optional args)
  "Execute `move' for repository file corresponding local file.
Optional ARGS (with prefix arg) means read svn subcommand arguments.
"
  (interactive)
  )

(defun fsvn-browse-cmd-read-upgrade-source-tree-args ()
  (fsvn-browse-cmd-wc-only
   (let ((dir (fsvn-read-directory-name "New Source: " nil nil t)))
     (unless (y-or-n-p "This takes many seconds. ok? ")
       (error "quit"))
     (list dir))))

(defun fsvn-browse-upgrade-source-tree (new-source)
  (interactive (fsvn-browse-cmd-read-upgrade-source-tree-args))
  (fsvn-browse-wc-only
   (let ((wcpath (fsvn-browse-current-directory)))
     (fsvn-call-command-discard "update" wcpath)
     (fsvn-browse-upgrade-source-tree-internal wcpath new-source))))

(defun fsvn-browse-upgrade-source-tree-internal (wcpath new-source-path)
  (let ((old (fsvn-get-ls wcpath))
	(new (directory-files new-source-path nil dired-re-no-dot))
	old-files)
    (setq old-files (mapcar 'fsvn-xml-lists->list->entry=>name$ old))
    (mapc
     (lambda (newfile)
       (let ((nf (fsvn-expand-file newfile new-source-path))
	     (of (fsvn-expand-file newfile wcpath))
	     (oldfile (car (fsvn-file-member newfile old-files))))
	 (when (and oldfile (not (file-exists-p of)))
	   (error "Old file missing"))
	 (if oldfile
	     ;; overwrite old entries
	     (cond
	      ((fsvn-file-exact-directory-p nf)
	       (fsvn-browse-upgrade-source-tree-internal of nf))
	      (t
	       (copy-file nf of t)))
	   (cond
	    ((fsvn-file-exact-directory-p nf)
	     (fsvn-copy-directory nf of))
	    (t
	     (copy-file nf of t)))
	   (fsvn-call-command-discard "add" of))))
     new)
    (mapc
     (lambda (oldfile)
       (unless (fsvn-file-member oldfile new)
	 (let ((of (fsvn-expand-file oldfile wcpath)))
	   (fsvn-call-command-discard "delete" of))))
     old-files)))



(defmacro fsvn-import-commit-foreach (alist &rest form)
  "Each cell of ALIST bound to `FROM-URL' and `TO-FILE'"
  `(mapc
    (lambda (cell)
      (let ((FROM-URL (car cell))
	    (TO-FILE  (cdr cell)))
	,@form))
    ,alist))

(defun fsvn-import-commit (alist from-revs)
  "ALIST is (from-url . to-file)
FROM-REVS (from-rev . to-rev)
"
  (fsvn-import-commit-foreach alist
    (unless (fsvn-url-repository-p FROM-URL)
      (error "%s is not a url" FROM-URL))
    (unless (fsvn-url-local-p TO-FILE)
      (error "%s is not a local path" TO-FILE))
    (unless (fsvn-file-versioned-directory-p TO-FILE)
      (error "%s is not under versioned" TO-FILE))
    (unless (file-exists-p TO-FILE)
      (error "%s is not exists" TO-FILE))
    ;; todo check local file status
    )
  (let ((logs (fsvn-get-files-logs (mapcar 'car alist) from-revs)))
    (mapc
     (lambda (log)
       (let ((rev (fsvn-xml-log->logentry.revision log)))
	 (fsvn-import-commit-foreach alist
	   (let ((urlrev (fsvn-url-urlrev FROM-URL rev)))
	     ;; save all FROM-URL to TO-FILE
	     (unless (fsvn-save-file urlrev TO-FILE 'no-msg)
	       (error "Error while saving %s" urlrev))
	     )))
       ;; all saved file will be commited.
       ;; non change file will be simply ignored.
       (fsvn-call-command-discard "commit"
				  "--message" (or (fsvn-xml-log->logentry=>msg$ log) "")
				  (mapcar 'cdr alist)))
     logs)))

(defun fsvn-import-commit-url (to-url from-revs from-url)
  (let* ((log-entries (fsvn-get-file-logs from-url from-revs))
	 (wc (fsvn-get-temporary-wc (fsvn-url-dirname to-url)))
	 (filename (fsvn-file-name-nondirectory to-url))
	 (tmpfile (fsvn-expand-file filename wc)))
    (mapc
     (lambda (entry)
       (let ((urlrev (fsvn-url-urlrev from-url (fsvn-xml-log->logentry.revision entry))))
	 (unless (fsvn-save-file urlrev tmpfile 'no-msg)
	   (error "Error while saving %s" urlrev))
	 (fsvn-call-command-discard "commit"
				    "--message" (or (fsvn-xml-log->logentry=>msg$ entry) "")
				    tmpfile)))
     log-entries)))

;;todo not implements
(defun fsvn-import-commit-url2 (to-url from-revs from-url)
  (let* ((log-entries (fsvn-get-file-logs from-url from-revs))
	 (toinfo (fsvn-get-info-entry to-url)))
    (let (wc dir)
      (setq dir
	    (cond
	     ((null toinfo)
	      (fsvn-url-dirname to-url))
	     ((eq (fsvn-xml-info->entry.kind toinfo) 'dir)
	      to-url)
	     (t
	      (fsvn-url-dirname to-url))))
      (setq wc (fsvn-get-temporary-wc dir))
      (mapc
       (lambda (entry)
	 (let ((urlrev (fsvn-url-urlrev from-url (fsvn-xml-log->logentry.revision entry))))
	   (unless (= (fsvn-call-command "export" nil "--quiet" "--force" urlrev wc) 0)
	     (error "Error while `export' %s" urlrev))
	   (fsvn-call-command-discard "commit"
				      "--message" (or (fsvn-xml-log->logentry=>msg$ entry) "")
				      wc)))
       log-entries))))



(defun fsvn-logs-multiple-url (urlrevs)
  (let (ret)
    (mapc
     (lambda (urlrev)
       (with-temp-buffer
	 (let (tmp)
	   (unless (= (fsvn-call-command "log" (current-buffer) "--xml" "--verbose" urlrev) 0)
	     (error "Error while executing `svn log'"))
	   (setq tmp (fsvn-xml-parse-logentry))
	   (setq ret (fsvn-logs-unique-merge ret tmp))
	   tmp)))
     urlrevs)
    (sort ret (lambda (l1 l2) (< (fsvn-xml-log->logentry.revision l1) (fsvn-xml-log->logentry.revision l2))))))



(put 'fsvn-import-commit-foreach 'lisp-indent-function 1)



(provide 'fsvn-dev)

;;; fsvn-dev.el ends here
