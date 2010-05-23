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
  (let* ((old (fsvn-get-ls wcpath))
	 (new (fsvn-browse-directory-files new-source-path))
	 (old-files (mapcar 'fsvn-xml-lists->list->entry=>name$ old))
	 message-log-max)
    (mapc
     (lambda (newfile)
       (let ((nf (fsvn-expand-file newfile new-source-path))
	     (of (fsvn-expand-file newfile wcpath))
	     (oldfile (car (fsvn-file-member newfile old-files))))
	 (when (and oldfile (not (file-exists-p of)))
	   (error "Old file missing"))
	 (message "Copying %s" newfile)
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
	   (fsvn-call-command-discard "delete" of))
	 (message "Deleting %s" oldfile)))
     old-files)
    t))



(defmacro fsvn-import-commit-foreach (alist &rest form)
  "Each cell of ALIST bound to `FROM-URL' and `TO-FILE'"
  `(mapc
    (lambda (cell)
      (let ((FROM-URL (car cell))
	    (TO-FILE  (cdr cell)))
	,@form))
    ,alist))

(defun fsvn-import-commit (alist rev-from rev-to)
  "ALIST is (from-url . to-file)
REV-FROM
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
  (let ((logs (fsvn-get-files-logs (mapcar 'car alist) (cons rev-from rev-to))))
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

(defcustom fsvn-import-with-log-message-format
  "%m

Imported from %u at %r"

  "*Format string for commited messages.
%u is the url that imported from.
%r is the revision number of imported url.
%m is the original log message."
  :group 'fsvn
  :type 'string)

(defun fsvn-import-with-log-formatted-message (url log-entry)
  (let ((msg (or (fsvn-xml-log->logentry=>msg$ log-entry) ""))
	(rev (fsvn-get-revision-string (fsvn-xml-log->logentry.revision log-entry))))
    (fsvn-text-format fsvn-import-with-log-message-format
		      `(("u" . ,url)
			("r" . ,rev)
			("m" . ,msg)))))

(defun fsvn-import-with-log (src-url rev-range dest-url)
  "Overwrite DEST-URL by SRC-URL completely ignore conflict.
REV-RANGE cons cell like (from . to)
"
  (let* ((src-info (fsvn-get-info-entry src-url))
	 (src-path (fsvn-info-repos-path src-info))
	 (src-root (fsvn-xml-info->entry=>repository=>root$ src-info))
	 (src-directoryp (eq (fsvn-xml-info->entry.kind src-info) 'dir))
	 log-entries dest-wc export-file buffer)
    (message "Getting log...")
    (setq log-entries (fsvn-get-file-logs src-url rev-range))
    (message "Creating temporary working copy...")
    (if src-directoryp
	(setq dest-wc (fsvn-get-temporary-wc dest-url))
      (setq dest-wc (fsvn-get-temporary-wc (fsvn-url-dirname dest-url)))
      (setq export-file (fsvn-expand-file (fsvn-url-filename src-url) dest-wc)))
    (setq buffer (fsvn-browse-wc-noselect dest-wc))
    (with-current-buffer buffer
      (mapc
       (lambda (entry)
	 (when src-directoryp
	   ;; Export to temporary directory that has directory hierarchy completedly
	   (setq export-file (fsvn-make-temp-directory)))
	 (let* ((rev (fsvn-xml-log->logentry.revision entry))
		(path (fsvn-logs-chain-find log-entries rev src-path))
		(url (fsvn-expand-url path src-root))
		(urlrev (fsvn-url-urlrev url rev))
		(log-message (fsvn-import-with-log-formatted-message url entry))
		message)
	   (unless (string= log-message "")
	     (setq message (fsvn-make-temp-file))
	     (let ((coding-system-for-write fsvn-message-edit-file-encoding))
	       (write-region log-message nil message nil 'no-msg)))
	   (message "Exporting %s at %d..." url rev)
	   (fsvn-call-command-discard "export" urlrev "--force" export-file)
	   (when src-directoryp
	     (fsvn-browse-upgrade-source-tree export-file))
	   (fsvn-call-command-discard "commit" 
				      (if message 
					  (list "--file" message)
					(list "--message" "")))))
       log-entries))
    (kill-buffer buffer)))

(defun fsvn-merged-import-with-log (src-url rev-range dest-url)
  "Merge SRC-URL to DEST-URL.
REV-RANGE cons cell like (from . to)
"
  (let* ((src-info (fsvn-get-info-entry src-url))
	 (src-path (fsvn-info-repos-path src-info))
	 (src-root (fsvn-xml-info->entry=>repository=>root$ src-info))
	 (src-directoryp (eq (fsvn-xml-info->entry.kind src-info) 'dir))
	 log-entries dest-wc merging-file buffer conflict-info)
    (message "Getting log...")
    (setq log-entries (fsvn-get-file-logs src-url rev-range))
    (message "Creating temporary working copy...")
    (if src-directoryp
	(progn
	  (setq dest-wc (fsvn-get-temporary-wc dest-url))
	  (setq merging-file dest-wc))
      (setq dest-wc (fsvn-get-temporary-wc (fsvn-url-dirname dest-url)))
      (setq merging-file (fsvn-expand-file (fsvn-url-filename src-url) dest-wc)))
    (setq buffer (fsvn-browse-wc-noselect dest-wc))
    (catch 'conflicted
      (unwind-protect
	  (progn
	    (set-buffer buffer)
	    (mapc
	     (lambda (entry)
	       (let* ((rev (fsvn-xml-log->logentry.revision entry))
		      (path (fsvn-logs-chain-find log-entries rev src-path))
		      (url (fsvn-expand-url path src-root))
		      (urlrev (fsvn-url-urlrev url rev))
		      (log-message (fsvn-import-with-log-formatted-message url entry))
		      message status-entries)
		 (unless (string= log-message "")
		   (setq message (fsvn-make-temp-file))
		   (let ((coding-system-for-write fsvn-message-edit-file-encoding))
		     (write-region log-message nil message nil 'no-msg)))
		 (message "Merging %s at %d..." url rev)
		 (with-temp-buffer
		   (fsvn-call-command "merge" (current-buffer)  "--accept" "postpone" "-c" rev urlrev merging-file))
		 (when (fsvn-status-conflict-exists-p merging-file)
		   (setq conflict-info merging-file)
		   (throw 'conflicted t))
		 (fsvn-call-command-discard "commit" 
					    (if message 
						(list "--file" message)
					      (list "--message" "")))
		 (fsvn-call-command-discard "update")))
	     log-entries))
	(if conflict-info
	    (progn
	      (switch-to-buffer buffer)
	      (error "Resolve conflict and commit it"))
	  (kill-buffer buffer))))))

(defun fsvn-status-modified-exists-p (file)
  (let (status-entries)
    (setq status-entries
	  (if (fsvn-file-exact-directory-p file)
	      (fsvn-get-directory-status file)
	    (list (fsvn-get-file-status file))))
    (catch 'modified
      (mapc
       (lambda (entry)
	 (when (or (memq (fsvn-xml-status->target->entry=>wc-status.item entry) '(modified added))
		   (memq (fsvn-xml-status->target->entry=>wc-status.props entry) '(added)))
	   (throw 'modified t)))
       status-entries)
      nil)))

(defun fsvn-status-conflict-exists-p (file)
  (let (status-entries)
    (setq status-entries
	  (if (fsvn-file-exact-directory-p file)
	      (fsvn-get-directory-status file)
	    (list (fsvn-get-file-status file))))
    (catch 'conflicted
      (mapc
       (lambda (entry)
	 (when (or (eq (fsvn-xml-status->target->entry=>wc-status.tree-conflicted entry) t)
		   (eq (fsvn-xml-status->target->entry=>wc-status.item entry) 'conflicted)
		   (eq (fsvn-xml-status->target->entry=>wc-status.props entry) 'conflicted))
	   (throw 'conflicted t)))
       status-entries)
      nil)))

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




;; TODO similar to fsvn-get-files-logs
(defun fsvn-logs-multiple-url (urlrevs)
  "Gather non-duplicated log entries."
  (let (entries)
    (mapc
     (lambda (urlrev)
       (let ((entry (fsvn-get-file-logs urlrev)))
	 (setq entries (fsvn-logs-unique-merge entries entry))))
     urlrevs)
    (sort entries (lambda (l1 l2) (< (fsvn-xml-log->logentry.revision l1) (fsvn-xml-log->logentry.revision l2))))))



;; testing

(defconst fsvn-xml-accessor-prefix "fsvn-xml-")

(defun fsvn-xml-create-accessor (dtd paren multi-nodes)
  (let* ((base-nodes (append paren (list (car dtd))))
	 (base-name (concat fsvn-xml-accessor-prefix (fsvn-xml-create-accessor-node base-nodes multi-nodes)))
	 (attrs (fsvn-xml-node-attributes dtd))
	 (name (symbol-name (car dtd)))
	 (children (fsvn-xml-node-children dtd)))
    (list
     (mapcar
      (lambda (attr)
	(concat base-name  "." (symbol-name (car attr))))
      attrs)
     (cond
      ((atom children)
       (concat fsvn-xml-accessor-prefix (fsvn-xml-create-accessor-node paren multi-nodes) "=" name))
      (t
       (mapcar
	(lambda (child)
	  (fsvn-xml-create-accessor child base-nodes multi-nodes))
	children))))))

(defun fsvn-xml-create-accessor-node (paren multi-nodes)
  (let (seq)
    (setq seq (fsvn-xml-accessor-multi-most-match paren multi-nodes))
    (cond
     ((or (null seq)
	  (equal seq paren))
      (mapconcat 'symbol-name paren "->"))
     (t
      (mapconcat 'symbol-name seq "=>")))))

(defun fsvn-xml-accessor-multi-most-match (nodes multi-nodes)
  (let (max)
    (mapc
     (lambda (seq)
       (let ((len (length seq))
	     (i 0)
	     node)
	 (catch 'unmatch
	   (while (< i len)
	     (setq node (nth i nodes))
	     (unless (eq node (nth i seq))
	       (throw 'unmatch t))
	     (setq i (1+ i)))
	   (when (> len (length max))
	     (setq max seq)))))
     multi-nodes)
    max))



(put 'fsvn-import-commit-foreach 'lisp-indent-function 1)



(provide 'fsvn-dev)

;;; fsvn-dev.el ends here
