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
  (let* ((old-entries (fsvn-get-ls wcpath))
	 (new-files (fsvn-browse-directory-files new-source-path))
	 (old-files (mapcar 'fsvn-xml-lists->list->entry=>name$ old-entries))
	 message-log-max)
    (mapc
     (lambda (new-file)
       (let ((nf (fsvn-expand-file new-file new-source-path))
	     (of (fsvn-expand-file new-file wcpath))
	     (oldfile (car (fsvn-file-member new-file old-files))))
	 (when (and oldfile (not (file-exists-p of)))
	   (error "Old file missing"))
	 (message "Copying %s" new-file)
	 (cond
	  ((not (fsvn-file-exact-directory-p nf))
	   (copy-file nf of t t))
	  (oldfile
	   (fsvn-browse-upgrade-source-tree-internal of nf))
	  (t
	   (fsvn-copy-directory nf of t)))
	 (unless oldfile
	   (fsvn-call-command-discard "add" of))
	 ;;TODO internal fsvn-copy-directory and new-source-path
	 ;; (when (fsvn-directory-under-versioned-p new-source-path)
	 ;; 	   (fsvn-duplicate-all-properties nf of))
	 ))
     new-files)
    (mapc
     (lambda (old-file)
       (unless (fsvn-file-member old-file new-files)
	 (let ((of (fsvn-expand-file old-file wcpath)))
	   (fsvn-call-command-discard "delete" of))
	 (message "Deleting %s" old-file)))
     old-files)
    t))



(defcustom fsvn-import-with-log-message-format
  "%m

Imported from %u at %r"

  "*Format string for commited messages.
%u is the url that imported from (Non user and password).
%r is the revision number of imported url.
%m is the original log message."
  :group 'fsvn
  :type 'string)

(defun fsvn-import-with-log-formatted-message (url log-entry)
  (let ((msg (or (fsvn-xml-log->logentry=>msg$ log-entry) ""))
	(rev (fsvn-get-revision-string (fsvn-xml-log->logentry.revision log-entry))))
    (fsvn-text-format fsvn-import-with-log-message-format
		      `(("u" . ,(fsvn-url-remove-authority url))
			("r" . ,rev)
			("m" . ,msg)))))

(defun fsvn-overwrite-import-with-log (src-url rev-range dest-url)
  "Overwrite DEST-URL by SRC-URL completely ignore conflict.
REV-RANGE cons cell like (from . to)
"
  (let* ((src-info (fsvn-get-info-entry src-url))
	 (src-path (fsvn-info-repos-path src-info))
	 (src-root (fsvn-xml-info->entry=>repository=>root$ src-info))
	 (src-directoryp (eq (fsvn-xml-info->entry.kind src-info) 'dir))
	 (popup-buffer (fsvn-popup-result-create-buffer))
	 log-entries dest-wc export-file buffer)
    (message "Getting log...")
    (setq log-entries (fsvn-get-file-logs src-url rev-range))
    (message "Creating temporary working copy...")
    (if src-directoryp
	(setq dest-wc (fsvn-get-temporary-wc dest-url))
      (setq dest-wc (fsvn-get-temporary-wc (fsvn-url-dirname dest-url)))
      (setq export-file (fsvn-expand-file (fsvn-url-filename src-url) dest-wc)))
    (fsvn-buffer-popup-as-information popup-buffer)
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
	   (fsvn-call-command-display "commit" popup-buffer
				      (if message 
					  (list "--file" message)
					(list "--message" "")))))
       log-entries))
    (kill-buffer buffer)))

(defun fsvn-merged-import-with-log (src-url rev-range dest-url)
  "Merge SRC-URL to DEST-URL.
REV-RANGE cons cell like (from . to)

Intent to mirror SRC-URL and DEST-URL with commit log (Only log message).
If ignore all conflict (DEST-URL subordinate to SRC-URL), use `fsvn-overwrite-import-with-log'
"
  (let* ((src-info (fsvn-get-info-entry src-url))
	 (src-path (fsvn-info-repos-path src-info))
	 (src-root (fsvn-xml-info->entry=>repository=>root$ src-info))
	 (src-directoryp (eq (fsvn-xml-info->entry.kind src-info) 'dir))
	 (popup-buffer (fsvn-popup-result-create-buffer))
	 log-entries dest-wc merging-file buffer conflict-urlrev)
    (message "Getting log...")
    (setq log-entries (fsvn-get-file-logs src-url rev-range))
    (message "Creating temporary working copy...")
    (if src-directoryp
	(progn
	  (setq dest-wc (fsvn-get-temporary-wc dest-url t))
	  (setq merging-file dest-wc))
      (setq dest-wc (fsvn-get-temporary-wc (fsvn-url-dirname dest-url)))
      (setq merging-file (fsvn-expand-file (fsvn-url-filename src-url) dest-wc)))
    (setq buffer (fsvn-browse-wc-noselect dest-wc))
    (fsvn-buffer-popup-as-information popup-buffer)
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
		      message status-entries add-files)
		 (unless (string= log-message "")
		   (setq message (fsvn-make-temp-file))
		   (let ((coding-system-for-write fsvn-message-edit-file-encoding))
		     (write-region log-message nil message nil 'no-msg)))
		 (message "Merging %s at %d..." url rev)
		 (cond
		  (src-directoryp
		   (setq add-files (fsvn-merged-import-export-non-tree-files src-root src-url entry))
		   (fsvn-call-command-display "merge" popup-buffer "--accept" "postpone" "-c" rev urlrev merging-file))
		  ((file-exists-p merging-file)
		   (fsvn-call-command-display "merge" popup-buffer "--accept" "postpone" "-c" rev urlrev merging-file))
		  (t
		   (fsvn-call-command-discard "export" "--force" urlrev merging-file)
		   (fsvn-call-command-display "add" popup-buffer merging-file)))
		 (when (fsvn-status-conflict-exists-p merging-file)
		   (setq conflict-urlrev urlrev)
		   (throw 'conflicted t))
		 (mapc
		  (lambda (file)
		    (fsvn-call-command-display "add" popup-buffer file))
		  add-files)
		 (fsvn-call-command-display "commit" 
					    popup-buffer
					    (if message 
						(list "--file" message)
					      (list "--message" "")))
		 (fsvn-call-command-discard "update")))
	     log-entries))
	(if conflict-urlrev
	    (progn
	      (switch-to-buffer buffer)
	      (error "Conflicted when merging %s. Resolve commit it" conflict-urlrev))
	  (kill-buffer buffer))))))

(defun fsvn-merged-import-export-non-tree-files (src-root src-url entry)
  (let ((rev (fsvn-xml-log->logentry.revision entry))
	add-files)
    (mapc
     (lambda (path-entry)
       (let* ((path (fsvn-xml-log->logentry->paths->path$ path-entry))
	      (url (fsvn-expand-url path src-root))
	      (urlrev (fsvn-url-urlrev url rev))
	      relative-name file)
	 (when (fsvn-url-descendant-p src-url url)
	   (setq relative-name (fsvn-url-relative-name src-url url))
	   (setq file (fsvn-expand-file relative-name))
	   (unless (file-exists-p file)
	     (unless (file-directory-p (file-name-directory file))
	       (make-directory (file-name-directory file) t))
	     (fsvn-call-command-discard "export" urlrev file)
	     ;; not commited file exists `merge' simply ignore the file.
	     ;; if add this point, sometime `merge' failed.
	     (setq add-files (cons file add-files))))))
     (fsvn-xml-log->logentry->paths entry))
    add-files))

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



(defcustom fsvn-browse-guessed-moved-parent-threshold 4
  "*"
  :group 'fsvn
  :type 'integer)

(defun fsvn-browse-search-guessed-moved-files (file file-versioned-p)
  (let ((dir (fsvn-file-name-parent-directory file fsvn-browse-guessed-moved-parent-threshold)))
    (fsvn-mapitem
     (lambda (f)
       (let ((versioned (fsvn-get-ls f)))
	 (cond
	  ((and file-versioned-p versioned))
	  ((and (not file-versioned-p) (null versioned)))
	  (t
	   f))))
     ;;TODO hard-coding
    (fsvn-search-same-name-files dir file 6))))

(defun fsvn-browse-search-moved/copied-file (target-file)
  (interactive (fsvn-browse-cmd-read-wc-this-file))
  (fsvn-browse-wc-only
   (let (files
	 src-file dest-file file 
	 file-versioned target-versioned done)
     (setq target-versioned (fsvn-get-ls target-file))
     (if target-versioned
	 (setq src-file target-file)
       (setq dest-file target-file))
     (setq files (fsvn-browse-search-guessed-moved-files target-file target-versioned))
     (while files
       (setq file (car files))
       (setq file-versioned (fsvn-get-ls file))
       (if target-versioned
	   (setq dest-file file)
	 (setq src-file file))
       ;;todo interactive command
       ;;todo electric
       (cond
	((y-or-n-p (format "Move %s to %s? " src-file dest-file))
	 (fsvn-browse-safe-move-this src-file dest-file)
	 (setq done t)
	 (setq files nil))
	((y-or-n-p (format "Copy %s to %s? " src-file dest-file))
	 (fsvn-browse-safe-copy-this src-file dest-file)
	 (setq done t)
	 (setq files nil)))
       (setq files (cdr files)))
     (unless done
       (message "Cannot do anything.")))))



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



(add-hook 'fsvn-log-list-mode-hook
	  (lambda ()
	    (define-key fsvn-log-list-mode-map "\C-c\C-t" 'fsvn-log-list-revert-to-revision)))

(defun fsvn-log-list-revert-to-revision (urlrev local-file)
  (interactive (fsvn-log-list-cmd-read-revert-to-revision))
  (fsvn-async-let ((urlrev urlrev)
		   (local-file local-file))
    (fsvn-popup-start-process "delete" (list local-file))
    (fsvn-popup-start-copy/move-process "copy" (list urlrev) local-file)))



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



(defun fsvn-browse-cmd-read-copy-this-properties ()
  (fsvn-browse-cmd-wc-only
   (let ((src-file)
	 (dest-file (fsvn-browse-point-local-filename))
	 (arg current-prefix-arg))
     (unless dest-file
       (error "No file on this line"))
     (setq src-file (fsvn-read-file-name "Properties copy from: " nil nil t))
     (list src-file dest-file arg))))

;;TODO full
(defun fsvn-browse-copy-this-properties (src-file dest-file &optional full)
  "Copy SRC-FILE properties to DEST-FILE.
FULL non-nil means DEST-FILE will have exactly same properties of SRC-FILE."
  (interactive (fsvn-browse-cmd-read-copy-this-properties))
  (let ((alist (fsvn-get-prop-value-alist src-file)))
    (mapc
     (lambda (key-value)
       (let ((prop (car key-value))
	     (val (cdr key-value)))
	 (fsvn-set-prop-value dest-file prop val)))
     alist)
    (fsvn-browse-redraw-wc-file-entry dest-file)))



(defvar fsvn-bugreport-mail-address "mhayashi1120@gmail.com")

;;TODO
(defvar fsvn-bugreport-salutation
  "
# If you are a Japanese, please write in Japanese:-)

Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of fsvn*.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"fsvn.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
")

(defun fsvn-bugreport ()
  (interactive)
  (let ((pkgname "fsvn.el"))
    (reporter-submit-bug-report
     fsvn-maintainer-mail-address
     pkgname
     (apropos-internal "^fsvn-" 'boundp)
     nil nil fsvn-bugreport-salutation)
    (mail-position-on-field "subject")
    (insert pkgname "; Bug report" )
    (unless (y-or-n-p 
	     (concat 
	      "This bug report may contain privacy information (Like password).\n"
	      "Please delete manually. OK? " ))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))




(require 'easymenu)

(defconst fsvn-browse-mode-menu-spec
  '("fsvn"
    ["Update"     fsvn-browse-update-path       t]
    ["Commit"     fsvn-browse-commit-path       t]
    ["Log"        fsvn-browse-logview-path      t]
    ["Diff"       fsvn-browse-diff-this         t]
    "----"
    ("Manipulate Current Directory"
     ["Commit"     fsvn-browse-commit-path       t]
     ["Export"     fsvn-browse-export-path       t]
     ["Info"       fsvn-browse-info-path         t]
     ["Log"        fsvn-browse-logview-path      t]
     ["Merge"      fsvn-browse-merge-path        t]
     ["Mkdir"      fsvn-browse-mkdir             t]
     ["Proplist"   fsvn-browse-propview-path     t]
     ["Revert"     fsvn-browse-revert-path       t]
     ["Update"     fsvn-browse-update-path       t]
     ["Switch"     fsvn-browse-switch-path       t]
     ["Cleanup"     fsvn-browse-cleanup-path       t]
     )
    ("Repository Browser"
     ["Magic"      fsvn-browse-magic-head            t]
     ["Browser"    fsvn-browse-open-repository       t]
     ["Mergeinfo"  fsvn-browse-mergeinfo-path t]
     ["Branch"  fsvn-browse-create-branch t]
     ["Tag"  fsvn-browse-create-tag t]
     )
    ("Manipulate Files"
     ["Delete selected"   fsvn-browse-delete-selected   t]
     ["Add selected"      fsvn-browse-add-selected      t]
     ["Revert selected"   fsvn-browse-revert-selected   t]
     ["Copy selected"     fsvn-browse-copy-selected     t]
     ["Move selected"     fsvn-browse-move-selected     t]
     ["Lock selected"     fsvn-browse-lock-selected     t]
     ["Unlock selected"   fsvn-browse-unlock-selected     t]
     ["Update selected"   fsvn-browse-update-selected   t]
     ["Add to changelist" fsvn-browse-add-changelist-selected t]
     ["Remove from changelist" fsvn-browse-remove-changelist-selected t]
     ["Commit selected"   fsvn-browse-commit-selected t]
     ["Info selected"     fsvn-browse-info-selected t]
     ["Externals" fsvn-browse-svn:externals-selected t]
     ["Resolved selected" fsvn-browse-resolved-selected t]
     ["Resolve selected" fsvn-browse-resolve-selected t]
     ["Toggle svn:needs-lock selected" fsvn-browse-prop-toggle-svn:needs-lock-selected t]
     ["Toggle svn:ignore selected" fsvn-browse-prop-add-svn:ignore-selected t]
     )
    ("Manipulate A File"
     ["Proplist of point" fsvn-browse-propview-this     t]
     ["Merge to point"    fsvn-browse-merge-this        t]
     ["Mergeinfo of point"    fsvn-browse-mergeinfo-this        t]
     ["EDiff"             fsvn-browse-ediff-this        t]
     ["Diff"              fsvn-browse-diff-this         t]
     ["Blame/Annotate"    fsvn-browse-blame-this  t]
     ["Export" fsvn-browse-export-this t]
     ["Log" fsvn-browse-logview-this t]
     ["Copy" fsvn-browse-copy-this t]
     ["Move" fsvn-browse-move-this t]
     ["Copy in repository" fsvn-browse-copy-this-in-repository t]
     ["Safe Copy" fsvn-browse-safe-copy-this t]
     ["Safe Move" fsvn-browse-safe-move-this t]
     ["TODO" fsvn-browse-rename-case-missing-file t]
     )
    "----"
    ("Emmulate Dired"
     ["Mark Regexp"        fsvn-browse-mark-file-regexp    t]
     ["Mark Delete Regexp" fsvn-browse-mark-delete-regexp  t]
     ["Unmark All"         fsvn-browse-mark-all-unmark     t]
     ["Unmark File"        fsvn-browse-mark-file-unmark     t]
     ["Mark Delete"        fsvn-browse-mark-file-delete     t]
     ["Mark"        fsvn-browse-mark-file-mark     t]
     ["Diff"               fsvn-browse-diff-local          t]
     ["Next"               fsvn-browse-next-file          t]
     ["Previous"               fsvn-browse-previous-file          t]
     ["Sort"               fsvn-browse-toggle-sort          t]
     ["View"               fsvn-browse-file-this          t]
     ["Up"               fsvn-browse-up-directory          t]
     )
    ))

(easy-menu-define fsvn-browse-mode-menu
  fsvn-browse-mode-map
  "Menu used in Browse mode."
  fsvn-browse-mode-menu-spec)



(provide 'fsvn-dev)

;;; fsvn-dev.el ends here
