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



(defun fsvn-recursive-status-sort-directories (topdir)
  (sort
   (fsvn-mapitem
    (lambda (dir)
      (when (fsvn-url-contains-p topdir dir)
	dir))
    (fsvn-browse-directories))
   (lambda (d1 d2)
     (> (length d1) (length d2)))))

(defun fsvn-recursive-status-sentinel2 (proc event)
  (fsvn-process-exit-handler proc event
    (let ((parsed (nreverse fsvn-recursive-status-parsed))
	  directories topdir info)
      (setq topdir (process-get proc 'fsvn-recursive-status-top-directory))
      (setq directories (fsvn-recursive-status-sort-directories topdir))
      (setq info (fsvn-recursive-status-join-info directories parsed))
      (fsvn-recursive-status-draw-browsing2 topdir info))
    (fsvn-recursive-status-unset-subordinate-process proc)
    (kill-buffer (current-buffer))))

(defun fsvn-recursive-status-join-info (directories parsed-info)
  (mapcar
   (lambda (dir)
     (let (files dirs)
       (mapc
	(lambda (status-info)
	  (let ((file (nth 0 status-info))
		(status (nth 1 status-info)))
	    (when (fsvn-url-grand-child-p dir file)
	      (let ((d (fsvn-url-only-child dir file))
		    cell)
		(unless (setq cell (assoc d dirs))
		  (setq cell (cons d nil))
		  (setq dirs (cons cell dirs)))
		(setcdr cell (fsvn-status-dir-status-stronger  
			      (fsvn-status-string-to-dir-status status)
			      (cdr cell)))))
	    (when (fsvn-url-child-p dir file)
	      (setq files (cons (cons file status) files)))))
	parsed-info)
       (cons dir (list (cons 'files files) (cons 'dirs dirs)))))
   directories))

(defun fsvn-recursive-status-draw-browsing2 (topdir status-alist)
  (mapc
   (lambda (dir-status)
     (let ((dir (car dir-status))
	   (files-status (cdr (assq 'files dir-status)))
	   (dirs-status (cdr (assq 'dirs dir-status))))
       (fsvn-save-browse-directory-excursion dir
	 (let (buffer-read-only)
	   ;; set to file status
	   ;; --show-updates `status' vs recursive `status'
	   (unless (fsvn-file= topdir dir)
	     (mapc
	      (lambda (status-cell)
		(let ((file (car status-cell))
		      (status (cdr status-cell)))
		  (when (fsvn-browse-goto-file file)
		    (setq status (fsvn-browse-status-string-to-display-status status))
		    (fsvn-browse-draw-status-string-this-line status))))
	      files-status))
	   ;; initialize directory status column
	   (fsvn-browse-each-file file dir
	     (when (fsvn-browse-point-directory-p)
	       (fsvn-browse-draw-dir-status-this-line)))
	   ;; set to directory column
	   (mapc
	    (lambda (status-cell)
	      (let ((file (car status-cell))
		    (status (cdr status-cell)))
		(when (fsvn-browse-goto-file file)
		  (fsvn-browse-draw-dir-status-this-line status))))
	    dirs-status)
	   (setq buffer-undo-list nil)
	   (set-buffer-modified-p nil)))))
   status-alist))

(defun fsvn-recursive-status-filter2 (proc event)
  (with-current-buffer (process-buffer proc)
    (let ((start (point))
	  file status)
      (insert event)
      (goto-char start)
      (while (re-search-forward "^\\([^?][A-Z+!?~ ]\\{6,7\\}\\) \\([^ ].+\\)\n" nil t)
	(setq status (match-string 1)
	      file (match-string 2))
	(setq fsvn-recursive-status-parsed 
	      (cons (list (fsvn-expand-file file) status) fsvn-recursive-status-parsed))))))

(defun fsvn-run-recursive-status2 (directory)
  "Execute recursive `status', and set subordinate directory."
  (let (proc buffer)
    (setq proc (fsvn-recursive-status-running-process directory))
    (if proc
	(when (eq major-mode 'fsvn-browse-mode)
	  (setq fsvn-browse-buffer-directories-status-process proc))
      (setq buffer (fsvn-make-temp-buffer))
      (with-current-buffer buffer
	(make-variable-buffer-local 'fsvn-recursive-status-parsed))
      (setq proc (fsvn-start-command "status" buffer directory))
      (set-process-sentinel proc 'fsvn-recursive-status-sentinel2)
      (set-process-filter proc 'fsvn-recursive-status-filter2)
      (process-put proc 'fsvn-recursive-status-top-directory directory)
      (fsvn-recursive-status-set-subordinate-process directory proc)
      proc)))



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

;; TODO popup-buffer
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
	 ;; 	 (popup-buffer (fsvn-popup-result-create-buffer))
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
		   (fsvn-call-command-discard "merge" "--accept" "postpone" "-c" rev urlrev merging-file))
		  ((file-exists-p merging-file)
		   (fsvn-call-command-discard "merge" "--accept" "postpone" "-c" rev urlrev merging-file))
		  (t
		   (fsvn-call-command-discard "export" "--force" urlrev merging-file)
		   (fsvn-call-command-discard "add" merging-file)))
		 (when (fsvn-status-conflict-exists-p merging-file)
		   (setq conflict-urlrev urlrev)
		   (throw 'conflicted t))
		 (mapc
		  (lambda (file)
		    (fsvn-call-command-discard "add" file))
		  add-files)
		 (fsvn-call-command-discard "commit" 
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



(defconst fsvn-process-control-re-mark "^[^ \n]")

(defconst fsvn-process-control-buffer-local-variables
  '(
    (fsvn-process-control-process-alist . fsvn-process-control-process-alist)
    (fsvn-process-control-showing-process)
    (fsvn-process-control-processes . fsvn-process-control-processes)
    (fsvn-process-control-timer)
    (post-command-hook . '(fsvn-process-control-after-move))
    (font-lock-defaults . '(fsvn-process-control-font-lock-keywords t nil nil beginning-of-line))
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

       (list (concat "^" (car (car fsvn-process-control-column-alist))) 
	     '(".+" (forward-line 0) nil (0 fsvn-header-key-face)))

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
  (fsvn-browse-setup-mode-line)
  (font-lock-mode 1)
  (font-lock-fontify-buffer))

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
	   ;;TODO 
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

(defun fsvn-process-gather-suspicious-freezing ()
  (let (alist ret)
    (mapc
     (lambda (p)
       (when (eq (process-status p) 'run)
	 (when (string-match "^fsvn" (process-name p))
	   (setq alist (cons (cons p (buffer-size (process-buffer p))) alist)))))
     (process-list))
    (sit-for 1) ;;TODO
    (mapc
     (lambda (c)
       (let ((p (car c))
	     (size (cdr c)))
	 (when (= (buffer-size (process-buffer p)) size)
	   (setq ret (cons p ret)))))
     alist)
    ret))



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

(global-set-key "\C-xvp" 'fsvn-process-control)
(put 'fsvn-process-control-retrieve-mark 'lisp-indent-function 1)



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




(provide 'fsvn-dev)

;;; fsvn-dev.el ends here
