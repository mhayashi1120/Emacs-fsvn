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
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive)
  )



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




;;TODO

(add-hook 'fsvn-log-list-mode-hook
	  (lambda ()
	    (define-key fsvn-log-list-mode-map "l" 'fsvn-log-list-diff-local)))

(defun fsvn-logview-cmd-read-diff-local ()
  (list (fsvn-cmd-read-subcommand-args "diff" fsvn-default-args-diff)))

(defun fsvn-log-list-diff-local (local-file &optional args)
  "Diff current revision at point with LOCAL-FILE.
"
  (interactive (fsvn-logview-cmd-read-diff-local))
  (let ((file fsvn-logview-target-urlrev)
	(rev (fsvn-log-list-point-revision))
	buffer diff-args)
    (setq diff-args (list "--revision" rev file))
    (fsvn-diff-start-process diff-args args)))

(defun fsvn-log-list-cmd-read-create-patch-region ()
  (unless mark-active
    (error "Mark is not active"))
  (let* ((region (fsvn-log-list-region-revision t))
	 (patch (fsvn-read-file-name "Patch file: ")))
    (when (file-exists-p patch)
      (unless (y-or-n-p "File exists. Overwrite? ")
	(signal 'quit nil)))
    (list (car region) (cdr region) patch)))

(defun fsvn-log-list-create-patch-region (from-urlrev to-urlrev patch-file)
  "Create PATCH-FILE contains FROM-URLREV to TO-URLREV.
FROM-URLREV is marked point revision, TO-URLREV is current point revision."
  (interactive (fsvn-log-list-cmd-read-create-patch-region))
  (let ((file (fsvn-expand-file patch-file))
	proc)
    (write-region "" nil file)
    (setq proc (fsvn-start-process nil "diff" from-urlrev to-urlrev))
    (set-process-sentinel proc (lambda (proc event) 
				 (message "Patch was created.")))
    (set-process-filter proc `(lambda (proc event)
				(write-region event nil ,file t)))
    proc))



(defconst fsvn-proplist-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-proplist-mode-menu
  fsvn-proplist-mode-map
  "Menu used in Fsvn Property List mode."
  fsvn-proplist-mode-menu-spec)

(defconst fsvn-propedit-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-propedit-mode-menu
  fsvn-propedit-mode-map
  "Menu used in Fsvn Property Edit mode."
  fsvn-propedit-mode-menu-spec)

(defconst fsvn-process-list-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-process-list-mode-menu
  fsvn-process-list-mode-map
  "Menu used in Fsvn Processes mode."
  fsvn-process-list-mode-menu-spec)

(defconst fsvn-popup-result-mode-menu-spec
  '("fsvn"
     ["Kill running process" fsvn-popup-result-kill-process t]
     ["Send password" fsvn-popup-result-send-password t]
     ["Send input string to process" fsvn-popup-result-send-string t]
    ))

(easy-menu-define fsvn-popup-result-mode-menu
  fsvn-popup-result-mode-map
  "Menu used in Fsvn Result mode."
  fsvn-popup-result-mode-menu-spec)

(defconst fsvn-select-file-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-select-file-mode-menu
  fsvn-select-file-mode-map
  "Menu used in Fsvn File Select mode."
  fsvn-select-file-mode-menu-spec)

(defconst fsvn-message-edit-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-message-edit-mode-menu
  fsvn-message-edit-mode-map
  "Menu used in Fsvn Log Message Edit mode."
  fsvn-message-edit-mode-menu-spec)



;; ~/.fsvn/cache/log/{root-hash}
;; ~/.fsvn/cache/log/{root-hash}/revs
;; ~/.fsvn/cache/log/{root-hash}/revs/{1,2,3,....N}
;; ~/.fsvn/cache/log/{root-hash}/index/{path-hash}....

(defun fsvn-log-cache-create (root path entries)
  )

(defun fsvn-log-cache-search (urlrev root rev-range count)
  )

(defun fsvn-log-cache-clenup (root)
  )



(defmacro fsvn-test-buffer-has (buffer regexp)
  `(with-current-buffer ,buffer
     (save-excursion
       (goto-char (point-min))
       (unless (re-search-forward ,regexp nil t)
	 (error "Assertion failed Expected %s have not found" ,regexp)))))



(provide 'fsvn-dev)

;;; fsvn-dev.el ends here
