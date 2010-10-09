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

;;TODO
;; (defun fsvn-browse-copy-path-in-repository (to-url &optional args)
;;   "Execute `copy' for repository file corresponding current directory.
;; Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.

;; This makes faster copy than in working copy.
;; "
;;   (interactive)
;;   (fsvn-browse-copy-this-in-repository (fsvn-browse-current-repository-url) to-url args))



(defcustom fsvn-browse-guessed-moved-parent-threshold 4
  "*"
  :group 'fsvn
  :type 'integer)

(defun fsvn-browse-file-name-parent-directory (file max-level)
  (let ((tmp (fsvn-file-name-directory (directory-file-name file)))
	(i 0))
    (while (and (fsvn-directory-under-versioned-p (fsvn-file-name-directory tmp))
		(< i max-level))
      (setq tmp (fsvn-file-name-directory tmp))
      (setq i (1+ i)))
    tmp))

(defun fsvn-browse-search-guessed-moved-files (file file-versioned-p)
  (let ((dir (fsvn-browse-file-name-parent-directory file fsvn-browse-guessed-moved-parent-threshold)))
    (fsvn-mapitem
     (lambda (f)
       (let ((versioned (fsvn-meta-file-registered-p f)))
	 (cond
	  ((and file-versioned-p versioned))
	  ((and (not file-versioned-p) (null versioned)))
	  (t
	   f))))
     ;;TODO hard-coding
    (fsvn-search-same-name-files dir file (+ fsvn-browse-guessed-moved-parent-threshold 2)))))

;;TODO change electric-select-file to be able to show message
(defun fsvn-browse-cmd-read-search-move/copy-file ()
  ;; (fsvn-browse-cmd-wc-only
   (let ((target-file (fsvn-browse-cmd-this-wc-file))
	 files src-file dest-file
	 target-versioned-p alist)
     (setq target-versioned-p (fsvn-meta-file-registered-p target-file))
     (if target-versioned-p
	 (setq src-file target-file)
       (setq dest-file target-file))
     (setq files (fsvn-browse-search-guessed-moved-files target-file target-versioned-p))
     (while files
       (if target-versioned-p
	   (setq dest-file (car files))
	 (setq src-file (car files)))
       (setq alist (cons (cons src-file dest-file) alist))
       (setq files (cdr files)))
     alist))

(defun fsvn-browse-search-moved/copied-file (src-file dest-file copy-p)
  (interactive (fsvn-browse-cmd-read-search-move/copy-file))
  (fsvn-browse-wc-only
   (if copy-p
       (fsvn-browse-safe-copy-this src-file dest-file)
     (fsvn-browse-safe-move-this src-file dest-file))))



(defun fsvn-browse-cmd-read-smart-copy-this ()
  (fsvn-browse-cmd-read-smart-copy/move-this 
   (fsvn-browse-cmd-this-urlrev) t))

(defun fsvn-browse-cmd-read-smart-move-this ()
  (fsvn-browse-cmd-wc-only
   (fsvn-browse-cmd-read-smart-copy/move-this 
    (fsvn-browse-cmd-this-wc-file) nil)))

(defun fsvn-browse-smart-move-this (alist &optional args)
  "Execute `move' for point file.
If that file indicate multiple files, electric prompt these files.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-smart-move-this))
  (let ((strategies
	 (mapcar
	  (lambda (item)
	    (list 'fsvn-popup-start-copy/move-process "move" (car item) (cdr item) args))
	  alist)))
    (fsvn-async-invoke strategies)))

(defun fsvn-browse-smart-copy-this (alist &optional args)
  "Execute `copy' for point file.
If that file indicate multiple files, electric prompt these files.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-smart-copy-this))
  (let ((strategies
	 (mapcar
	  (lambda (item)
	    (list 'fsvn-popup-start-copy/move-process "copy" (car item) (cdr item) args))
	  alist)))
    (fsvn-async-invoke strategies)))

(defun fsvn-browse-cmd-read-smart-copy/move-this (from copy-p)
  (let* ((subcommand (if copy-p "copy" "move"))
	 (default-args (if copy-p 
			   fsvn-default-args-copy
			  fsvn-default-args-move))
	 (from (fsvn-browse-cmd-this-urlrev))
	 (prompt (format "%s `%s' -> " (capitalize subcommand) (fsvn-url-filename from)))
	 (to (fsvn-read-file-under-versioned prompt from))
	 (args (fsvn-cmd-read-subcommand-args subcommand default-args))
	 (alist (fsvn-smart-move/copy-file-alist from to))
	 (directory (fsvn-browse-current-directory-url))
	 (prompt (format "Select %s files. "
			 (if copy-p "copying" "moving")))
	 selected)
    (when (> (length alist) 1)
      (setq selected (fsvn-electric-select-files 
		      directory
		      (mapcar
		       (lambda (item)
			 (list (car item) 
			       t
			       (format "%s to %s" 
				       (capitalize subcommand)
				       (fsvn-url-relative-name (cdr item) directory))
			       (cdr item)))
		       alist)
		      prompt))
      (setq alist (mapcar 
		   (lambda (item) 
		     (cons (nth 0 item) (nth 3 item)))
		   selected)))
    (list alist args)))

(defun fsvn-file-name-changed-prefix (src-file dest-file)
  (let* ((src-name (fsvn-file-name-nondirectory src-file))
	 (dest-name (fsvn-file-name-nondirectory dest-file))
	 (src-list (reverse (string-to-list src-name)))
	 (dest-list (reverse (string-to-list dest-name)))
	 src-diff dest-diff same)
    (while (and src-list dest-list 
		(= (car src-list) (car dest-list)))
      (setq same (cons (car src-list) same))
      (setq src-list (cdr src-list)
	    dest-list (cdr dest-list)))
    (setq src-diff (nreverse src-list))
    (setq dest-diff (nreverse dest-list))
    ;; Match to `.'
    (if (string-match "^\\([^.]+\\)\\." (concat same))
	(let ((rest (match-string 1 (concat same))))
	  (cons (concat src-diff rest) (concat dest-diff rest)))
      (cons (concat src-diff) (concat dest-diff)))))

(defun fsvn-smart-move/copy-file-alist (src-file dest-file)
  (let ((prefix (fsvn-file-name-changed-prefix src-file dest-file))
	(src-dir (fsvn-file-name-directory src-file))
	(dest-dir (fsvn-file-name-directory dest-file))
	regexp src-files)
    ;;TODO check under versiond or not
    (setq src-files 
	  (fsvn-mapitem
	   (lambda (file)
	     (when (fsvn-meta-file-registered-p file)
	       file))
	   (directory-files src-dir t (concat "^" (regexp-quote (car prefix))))))
    (setq regexp (concat "^" (regexp-quote (car prefix)) "\\(.*\\)$"))
    (mapcar
     (lambda (src-file)
       (let ((src-name (fsvn-file-name-nondirectory src-file))
	     dest-name)
	 (unless (string-match regexp src-name)
	   (error "Assertion failed. File name is not matched"))
	 (setq dest-name (concat (cdr prefix) (match-string 1 src-name)))
	 (cons src-file (fsvn-expand-file dest-name dest-dir))))
     src-files)))

;;TODO
;; ("File At Point"
;;  ["Smart Copy" fsvn-browse-smart-copy-this t]
;;  ["Smart Move" fsvn-browse-smart-move-this t]




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



(defconst fsvn-select-file-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-select-file-mode-menu
  fsvn-select-file-mode-map
  "Menu used in Fsvn File Select mode."
  fsvn-select-file-mode-menu-spec)



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



(defun fsvn-cache-repository-directory ()
  "Repository directory."
  (fsvn-expand-file "repository" (fsvn-cache-directory)))

(defun fsvn-cache-uuid-repository (uuid)
  (let* ((repos (expand-file-name uuid (fsvn-cache-repository-directory)))
	 (url (fsvn-directory-name-as-repository repos)))
    (unless (and (file-directory-p repos)
		 (> (length (directory-files repos nil dired-re-no-dot)) 0))
      (make-directory repos t)
      ;;TODO local password???
      (fsvn-admin-call-command-discard "create" nil repos)
      (fsvn-admin-call-command-discard "setuuid" nil repos uuid)
      (fsvn-admin-create-empty-hook repos "pre-revprop-change"))
    url))

(defun fsvn-cache-enable-p ()
  (and fsvn-svnsync-command-internal
       (executable-find fsvn-svnsync-command-internal)))

(defun fsvn-cache-initialize-repository (root)
  (let* ((uuid (fsvn-get-uuid root))
	 (url (fsvn-cache-uuid-repository uuid))
	 (info (fsvn-get-info-entry url)))
    (unless (and info (> (fsvn-xml-info->entry.revision info) 0))
      ;; No costed execute. sync process.
      (with-temp-buffer
	(unless (= (call-process fsvn-svnsync-command-internal nil (current-buffer) nil "initialize" url root) 0)
	  (signal 'fsvn-command-error (cons (buffer-string) nil)))))
    url))

(defun fsvn-cache-mirror (root)
  (let* ((url (fsvn-cache-initialize-repository root))
	 (buffer (fsvn-make-temp-buffer))
	 proc)
    (fsvn-process-environment
     (setq proc (start-process "fsvn" buffer fsvn-svnsync-command-internal "synchronize" url)))
    (set-process-sentinel proc 
			  (lambda (p e)
			    (fsvn-process-exit-handler p e
			      (kill-buffer (process-buffer p)))))
    (set-process-filter proc (lambda (p e)))
    proc))

(defun fsvn-cache-start-command (subcommand buffer &rest args)
  (apply 'fsvn-start-command subcommand buffer args))



(defmacro fsvn-test-buffer-has (buffer regexp)
  `(with-current-buffer ,buffer
     (save-excursion
       (goto-char (point-min))
       (unless (re-search-forward ,regexp nil t)
	 (error "Assertion failed Expected %s have not found" ,regexp)))))




;; background gardian
;; status `preparing' `prepared' `invoking'?? `done'

(defvar fsvn-gardian-timer nil)

(defun fsvn-gardian-invoke ()
  )

(defvar fsvn-gardian-registered-alist nil)

(defun fsvn-gardian-register (name)
  (unless fsvn-gardian-timer
    (setq fsvn-gardian-timer)))

(defun fsvn-gardian-mode ()
  )

(defun fsvn-gardian-draw-list ()
  (mapc
   (lambda (item)
     )
   fsvn-gardian-registered-alist))



(provide 'fsvn-dev)

;;; fsvn-dev.el ends here
