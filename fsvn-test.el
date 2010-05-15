;;; fsvn-test.el --- test for fsvn.el

;;; Commentary:
;; 

;; TODO see testcover.el


;;; History:
;; 

;;; Code:
;;



(require 'fsvn)



(defmacro fsvn-test-equal (executed-form expected-value)
  "Assertion for EXECUTED-FORM and EXPECTED-VALUE is equal."
  `(let ((RET ,executed-form))
     (unless (equal RET ,expected-value)
       (error "Assertion failed. Expected `%s' but `%s'" ,expected-value RET))
     RET))

(defmacro fsvn-test-nil (executed-form)
  `(let ((RET ,executed-form))
     (when RET
       (error "Assertion failed. Expected nil but `%s'" RET))
     RET))

(defmacro fsvn-test-non-nil (executed-form)
  `(let ((RET ,executed-form))
     (unless RET
       (error "Assertion failed Expected non-nil but `nil'"))
     RET))

(defvar fsvn-test-async-proc nil)
;;FIXME or some utility?
(defmacro fsvn-test-async (&rest form)
  `(let (RET)
     (setq fsvn-test-async-proc nil)
     (mapc
      (lambda (exec-form)
	(let ((tmp-ret (eval exec-form))
	      (tmp-sentinel))
	  (when (and (processp tmp-ret)
		     (not (eq (process-status tmp-ret) 'exit)))
	    (setq fsvn-test-async-proc tmp-ret)
	    (setq tmp-sentinel (process-sentinel tmp-ret))
	    (if (null tmp-sentinel)
		(set-process-sentinel tmp-ret 
				      (lambda (proc event)
					(when (eq (process-status proc) 'exit)
					  (setq fsvn-test-async-proc nil))))
	      (set-process-sentinel
	       tmp-ret
	       `(lambda (proc event) 
		  (funcall ',tmp-sentinel proc event)
		  (when (eq (process-status proc) 'exit)
		    (setq fsvn-test-async-proc nil))))))
	  (setq RET tmp-ret)
	  ;; wait until process exit.
	  (while fsvn-test-async-proc
	    (sit-for 0.5))))
      ',form)
     RET))

(defmacro fsvn-test-excursion (&rest form)
  `(let ((PREV-BUFFER-LIST (buffer-list))
	 (PREV-BUFFER (current-buffer))
	 (PREV-WIN-CONFIG (current-window-configuration)))
     (unwind-protect 
	 (progn ,@form)
       (mapc 
	(lambda (b)
	  (unless (memq b PREV-BUFFER-LIST)
	    (let ((process (get-buffer-process b)))
	      (when (or (null process) (eq (process-status process) 'exit))
		(kill-buffer b)))))
	(buffer-list))
       (switch-to-buffer PREV-BUFFER)
       (set-window-configuration PREV-WIN-CONFIG))))

(defun fsvn-test-switch-file (wc1 wc2 file)
  (let ((r (file-relative-name file wc1)))
    (expand-file-name r wc2)))

(defvar fsvn-test-check-through-all nil
  "This is testing.")
(defvar fsvn-test-done-functions nil)
(defvar fsvn-test-target-functions nil)
(defun fsvn-test-gather-functions ()
  (let ((targetp (lambda (s) 
		   (and (string-match "^fsvn-" (symbol-name s)) 
			(not (string-match "^fsvn-test-" (symbol-name s)))
			(functionp s)
			(listp (symbol-function s))))))
    (mapatoms
     (lambda (s)
       (when (funcall targetp s)
	 (fmakunbound s)))
     obarray)
    (load "fsvn.el")
    (let (ret)
      (mapatoms
       (lambda (s)
	 (when (funcall targetp s)
	   (setq ret (cons s ret))))
       obarray)
      (sort ret 'string-lessp))))

(defun fsvn-test-gather-function-add-checker ()
  (setq fsvn-test-target-functions (fsvn-test-gather-functions))
  (mapcar
   (lambda (s)
     (let ((func (symbol-function s))
	   substance checker)
       (setq substance 
	     (cond
	      ((commandp s)
	       (memq (assq 'interactive func) func))
	      ((stringp (nth 2 (symbol-function s)))
	       (nthcdr 2 (symbol-function s)))
	      (t
	       (nthcdr 1 (symbol-function s)))))
       (setq checker
	     `(unless (memq ',s fsvn-test-done-functions)
		(setq fsvn-test-done-functions 
		      (cons ',s fsvn-test-done-functions))))
       (unless (member checker substance)
	 (setcdr substance 
		 (cons checker
		       (cdr substance))))))
   fsvn-test-target-functions)
  (message "%d items adviced." (length fsvn-test-target-functions)))

(defun fsvn-test-all-command-list ()
  (let ((ret))
    (mapatoms
     (lambda (s)
       (when (and (string-match "^fsvn" (symbol-name s))
		  (commandp s))
	 (setq ret (cons s ret))))
     obarray)
    (sort ret 'string-lessp)))

(defun fsvn-test-not-yet-tested-command-list ()
  (let ((all (fsvn-test-all-command-list))
	ret)
    (save-window-excursion
      (find-file (locate-library "fsvn-test"))
      (mapc
       (lambda (s)
	 (goto-char (point-min))
	 (unless (re-search-forward (format "\\_<%s\\_>" s) nil t)
	   (setq ret (cons s ret))))
       all)
      (nreverse ret))))

(defun fsvn-test-sit-for (&optional time)
  (sit-for (or time 0.0)))

(defun fsvn-test-wait-for-all-process ()
  (while (catch 'wait
	   (mapc
	    (lambda (p)
	      (unless (memq p fsvn-test-start-process-list)
		(throw 'wait t)))
	    (process-list))
	   nil)
    (sit-for 5)))

(when (require 'testcover nil t)
  (testcover-start "fsvn-browse"))

(defvar fsvn-test-start-process-list nil)
(setq fsvn-test-start-process-list (process-list))

(when fsvn-test-check-through-all
  (fsvn-test-gather-function-add-checker))

(fsvn-test-equal (fsvn-flatten-command-args '("a" 1 nil ("b" 2))) '("a" "1" "b" "2"))

(fsvn-test-equal (fsvn-expand-url "c" "http://a/b") "http://a/b/c")
(fsvn-test-equal (fsvn-expand-url "c" "http://a/b/..") "http://a/c")
(fsvn-test-equal (fsvn-expand-url "c" "http://a/b/./") "http://a/b/c")
(fsvn-test-equal (fsvn-expand-url "c" "http://a/b/../../") "http://c")
(fsvn-test-equal (fsvn-expand-url "c" "http://a/../b/../") "http://c")
(fsvn-test-equal (fsvn-expand-url "http://a/b/../") "http://a")
(fsvn-test-equal (fsvn-expand-url "http://a/b/") "http://a/b")
(fsvn-test-equal (fsvn-expand-url "http://a/b") "http://a/b")

;;todo
;;(fsvn-test-equal (fsvn-expand-url "c" "http://") "http://c")

(fsvn-test-equal (fsvn-urlrev-filename "http://a/foo.el") "foo.el")
(fsvn-test-equal (fsvn-urlrev-filename (fsvn-url-urlrev "http://a/foo.el" "HEAD")) "foo.el")
(fsvn-test-equal (fsvn-urlrev-filename "http://user@a/foo.el") "foo.el")
(fsvn-test-equal (fsvn-urlrev-filename (fsvn-url-urlrev "http://user@a/foo.el" 500)) "foo.el")


;; fsvn-url-ediff-filename

(fsvn-test-equal (fsvn-url-dirname "http://a/b") "http://a/")
(fsvn-test-equal (fsvn-url-dirname "http://a/b/") "http://a/")
(fsvn-test-equal (fsvn-url-dirname "c:/a/b") "c:/a/")
(fsvn-test-equal (fsvn-url-dirname "c:/a/b/") "c:/a/")
(fsvn-test-equal (fsvn-url-dirname "/a/b") "/a/")
(fsvn-test-equal (fsvn-url-dirname "/a/b/") "/a/")

(fsvn-test-equal (fsvn-urlrev-dirname "http://a/b") "http://a/")
(fsvn-test-equal (fsvn-urlrev-dirname "http://a/b/") "http://a/")
(fsvn-test-equal (fsvn-urlrev-dirname "c:/a/b") "c:/a/")
(fsvn-test-equal (fsvn-urlrev-dirname "c:/a/b/") "c:/a/")
(fsvn-test-equal (fsvn-urlrev-dirname "/a/b") "/a/")
(fsvn-test-equal (fsvn-urlrev-dirname "/a/b/") "/a/")
(fsvn-test-equal (fsvn-urlrev-dirname (fsvn-url-urlrev "http://a/b" 5)) "http://a/@5")
(fsvn-test-equal (fsvn-urlrev-dirname (fsvn-url-urlrev "http://a/b/" 5)) "http://a/@5")
(fsvn-test-equal (fsvn-urlrev-dirname (fsvn-url-urlrev "c:/a/b" 5)) "c:/a/@5")
(fsvn-test-equal (fsvn-urlrev-dirname (fsvn-url-urlrev "c:/a/b/" 5)) "c:/a/@5")
(fsvn-test-equal (fsvn-urlrev-dirname (fsvn-url-urlrev "/a/b" 5)) "/a/@5")
(fsvn-test-equal (fsvn-urlrev-dirname (fsvn-url-urlrev "/a/b/" 5)) "/a/@5")

(fsvn-test-equal (fsvn-urlrev-filename (fsvn-url-urlrev "http://a/b/" 5)) "b")
(fsvn-test-equal (fsvn-urlrev-filename (fsvn-url-urlrev "http://a/b" 5)) "b")

(fsvn-test-equal (fsvn-urlrev-revision (fsvn-url-urlrev "/a/b/" 5)) "5")
(fsvn-test-equal (fsvn-urlrev-revision (fsvn-url-urlrev "/a/b" 5)) "5")
(fsvn-test-equal (fsvn-urlrev-revision (fsvn-url-urlrev "/a/b/" 5)) "5")
(fsvn-test-equal (fsvn-urlrev-revision "/a/b/") nil)

(fsvn-test-non-nil (fsvn-url-repository-p "http://a/b/c"))
(fsvn-test-non-nil (fsvn-url-repository-p "http://"))
(fsvn-test-nil (fsvn-url-repository-p "http:/"))
(fsvn-test-nil (fsvn-url-repository-p "http:"))
(fsvn-test-nil (fsvn-url-repository-p "/a/b/c"))
(fsvn-test-nil (fsvn-url-repository-p ""))
(fsvn-test-nil (fsvn-url-repository-p "c:/a/b"))

(fsvn-test-equal (fsvn-url-relative-name "http://a/b" "http://a/b/c") "c")
(fsvn-test-equal (fsvn-url-relative-name "http://a/b" "http://a/b/c/") "c/")

(fsvn-test-equal (fsvn-magic-create-name "http://a/b/c") "/fsvn@HEAD/http/a/b/c")
(fsvn-test-equal (fsvn-magic-create-name "file:///a") "/fsvn@HEAD/file/a")
(fsvn-test-equal (fsvn-magic-create-name "/a/b") "/a/b")
(fsvn-test-equal (fsvn-magic-create-name "/a/b/") "/a/b/")

(fsvn-test-equal (fsvn-magic-parse-file-name "/fsvn@10/http/a") "http://a@10")
(fsvn-test-equal (fsvn-magic-parse-file-name "/fsvn@20/file/a") "file:///a@20")
(fsvn-test-equal (fsvn-magic-parse-file-name "/fsvn@30/svn/a/") "svn://a/@30")

(fsvn-test-equal (file-name-nondirectory "/fsvn@10/") "")
(fsvn-test-equal (file-name-nondirectory "/fsvn@10/a") "a")
(fsvn-test-equal (file-name-nondirectory "/fsvn@10/a/") "")

;;todo
;; (fsvn-complete-reading-split-arguments "a 	b c \"abcd")
;; (fsvn-complete-reading-split-arguments " a 	b c 'abcd' 	")
;; (fsvn-complete-reading-split-arguments " a")

(let ((url (fsvn-url-urlrev "http://a/b/c" 10))) (fsvn-test-equal (fsvn-magic-parse-file-name (fsvn-magic-create-name url)) url))
(if (memq system-type '(windows-nt))
    (let ((url "c:/a/b/c")) (fsvn-test-equal (fsvn-magic-parse-file-name (fsvn-magic-create-name url)) url))
  (let ((url "/a/b/c")) (fsvn-test-equal (fsvn-magic-parse-file-name (fsvn-magic-create-name url)) url)))

(fsvn-test-equal (fsvn-url-as-directory "http://a/b/c") "http://a/b/c/")
(fsvn-test-equal (fsvn-url-as-directory "http://a/b/c/") "http://a/b/c/")

;; fsvn-file-name-root-p
;;fsvn-file-name-directory
;; fsvn-file-name-nondirectory
;; fsvn-expand-file
;; fsvn-file-relative
;; fsvn-url-repository-p
;; fsvn-url-local-p

(fsvn-test-non-nil (fsvn-url-belongings-p "http://a" "http://a/b"))
(fsvn-test-non-nil (fsvn-url-belongings-p "http://a" "http://a/"))
(fsvn-test-non-nil (fsvn-url-belongings-p "http://a/" "http://a/"))
(fsvn-test-nil (fsvn-url-belongings-p "http://a/b" "http://a/"))

;; non interactive test
(fsvn-test-excursion
 (let* ((test-dir (fsvn-make-temp-directory))
	(repos-dir (expand-file-name "rep" test-dir))
	(repos-url (fsvn-file-name-as-repository repos-dir))
	(wc1-dir (expand-file-name "wc1" test-dir))
	(wc2-dir (expand-file-name "wc2" test-dir))
	(wc3-dir (expand-file-name "wc3" test-dir))
	(trash-dir (expand-file-name "trash" test-dir))
	(default-directory test-dir)
	;; cancel all configuration
	(fsvn-repository-alist nil)
	(fsvn-config-browse-show-update (default-value 'fsvn-config-browse-show-update))
	(fsvn-config-commit-default-file-select-p (default-value 'fsvn-config-commit-default-file-select-p))
	(fsvn-config-log-empty-warnings (default-value 'fsvn-config-log-empty-warnings))
	(fsvn-config-log-limit-count (default-value 'fsvn-config-log-limit-count))
	(fsvn-config-magic-remote-commit-message (default-value 'fsvn-config-magic-remote-commit-message))
	(fsvn-config-repository-default-coding-system (default-value 'fsvn-config-repository-default-coding-system))
	(fsvn-config-tortoise-property-use (default-value 'fsvn-config-tortoise-property-use))
	list file1 file2 file3 file4 dir1 dir2 dir3
	ignore-file)
   (make-directory wc1-dir)
   (make-directory wc2-dir)
   (make-directory wc3-dir)
   (make-directory trash-dir)
   (fsvn-test-async
    (fsvn-admin-create-repository repos-dir)
    (setq default-directory wc1-dir)
    (fsvn-checkout repos-url)
    (dired wc1-dir)
    ;;add three files and a dir
    (setq list nil)
    (setq file1 (expand-file-name "a" wc1-dir))
    (setq list (cons file1 list))
    (write-region "A" nil file1)
    (setq file2 (expand-file-name "b" wc1-dir))
    (setq list (cons file2 list))
    (write-region "B" nil file2)
    (setq file3 (expand-file-name "c" wc1-dir))
    (setq list (cons file3 list))
    (write-region "C" nil file3)
    (setq dir1 (expand-file-name "z" wc1-dir))
    (setq list (cons dir1 list))
    (make-directory dir1)
    (setq file4 (expand-file-name "a" dir1))
    (write-region "AA" nil file4)
    (setq ignore-file (expand-file-name "ignore" wc1-dir))
    (write-region "ignore" nil ignore-file)
    (fsvn-browse-prop-add-svn:ignore-selected (list ignore-file))
    (fsvn-browse-add-selected (nreverse list))
;;     (fsvn-browse-commit-path)
    (fsvn-call-command-discard "commit" "--message" "add file") ;; revision 1
    (revert-buffer)
    (fsvn-browse-goto-file file1)
    (fsvn-browse-info-path)
    (fsvn-test-sit-for)
    (fsvn-next-file 1) ;; move to file2
    (fsvn-test-sit-for)
    (fsvn-browse-mark-file-mark) ;; mark file2
    (fsvn-browse-mark-file-delete) ;; delete mark file3
    (fsvn-test-sit-for)
    (fsvn-test-equal (fsvn-browse-cmd-selected-urls) (list file2))
    (fsvn-test-equal (fsvn-browse-cmd-selected-urlrevs) (list file2))
    (fsvn-test-sit-for)
    (fsvn-previous-file 2) ;; move to file2
    (fsvn-test-equal (fsvn-browse-cmd-this-urlrev) file2)
    (fsvn-test-equal (fsvn-browse-cmd-this-wc-file) file2)
    (fsvn-test-sit-for)
    (fsvn-browse-mark-file-unmark) ;; unmark file2
    (fsvn-browse-mark-file-unmark) ;; unmark file3
    (fsvn-test-sit-for)
    (fsvn-previous-file 1)
    (fsvn-test-sit-for)
    ;; mkdir
    (setq dir2 (expand-file-name "y" wc1-dir))
    (fsvn-browse-mkdir dir2)
    ;; delete a file
    (fsvn-browse-delete-selected (list file1))
    (fsvn-call-command-discard "commit" "--message" "delete file") ;; revision 2
    (revert-buffer)
    (fsvn-test-sit-for)
    ;; modify a file
    (write-region "Z" nil file2 t)
    (fsvn-browse-diff-this file2)
    (fsvn-call-command-discard "commit" "--message" "modify file") ;; revision 3
    (revert-buffer)
    (fsvn-test-sit-for)
    ;; revert a file
    (write-region "Y" nil file3 t)
    (fsvn-browse-revert-selected (list file3))
    (revert-buffer)
    ;; sort
    (fsvn-browse-toggle-sort)
    (fsvn-test-sit-for)
    (fsvn-browse-toggle-sort)
    (fsvn-test-sit-for)
    ;; lock and unlock file
    (fsvn-browse-lock-selected (list file3))
    (fsvn-test-sit-for)
    (fsvn-browse-unlock-selected (list file3))
    (fsvn-test-sit-for)
    ;; info
    (fsvn-browse-info-path)
    (fsvn-test-sit-for)
    (fsvn-browse-info-selected (list file3))
    (fsvn-test-sit-for)
    ;; blame
    (fsvn-browse-blame-this file3)
    (fsvn-test-sit-for)
    ;; export
    (let ((export-file (expand-file-name "export-file" trash-dir))
	  (export-dir (expand-file-name "export-dir" trash-dir)))
      (fsvn-test-async
       (fsvn-browse-export-this file3 export-file)
       (fsvn-test-sit-for)
       (fsvn-browse-export-path export-dir)
       (fsvn-test-sit-for)))
    ;; copy
    (if (version<= fsvn-svn-version "1.5.0")
	(fsvn-test-async
	 (fsvn-browse-copy-selected (list file2) dir2)
	 (fsvn-browse-copy-selected (list file3) dir2))
      (fsvn-browse-copy-selected (list file2 file3) dir2))
    (fsvn-test-sit-for)
    (fsvn-browse-copy-this file2 (expand-file-name "non-existence" dir2))
    (fsvn-test-sit-for)
    ;; cleanup
    (fsvn-browse-cleanup-path)
    (fsvn-test-sit-for)
    ;; property
    (fsvn-set-prop-value file3 "fsvn:test" "a\nb")
    (fsvn-browse-update-path)
    (fsvn-test-equal (fsvn-get-propget "fsvn:test" file3) "a\nb")
    (fsvn-test-equal (length (fsvn-meta-get-properties file3)) 1)
    (fsvn-test-equal (fsvn-meta-get-property "fsvn:test" file3) "a\nb")
    (fsvn-test-equal (fsvn-get-proplist file3) '("fsvn:test"))
    (let* ((info (fsvn-get-info-entry (fsvn-file-name-nondirectory file2)))
	   (url (fsvn-xml-info->entry=>url$ info))
	   (tmpfile (fsvn-make-temp-file))
	   magic2 magic3 magic4)
      ;; save file
      (fsvn-test-non-nil (fsvn-save-file (fsvn-url-urlrev url "HEAD") tmpfile))
      (fsvn-test-equal (fsvn-get-file-contents tmpfile) "BZ")
      (fsvn-test-non-nil (fsvn-save-file (fsvn-url-urlrev url 2) tmpfile))
      (fsvn-test-equal (fsvn-get-file-contents tmpfile) "B")
      (fsvn-test-equal (fsvn-get-revprop "svn:log" (fsvn-url-urlrev url 2)) "delete file")
      (setq magic2 (fsvn-magic-create-name (fsvn-url-urlrev url 2)))
      (setq magic3 (fsvn-magic-create-name (fsvn-url-urlrev url 3)))
      (setq magic4 (fsvn-magic-create-name (fsvn-url-urlrev url 4)))
      (with-temp-buffer
	(fsvn-test-non-nil (file-exists-p magic2))
	(fsvn-test-nil (file-directory-p magic2))
	(fsvn-test-non-nil (file-readable-p magic2))
	(fsvn-test-non-nil (file-regular-p magic2))
	(fsvn-test-non-nil (file-remote-p magic2))
	(fsvn-test-nil (file-writable-p magic2))
;; 	(file-executable-p           magic2)
;; 	(fsvn-test-nil (file-symlink-p magic2))
	(insert-file-contents magic2)
	(fsvn-test-equal (buffer-string) "B")
	)
      (with-temp-buffer
	(fsvn-test-non-nil (file-exists-p magic3))
	(fsvn-test-nil (file-directory-p magic3))
	(insert-file-contents magic3)
	(fsvn-test-equal (buffer-string) "BZ")
	)
      (with-temp-buffer
	(fsvn-test-nil (file-exists-p magic4))
	)
      )
    (fsvn-call-command-discard "commit" "--message" "modify file") ;; revision 4
    ;; move No.1
    (if (version<= fsvn-svn-version "1.5.0")
	(fsvn-test-async
	 (fsvn-browse-move-selected (list file2) dir1)
	 (fsvn-browse-move-selected (list file3) dir1))
      (fsvn-browse-move-selected (list file2 file3) dir1))
    (fsvn-test-sit-for)
    (fsvn-call-command-discard "commit" "--message" "modify file") ;; revision 5
    ;; move No.2
    (if (version<= fsvn-svn-version "1.5.0")
	(fsvn-test-async
	 (fsvn-browse-move-selected (list (expand-file-name (file-name-nondirectory file2) dir1)) wc1-dir)
	 (fsvn-browse-move-selected (list (expand-file-name (file-name-nondirectory file3) dir1)) wc1-dir))
      (fsvn-browse-move-selected (list (expand-file-name (file-name-nondirectory file2) dir1)
				       (expand-file-name (file-name-nondirectory file3) dir1))
				 wc1-dir))
    (fsvn-test-sit-for)
    (fsvn-call-command-discard "commit" "--message" "modify file") ;; revision 6
    ;; move No.3
    (fsvn-browse-move-this file2 (expand-file-name "non-existence" dir1))
    (fsvn-test-sit-for)
    (fsvn-call-command-discard "commit" "--message" "modify file") ;; revision 7
    ;; conflict test
    (cd wc2-dir)
    (fsvn-checkout repos-url)
    (find-file (fsvn-test-switch-file wc1-dir wc2-dir file2))
    (goto-char (point-max))
    (insert "Z")
    (save-buffer)
    (fsvn-call-command-discard "commit" "--message" "make conflicted state")
    (cd wc1-dir)
    (find-file file2)
    (goto-char (point-max))
    (insert "z")
    (save-buffer)
    ;;todo
;;     (let ((proc (fsvn-browse-update-path)))
;;       (sit-for 2) ;; waiting response
;;       (process-send-string proc "p"))
      ;;todo
      ;;       (fsvn-set-revprop-value (fsvn-url-urlrev url 2) 
    
    )
   (fsvn-test-async
    (dired wc3-dir)
    (setq default-directory wc3-dir)
    (fsvn-checkout repos-url)
    (fsvn-dired-toggle-browser)
    (fsvn-test-sit-for)
    (fsvn-browse-logview-path)
    (fsvn-test-sit-for))
   (fsvn-test-wait-for-all-process)))

;; check coverage.
(when fsvn-test-check-through-all
  (let ((buffer (fsvn-make-temp-buffer))
	(count 0))
    (with-current-buffer buffer
      (mapcar
       (lambda (symbol)
	 (let ((symbol-name (symbol-name symbol)))
	   (cond
	    ((memq symbol fsvn-test-done-functions)
	     (setq count (1+ count)))
	    (t
	     (insert symbol-name "\n")))))
       fsvn-test-target-functions))
    (pop-to-buffer buffer t)
    (message "%d / %d functions through." count (length fsvn-test-target-functions))))



;; todo
;; subr-arity

(defun fsvn-test-make-conflicted-file (file)
  (let (url magic)
    (write-region (format-time-string "%c\n") nil file t)
    (setq url (fsvn-wc-file-repository-url file))
    (setq magic (fsvn-magic-create-name url))
    (write-region (format-time-string "%c\n" (seconds-to-time (- (float-time) 20))) nil magic t)))

(defun fsvn-test-add-file-in-repos (dir)
  (let* ((file (fsvn-make-temp-file))
	 (url (fsvn-wc-file-repository-url dir))
	 (magic (fsvn-magic-create-name url)))
    (write-region (format-time-string "%c\n") nil file t)
    (fsvn-asap-add-file file url (format-time-string "%H-%M-%S"))))

;; todo
(defun fsvn-test-magic-handler ()
  (let (ret)
    (mapcar
     (lambda (cell)
       (cond
	;; if Emacs function not provided, ignore this.
	((not (functionp (car cell)))
	 (setq ret (cons (format "%s is not a provided function." (car cell)) ret)))
	((not (functionp (cdr cell)))
	 (setq ret (cons (format "Magic handler not defined for %s" (car cell)) ret)))
	((subrp (symbol-function (car cell)))
	 )
	((symbol-function (car cell))
	 )
	((equal (cadr (symbol-function (car cell))) 
		(cadr (symbol-function (cdr cell))))
	 cell)
	(t
	 nil)))
     fsvn-magic-handler-alist)
    (nreverse ret)))




;;; fsvn-test.el ends here
