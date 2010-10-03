;;; fsvn-diff.el --- Diff utility for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'diff)
(require 'fsvn-deps)



(defvar auto-mode-alist)
(defvar ediff-window-A)
(defvar ediff-window-B)



(defvar fsvn-ediff-previous-configuration nil)

(fsvn-defstruct ediff-config
  window file1 file2)

(defun fsvn-ediff-files (file1 file2)
  (let* ((fsvn-ediff-previous-configuration
	  (fsvn-struct-ediff-config-make
	   :window (current-window-configuration)
	   :file1 (cons file1 (get-file-buffer file1))
	   :file2 (cons file2 (get-file-buffer file2))))
	 (hook '(fsvn-ediff-startup-hooks)))
    (ediff-files file1 file2 hook)))

(defun fsvn-ediff-directories (dir1 dir2)
  (ediff-directories dir1 dir2 nil))

(defun fsvn-ediff-startup-hooks ()
  (let ((func `(lambda () (fsvn-ediff-exit-hook ',fsvn-ediff-previous-configuration))))
    (add-hook 'ediff-after-quit-hook-internal func nil 'local)))

(defun fsvn-ediff-exit-hook (prev-config)
  (set-window-configuration (fsvn-struct-ediff-config-get-window prev-config))
  (mapc
   (lambda (file)
     (let ((name (car file))
	   (buffer (cdr file)))
       (unless buffer
	 (when (get-file-buffer name)
	   (kill-buffer (get-file-buffer name))))))
   (list (fsvn-struct-ediff-config-get-file1 prev-config)
	 (fsvn-struct-ediff-config-get-file2 prev-config))))

(defun fsvn-ediff-hash-directory (urlrev)
  (let ((dir (fsvn-expand-file (md5 (fsvn-urlrev-dirname urlrev)) (fsvn-ediff-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun fsvn-ediff-make-temp-file (urlrev)
  (let* ((topdir (fsvn-ediff-hash-directory urlrev))
	 (file (fsvn-expand-file (fsvn-url-ediff-filename urlrev) topdir)))
    (write-region (point-min) (point-min) file nil 'no-msg)
    file))

(defun fsvn-ediff-make-temp-directory (urlrev)
  (let* ((topdir (fsvn-ediff-hash-directory urlrev))
	 (dir (fsvn-expand-file (fsvn-url-ediff-filename urlrev) topdir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;; subcommand `diff' utility

(defvar fsvn-diff-buffer-subcommand-args nil)

(defun fsvn-diff-start-process (&rest args)
  (let ((buffer (fsvn-diff-get-buffer args))
	proc)
    (prog1
	(setq proc (fsvn-start-command-display "diff" buffer args))
      (fsvn-diff-setup-mode buffer args)
      (fsvn-buffer-popup-as-information buffer)
      (set-process-sentinel proc (lambda (proc event))))))

(defun fsvn-diff-get-buffer (diff-args)
  (let ((args (fsvn-flatten-command-args diff-args))
	buffer)
    (catch 'found
      (mapc
       (lambda (b)
	 (with-current-buffer b
	   (when (equal fsvn-diff-buffer-subcommand-args args)
	     (erase-buffer)
	     (throw 'found b))))
       (buffer-list))
      (generate-new-buffer (format "*Fsvn diff %s*" (fsvn-diff-buffer-key-name args))))))

(defun fsvn-diff-setup-mode (buffer args)
  (with-current-buffer buffer
    (diff-mode)
    (let ((real-args (fsvn-flatten-command-args args)))
      (set (make-local-variable 'fsvn-popup-result-buffer-p) t)
      (set (make-local-variable 'fsvn-diff-buffer-subcommand-args) real-args))))

(defun fsvn-diff-buffer-key-name (args)
  (catch 'decide
    (mapc
     (lambda (x)
       (cond
	((fsvn-url-local-p x)
	 (throw 'decide (fsvn-url-filename x)))
	((string-match fsvn-diff-subcommand-arg-regexp x)
	 (throw 'decide (fsvn-urlrev-filename (match-string 2 x))))))
     args)
    (error "Diff keyname not found")))



(defun fsvn-diff-file-alist (file)
  (let (base-line wc-line ret)
    (with-temp-buffer
      (unless (= (fsvn-call-command "diff" (current-buffer) file) 0)
	(error "Executing error while `diff'"))
      (goto-char (point-min))
      (while (re-search-forward fsvn-diff-separated-regexp nil t)
	(setq base-line (string-to-number (match-string 1))
	      wc-line (string-to-number (match-string 3)))
	(forward-line 1)
	(while (not (or (looking-at "^@@") (eobp)))
	  (cond
	   ((looking-at "^-")
	    (setq base-line (1+ base-line))
	    (setq ret (cons (cons nil base-line) ret)))
	   ((looking-at "^\\+")
	    (setq wc-line (1+ wc-line))
	    (setq ret (cons (cons wc-line nil) ret)))
	   (t
	    (setq base-line (1+ base-line))
	    (setq wc-line (1+ wc-line))))
	  (forward-line 1))))
    (nreverse ret)))



(defun fsvn-diff-create-patch (patch-file &rest args)
  (let (proc)
    (write-region "" nil patch-file nil 'no-msg)
    (setq proc (apply 'fsvn-start-process nil "diff" args))
    (set-process-sentinel proc (lambda (proc event) 
				 (message "Patch was created.")))
    (set-process-filter proc `(lambda (proc event)
				(write-region event nil ,patch-file t 'no-msg)))
    proc))



(provide 'fsvn-diff)

;;; fsvn-diff.el ends here
