;;; fsvn-make.el --- fsvn make lisp


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:
;;



(setq ALL-MODULES 
      (list
       "fsvn.el"

       "fsvn-admin.el"
       "fsvn-blame.el"
       "fsvn-browse.el"
       "fsvn-cmd.el"
       "fsvn-config.el"
       "fsvn-data.el"
       "fsvn-debug.el"
       "fsvn-deps.el"
       "fsvn-diff.el"
       "fsvn-dired.el"
       "fsvn-env.el"
       "fsvn-fs.el"
       "fsvn-logview.el"
       "fsvn-magic.el"
       "fsvn-minibuf.el"
       "fsvn-mode.el"
       "fsvn-msgedit.el"
       "fsvn-parasite.el"
       "fsvn-popup.el"
       "fsvn-proc.el"
       "fsvn-propview.el"
       "fsvn-pub.el"
       "fsvn-select.el"
       "fsvn-svk.el"
       "fsvn-tortoise.el"
       "fsvn-ui.el"
       "fsvn-url.el"
       "fsvn-xml.el"
       ))

(when (memq system-type '(windows-nt))
  (setq ALL-MODULES
	(append ALL-MODULES (list "fsvn-win.el")))
  (unless (featurep 'meadow)
    (setq ALL-MODULES
	(append ALL-MODULES (list 
			     "mw32cmp.el"
			     "mw32mci.el"
			     "mw32misc.el"
			     "mw32script.el"
			     )))))

(defun compile-fsvn ()
  (initialize)
  (compile))

(defun check-fsvn ()
  (initialize)
  (lint)
  (compile)
  (test))

(defun install-fsvn ()
  (initialize)
  (install))

(defun what-where-fsvn ()
  (initialize)
  (install t))

(defun initialize ()
  (let ((config (or (car command-line-args-left) "MAKE-CFG")))
    (setq load-path (cons "." load-path))
    (load config)
    (when (and (eq system-type 'windows-nt)
	       (not (featurep 'meadow)))
      (unless (and (file-exists-p "mw32mci.el")
		   (file-exists-p "mw32misc.el")
		   (file-exists-p "mw32script.el"))
	;; for NTEmacs
	(princ "\n")
	(princ "-------------------------------------------------------------\n")
	(princ "For NTEmacs user\n")
	(princ "To complete this installation, please read mw32cmp.el header.\n")
	(princ "-------------------------------------------------------------\n")
	(princ "\n")
	(error "")))))

(defun compile ()
  (mapc
   (lambda (m)
     (byte-compile-file m))
   ALL-MODULES))

(defun lint ()
  (mapc
   (lambda (module)
     (find-file module)
     (eval-buffer)
     (elint-initialize)
     (elint-current-buffer)
     (with-current-buffer "*Elint*"
       (message (buffer-string))))
   ALL-MODULES))

(defun install (&optional just-print)
  (unless (or just-print (file-directory-p INSTALL-DIR))
    (make-directory INSTALL-DIR t))
  (let (src dest elc el)
    (mapc
     (lambda (m)
       (setq el m)
       (setq elc (concat m "c"))
       (setq dest-el (expand-file-name el INSTALL-DIR))
       (setq dest-elc (expand-file-name elc INSTALL-DIR))
       (princ (format "%s -> %s\n" el dest-el))
       (princ (format "%s -> %s\n" elc dest-elc))
       (unless just-print
	 (mapc
	  (lambda (src-dest)
	    (let ((src (car src-dest))
		  (dest (cdr src-dest)))
	      (unless (file-exists-p src)
		(error "%s not exists." src))
	      (copy-file src dest t)
	      (set-file-modes dest ?\644)))
	  (list (cons el dest-el) (cons elc dest-elc)))))
     ALL-MODULES)))

(defun test ()
  (mapc
   (lambda (m)
     (load-file m))
   ALL-MODULES)
  (load-file "fsvn-test.el")
  (princ "\n")
  (princ "-------------------------------------------------------------\n")
  (princ "Test completed\n")
  (princ "-------------------------------------------------------------\n")
  (princ "\n"))

;;; fsvn-make.el ends here
