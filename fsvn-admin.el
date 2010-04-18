;;; fsvn-admin.el --- Interface for svnadmin.


;;; History:
;; 

;;; Commentary:
;; 

(require 'fsvn-deps)
(require 'fsvn-proc)
(require 'fsvn-debug)
(require 'fsvn-ui)



(defvar process-environment)
(defvar current-prefix-arg)



(defgroup fsvn-admin nil
  ""
  :group 'fsvn)

(defun fsvn-admin-start-command (subcommand buffer &rest args)
  (fsvn-process-environment
   (let ((real-args (fsvn-flatten-command-args args)))
     (fsvn-debug real-args)
     (apply 'start-process "fsvn admin" buffer fsvn-svnadmin-command subcommand real-args))))

(defun fsvn-admin-call-command (subcommand buffer &rest args)
  (fsvn-process-environment
   (let ((real-args (fsvn-flatten-command-args args)))
     (fsvn-debug real-args)
     (prog1
	 (apply 'call-process fsvn-svnadmin-command nil buffer nil subcommand real-args)
       (fsvn-debug buffer)))))

(defun fsvn-admin-create-repository (dir &optional args)
  "Create repository to current directory."
  (interactive (list (fsvn-expand-file default-directory)
		     (when current-prefix-arg 
		       (fsvn-read-svnadmin-subcommand-args "create"))))
  (let ((buffer (fsvn-make-temp-buffer)))
    (prog1
	(fsvn-admin-start-command "create" buffer dir args)
      (fsvn-buffer-popup-as-information buffer))))

(defun fsvn-admin-command (subcommand args)
  "Execute `svnadmin' command with completing read."
  (interactive (let (subcommand args)
		 (setq subcommand (fsvn-read-svnadmin-subcommand))
		 (setq args (fsvn-read-svnadmin-subcommand-args subcommand))
		 (list subcommand args)))
  (let ((buffer (fsvn-make-temp-buffer)))
    (prog1
	(fsvn-admin-start-command subcommand buffer args)
      (fsvn-buffer-popup-as-information buffer))))

(defun fsvn-admin-show-svnadmin-help (subcommand)
  "Show `svnadmin' help for SUBCOMMAND."
  (interactive (list (fsvn-read-svnadmin-subcommand)))
  (let ((buffer (fsvn-make-temp-buffer))
	(fsvn-process-environment-lang fsvn-help-locale))
    (fsvn-admin-call-command "help" buffer subcommand)
    (fsvn-buffer-popup-as-information buffer)))

(provide 'fsvn-admin)

;;; fsvn-admin.el ends here
