;;; fsvn-svk.el --- svk utility for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



(require 'fsvn-proc)
(require 'fsvn-popup)
(require 'fsvn-browse)



(defvar process-environment)
(defvar system-type)



;; http://svk.bestpractical.com/view/HomePage

(defgroup fsvn-svk nil
  "`svk' Utilities."
  :group 'fsvn)

(defcustom fsvn-svk-perl-command nil
  "*Perl command that executing `fsvn-svk-script'.
If there is executing problem in windows/cygwin then set path to perl.exe."
  :group 'fsvn-svk)

(defcustom fsvn-svk-script "svk"
  "*Perl script file of `svk'"
  :group 'fsvn-svk)

(defcustom fsvn-svk-perllib (getenv "PERLLIB")
  "*Environment variable `PERLLIB' value when executing `svk' command"
  :group 'fsvn-svk)

(defcustom fsvn-svk-mirror-depot "mirror"
  "*Name of path to `svk' mirroring repository. (Generally \"mirror\")"
  :group 'fsvn-svk)

(defcustom fsvn-svk-editor-command
  (or
   (getenv "EDITOR")
   (and (featurep 'meadow)
	(executable-find "gnuclient.exe"))
   "emacsclient")
  "*Client program for edit conflicted file."
  :group 'fsvn-svk)

(defvar fsvn-svk-browse-map nil)

(unless fsvn-svk-browse-map
  (setq fsvn-svk-browse-map
	(let ((map (make-sparse-keymap)))
	  (suppress-keymap map)
	  (define-key map "\eI" 'fsvn-svk-browse-create)
	  (define-key map "S" 'fsvn-svk-browse-push)
	  (define-key map "L" 'fsvn-svk-browse-pull)
	  (define-key map "R" 'fsvn-svk-browse-resync)
	  map)))

(add-hook 'fsvn-browse-mode-hook
	  (lambda () (define-key fsvn-browse-mode-map "\C-cv" fsvn-svk-browse-map)))

;; fsvn-svk internal function

(defun fsvn-svk-home-directory ()
  (fsvn-expand-file ".svk" (getenv "HOME")))

(defun fsvn-svk-depotmap-directory ()
  (let ((conf-file (fsvn-expand-file "config" (fsvn-svk-home-directory))))
    (when (file-exists-p conf-file)
      (with-temp-buffer
	(insert-file-contents conf-file)
	(goto-char (point-min))
	(when (re-search-forward "^depotmap:" nil t)
	  (when (re-search-forward "^[ \t]*\\(?:\"\"\\|''\\):[ \t]*\\(.+\\)" nil t)
	    (fsvn-expand-file (match-string 1))))))))

(defun fsvn-svk-depotmap-url ()
  (let ((dir (fsvn-svk-depotmap-directory)))
    (when dir
      (fsvn-file-name-as-repository dir))))

(defun fsvn-svk-depotpath-url (depotpath)
  (when (string-match "^//\\(.*\\)" depotpath)
    (fsvn-expand-url (match-string 1 depotpath) (fsvn-svk-depotmap-url))))

(defun fsvn-svk-mirrored-repos-root (file)
  (let ((prop (fsvn-get-file-parent-property file "svm:source"))
	list)
    (when prop
      (setq list (split-string prop "!"))
      (concat (car list) (mapconcat 'identity (cdr list) "!")))))

(defun fsvn-svk-mirroring-depotpath (file)
  (let* ((root (fsvn-svk-depotmap-url))
	 (uuid (fsvn-get-file-parent-property file "svm:uuid"))
	 (top-url (fsvn-expand-url fsvn-svk-mirror-depot root))
	 url depot)
    (catch 'found
      (mapc
       (lambda (entry)
	 (setq url (fsvn-expand-url (fsvn-xml-lists->list->entry=>name$ entry) top-url))
	 (when (string= (fsvn-get-propget "svm:uuid" url) uuid)
	   (setq depot (concat (fsvn-svk-mirror-depotpath) (fsvn-xml-lists->list->entry=>name$ entry)))
	   (throw 'found depot)))
       (fsvn-get-ls top-url))
      nil)))

(defun fsvn-svk-file-depotpath (file)
  (let (prop)
    (setq prop (fsvn-get-file-parent-property file "svm:uuid" t))
    (when prop
      (let (uuid1 uuid2 repos url name topdir info)
	(setq topdir (car prop))
	(setq uuid1 (cdr prop))
	(setq repos (fsvn-svk-depotmap-url))
	(setq info (fsvn-get-info-entry topdir))
	(concat "/" (fsvn-info-repos-path info))))))

(defun fsvn-svk-mirror-depotpath ()
  (concat "//" fsvn-svk-mirror-depot "/"))

(defun fsvn-svk-browse-depotpath ()
  (fsvn-svk-file-depotpath (fsvn-browse-current-directory)))

(defun fsvn-svk-browse-mirroring-depotpath ()
  (fsvn-svk-mirroring-depotpath (fsvn-browse-current-directory)))

(defun fsvn-svk-browse-wc-p ()
  (not (not (fsvn-svk-browse-depotpath))))

(defun fsvn-svk-browse-check-exec ()
  (unless (or (and fsvn-svk-perl-command
		   (executable-find fsvn-svk-perl-command)
		   (or (and (file-name-absolute-p fsvn-svk-script)
			    (file-exists-p fsvn-svk-script))
		       (executable-find fsvn-svk-script)))
	      (executable-find fsvn-svk-script))
    (error "cannot execute %s" fsvn-svk-script)))

(defun fsvn-svk-browse-draw-mirrored-url ()
  (save-excursion
    (let ((mirroered-url (fsvn-svk-mirrored-repos-root default-directory))
	  buffer-read-only)
      (when mirroered-url
	(goto-char (point-min))
	(when (re-search-forward fsvn-browse-re-root nil t)
	  (replace-match mirroered-url t nil nil 2))))))

(defmacro fsvn-svk-process-environment (&rest form)
  `(fsvn-process-environment 
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "PERLLIB" fsvn-svk-perllib)
      (setenv "EDITOR" fsvn-svk-editor-command)
      ,@form)))

(defun fsvn-svk-start-command (subcommand buffer &rest args)
  (fsvn-svk-process-environment
   (let ((real-args (fsvn-flatten-command-args args))
	 internal-args proc script)
     (setq internal-args 
	   (if (null fsvn-svk-perl-command)
	       (list fsvn-svk-script)
	     (setq script (executable-find fsvn-svk-script))
	     (unless script
	       (error "Execution error. Script(%s) not found" fsvn-svk-script))
	     (list fsvn-svk-perl-command script)))
     (setq internal-args (append internal-args (cons subcommand real-args)))
     (fsvn-debug internal-args)
     (setq proc (apply 'start-process "fsvn svk" buffer internal-args))
     (set-process-sentinel proc 'fsvn-svk-process-sentinel)
     (set-process-filter proc 'fsvn-popup-process-filter-in-buffer)
     (with-current-buffer buffer
       (when (eq major-mode 'fsvn-popup-result-mode)
	 (setq fsvn-popup-result-process proc)))
     proc)))

(defun fsvn-svk-process-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (setq fsvn-popup-result-process nil)
    (when (and (eq system-type 'windows-nt)
	       (/= (process-exit-status proc) 0))
      (fsvn-svk-win-start-external-window (process-command proc)))))

;;FIXME windows native perl.exe cannot accept terminal input
(defun fsvn-svk-win-start-external-window (args)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "EDITOR" fsvn-svk-editor-command)
    (fsvn-win-start-external-terminal args)))

(defvar fsvn-svk-read-depotpath-history nil)
(defun fsvn-svk-read-depotpath (prompt &optional initial-contents)
  (read-from-minibuffer prompt initial-contents nil nil 'fsvn-svk-read-depotpath-history))

(defun fsvn-svk-browse-cmd-read-create ()
  (when (eq system-type 'windows-nt)
    (error "Not supported this function on windows."))
  (let (url mirrorpath depotpath)
    (fsvn-brief-message-showing 
     (setq url (fsvn-browse-current-repository-url))
     (setq url (fsvn-completing-read-url "Mirroring URL: " url t))
     (fsvn-brief-message-add-message (format "Mirrored URL: %s" url))
     (setq mirrorpath (fsvn-svk-read-depotpath "Depotpath (Read-only mirror): " (fsvn-svk-mirror-depotpath)))
     (fsvn-brief-message-add-message (format "Mirroring Path (Read-only): %s" mirrorpath))
     (setq depotpath (fsvn-svk-read-depotpath "Depotpath (For working copy): " "//")))
    (list url mirrorpath depotpath)))

;; fsvn-svk interactive command

(defun fsvn-svk-browse-create (mirrored-url mirroring-depotpath depotpath)
  "Initialize and create `svk' repository to HOME directory."
  (interactive (fsvn-svk-browse-cmd-read-create))
  (fsvn-async-let ((buffer (fsvn-popup-result-create-buffer))
		   (url mirrored-url)
		   (mirrorpath mirroring-depotpath)
		   (depotpath depotpath))
    (fsvn-buffer-popup-as-information buffer)
    ;; create ~/.svk and local repository
    (fsvn-svk-start-command "depotmap" buffer "--init")
    ;; create mirrorpath as mirroring repository and adding svm:* property
    (fsvn-svk-start-command "mirror" buffer mirrorpath url)
    ;; get all revision log and data to mirroring repository
    (fsvn-svk-start-command "sync" buffer mirrorpath)
    ;; copy mirroring repository to working repository
    (fsvn-svk-start-command "copy" buffer mirrorpath depotpath)
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\n")
      (insert "####################################################\n")
      (insert "Done mirrorring.\n")
      (insert "Checkout " (fsvn-svk-depotpath-url depotpath) "\n")
      (insert "####################################################\n"))))

(defun fsvn-svk-browse-push ()
  "Push working copy repository to mirroring repository and mirrored repository."
  (interactive)
  (fsvn-svk-browse-check-exec)
  (let ((buffer (fsvn-popup-result-create-buffer))
	(depotpath (fsvn-svk-browse-depotpath)))
    (unless depotpath
      (error "This directory has no depotpath"))
    (prog1
	(fsvn-svk-start-command "push" buffer depotpath)
      (fsvn-buffer-popup-as-information buffer))))

(defun fsvn-svk-browse-pull ()
  "Pull mirroring repository to working copy repository."
  (interactive)
  (fsvn-svk-browse-check-exec)
  (let ((buffer (fsvn-popup-result-create-buffer))
	(depotpath (fsvn-svk-browse-depotpath)))
    (unless depotpath
      (error "This directory has no depotpath"))
    (prog1
	(fsvn-svk-start-command "pull" buffer depotpath)
      (fsvn-buffer-popup-as-information buffer))))

(defun fsvn-svk-browse-resync ()
  "Synchronize mirroring repository."
  (interactive)
  (fsvn-svk-browse-check-exec)
  (let ((buffer (fsvn-popup-result-create-buffer))
	(depotpath (fsvn-svk-browse-mirroring-depotpath)))
    (unless depotpath
      (error "This directory has no mirroring depotpath"))
    (prog1
	(fsvn-svk-start-command "sync" buffer depotpath)
      (fsvn-buffer-popup-as-information buffer))))



;; modify browse-mode buffer

(add-hook 'fsvn-browse-mode-hook 'fsvn-svk-browse-draw-mirrored-url)



(provide 'fsvn-svk)

;;; fsvn-svk.el ends here
