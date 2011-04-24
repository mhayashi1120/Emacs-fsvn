;;; -*-Emacs-Lisp-*-

;; MAKE-CFG.el: installation setting about fsvn.

;;; Code:

(setq fsvn-svn-command "svn")
(setq fsvn-svnadmin-command "svnadmin")

(setq INSTALL-DIR (expand-file-name "../share/emacs/site-lisp/fsvn" (invocation-directory)))
(setq INSTALL-IMAGE-DIR (expand-file-name "images/fsvn" data-directory))


;; (setq load-path (cons "~/some/directory" load-path))

;;; MAKE-CFG.el ends here
