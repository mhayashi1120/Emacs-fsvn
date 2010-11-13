;;; -*- Coding: utf-8 -*-
;;; fsvn-publish.el --- Fsvn developer utilities


;;; Commentary:
;; 

;;; Code:

;; requires extended emacs-wiki-mode that have not been published.

(defvar fsvn-publish-doc-directory
  "~/.emacs.d/util/fsvn/DOC")

(defconst fsvn-publish-template-directory
  (expand-file-name "Templates" fsvn-publish-doc-directory))

(defconst fsvn-publish-publishing-directory
  (expand-file-name "WebWiki" fsvn-publish-doc-directory))

(defconst fsvn-publish-wiki-directory
  (expand-file-name "Wiki" fsvn-publish-doc-directory))

(defun fsvn-publish-content-lang (html)
  (let ((regexp "\\.html\\.\\(..\\)$"))
    (string-match regexp html)
    (match-string 1 published)))

(defun fsvn-publish-html-file (piki)
  (let ((path (relative-file-name piki fsvn-publish-wiki-directory)))
    (unless (string-match "/\\([^/]+\\)-\\(..\\)\\.piki$" piki)
      (error "Invalid filename %s" piki))
    (let ((name (match-string 1 piki))
	  (lang (match-string 2 piki))
	  (newpath (expand-file-name (file-name-directory path) fsvn-publish-publishing-directory)))
      (expand-file-name (format "%s.html.%s" name lang) newpath))))

(defun fsvn-publish-directory-files (dir regexp)
  (let (ret)
    (mapc
     (lambda (file)
       (cond
	((file-directory-p file)
	 (setq ret (append ret (fsvn-publish-directory-files file regexp))))
	((string-match regexp file)
	 (setq ret (cons file ret)))))
     (directory-files dir t dired-re-no-dot))
    ret))

(defun fsvn-publish-files ()
  (fsvn-publish-directory-files fsvn-publish-wiki-directory "\\.piki$"))

(defun fsvn-publish ()
  (interactive)
  (let* ((coding-system-for-read 'utf-8-unix)
	 (coding-system-for-write 'utf-8-unix)
	 (template (expand-file-name "template.htm" fsvn-publish-template-directory)))
    (with-temp-buffer
      (mapc
       (lambda (piki)
	 (let ((html (fsvn-publish-html-file piki)))
	   (shell-command (format "piki %s %s > %s" template piki html))))
       (fsvn-publish-files)))))

(provide 'fsvn-publish)

;;; fsvn-publish.el ends here
