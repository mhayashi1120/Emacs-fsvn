;;; -*- Coding: utf-8 -*-
;;; fsvn-publish.el --- Fsvn developer utilities


;;; Commentary:
;; 

;;; Code:

(defvar fsvn-publish-doc-directory
  "~/.emacs.d/util/fsvn/DOC")

(set-alist 'emacs-wiki-projects
	     "FsvnWiki" 
	     '((emacs-wiki-directories . ("~/.emacs.d/util/fsvn/DOC/Wiki/" "~/.emacs.d/util/fsvn/DOC/Wiki/howto"))
	       (emacs-wiki-project-server-prefix)
	       (emacs-wiki-publishing-directory . "~/.emacs.d/util/fsvn/DOC/WebWiki")))

(defconst fsvn-publish-template-directory
  (expand-file-name "Templates" fsvn-publish-doc-directory))

(defconst fsvn-publish-publishing-transform
      '(
	("^basicuse-ja.wiki.html$" . "howto/basicuse.html.ja")
	("^basicuse-en.wiki.html$" . "howto/basicuse.html.en")
	("^install-ja.wiki.html$" . "howto/install.html.ja")
	("^install-en.wiki.html$" . "howto/install.html.en")
	))

(defconst fsvn-publish-publishing-directory
  (expand-file-name "WebWiki" fsvn-publish-doc-directory))

(defconst fsvn-publish-page-alist
  '(
    ("Wiki/howto/basicuse-ja.wiki" 
     (title . "fsvn.el の使い方"))
    ("Wiki/howto/basicuse-en.wiki" 
     (title . "How to use fsvn.el"))
    ("Wiki/howto/install-ja.wiki" 
     (title . "fsvn.el のインストール"))
    ("Wiki/howto/install-en.wiki" 
     (title . "How to install fsvn.el"))
    ))

(defun fsvn-publish-content-lang ()
  (defvar published)
  (let ((regexp (concat (regexp-quote emacs-wiki-publishing-file-suffix) ".\\(.+\\)$")))
    (string-match regexp published)
    (match-string 1 published)))

(defun fsvn-publish-header ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "header.htm" fsvn-publish-template-directory))
    (buffer-string)))

(defun fsvn-publish-footer ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "footer.htm" fsvn-publish-template-directory))
    (buffer-string)))

(defun fsvn-publish-files ()
  (directory-files (expand-file-name "Wiki/howto" fsvn-publish-doc-directory) t "\\.wiki")
  )

(defun fsvn-publish-current-file ()
  (relative-file-name emacs-wiki-current-file fsvn-publish-doc-directory))

(defun fsvn-publish-page-value (key)
  (let ((cell (assoc (fsvn-publish-current-file) fsvn-publish-page-alist)))
    (cdr (assq key cell))))

(defun fsvn-publish ()
  (interactive)
  (require 'emacs-wiki)
  (let* ((emacs-wiki-current-project "FsvnWiki")
	 (emacs-wiki-directories)
	 (emacs-wiki-publishing-directory fsvn-publish-publishing-directory)
	 (emacs-wiki-publishing-header (fsvn-publish-header))
	 (emacs-wiki-publishing-footer (fsvn-publish-footer))
	 (emacs-wiki-publishing-file-suffix ".html")
	 (emacs-wiki-publishing-transforms  fsvn-publish-publishing-transform)
	 (emacs-wiki-after-file-publish-hook)
	 (coding-system-for-read 'utf-8-unix)
	 (coding-system-for-write 'utf-8-unix)
	 )
    (add-hook 'emacs-wiki-after-file-publish-hook 'fsvn-publish-after-publish)
    (emacs-wiki-publish-files (fsvn-publish-files) t)))

(defun fsvn-publish-after-publish (dummy)
  (defvar published)
  (let* ((file published)
	 (buffer (get-file-buffer file))
	 kill)
    (unless buffer
      (setq kill t)
      (setq buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (revert-buffer nil t)
      (let (buffer-read-only)
	(indent-region (point-min) (point-max))
	(save-buffer file)))
    (when kill
      (kill-buffer buffer))))

(provide 'fsvn-publish)

;;; fsvn-publish.el ends here
