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
    (match-string 1 html)))

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
         (template (expand-file-name "template.htm" fsvn-publish-template-directory))
         (message-log-max))
    (with-temp-buffer
      (mapc
       (lambda (piki)
         (let ((html (fsvn-publish-html-file piki)))
           (shell-command (format "piki %s %s > %s" template piki html))
           (message nil)
           (fsvn-publish-prepare-lang html)))
       (fsvn-publish-files)))
    (message "Wiki files are published.")))

(defun fsvn-publish-prepare-lang (html)
  (let ((lang (fsvn-publish-content-lang html)))
    (with-temp-buffer
      (insert-file-contents html)
      (fsvn-publish-replace-in-buffer 
       "[ \t]*<meta name=\"Content-Language\" content=\"\\(\\)\""
       1 lang)
      (fsvn-publish-replace-in-buffer 
       "[ \t]*<html lang=\"\\(\\)\""
       1 lang)
      (fsvn-publish-replace-in-buffer 
       "^[ \t]*Published: TODO"
       0 (format "Published: %s" (format-time-string "%Y-%m-%d %H:%M")))
      (write-region (point-min) (point-max) html nil 'no-msg))))

(defun fsvn-publish-replace-in-buffer (regexp subexp new-text)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match new-text nil nil nil subexp))))

(provide 'fsvn-publish)

;;; fsvn-publish.el ends here
