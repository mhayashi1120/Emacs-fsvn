;;; fsvn-make.el --- fsvn make lisp


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



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
       "fsvn-proclist.el"
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
                             "mw32script.el"
                             )))))

(defun make-fsvn ()
  (fsvn-make-initialize)
  (fsvn-make-single-file))

(defun compile-fsvn ()
  (fsvn-make-initialize)
  (fsvn-make-compile))

(defun check-fsvn ()
  (fsvn-make-initialize)
  (fsvn-make-lint)
  (fsvn-make-compile)
  ;; see comment in `fsvn-test-excursion' at fsvn-test.el
  (condition-case err
      (progn
        (fsvn-make-test)
        (kill-emacs))
    (error
     (princ err)
     (kill-emacs 1))))

(defun install-fsvn ()
  (fsvn-make-initialize)
  (fsvn-make-install))

(defun uninstall-fsvn ()
  (fsvn-make-initialize)
  (fsvn-make-uninstall))

(defun what-where-fsvn ()
  (fsvn-make-initialize)
  (fsvn-make-install t))

(defun fsvn-make-initialize (&optional suppress-warnings)
  (let ((config (or (car command-line-args-left) "MAKE-CFG")))
    (setq load-path (cons "." load-path))
    (load config)
    (when (and (not suppress-warnings)
               (eq system-type 'windows-nt)
               (not (featurep 'meadow)))
      (unless (and (file-exists-p "mw32script.el"))
        ;; for NTEmacs
        (princ "\n")
        (princ "-------------------------------------------------------------\n")
        (princ "For NTEmacs user\n")
        (princ "To complete this installation, please read mw32cmp.el header.\n")
        (princ "-------------------------------------------------------------\n")
        (princ "\n")
        (error "")))))

(defun fsvn-make-compile ()
  (mapc
   (lambda (m)
     (byte-compile-file m))
   ALL-MODULES))

(defun fsvn-make-lint ()
  (elint-initialize)
  (mapc
   (lambda (module)
     (find-file module)
     (eval-buffer)
     (elint-current-buffer)
     (with-current-buffer "*Elint*"
       (message (replace-regexp-in-string "%" "%%" (buffer-string)))))
   ALL-MODULES))

(defun fsvn-make-uninstall ()
  (unless (file-directory-p INSTALL-DIR)
    (error "fsvn is not installed"))
  (fsvn-make-delete-directory INSTALL-DIR)
  (fsvn-make-delete-directory INSTALL-IMAGE-DIR))

(defun fsvn-make-delete-directory (dir)
  (princ (format "Delete recursively %s...\n" dir))
  (mapc
   (lambda (file)
     (cond
      ((not (eq (car (file-attributes file)) t))
       (princ (format "Deleting %s\n" file))
       (condition-case err
           (delete-file file)
         (princ (format "Failed to delete %s\n" file))))
      (t
       (fsvn-make-delete-directory file))))
   (directory-files dir t "^[^.][^.]"))
  (princ (format "Deleting %s\n" dir))
  (condition-case err
      (delete-directory dir)
    (princ (format "Failed to delete %s\n" dir))))

(defun fsvn-make-install (&optional just-print)
  (unless (or just-print (file-directory-p INSTALL-DIR))
    (make-directory INSTALL-DIR t))
  (mapc
   (lambda (m)
     (let* ((el m)
            (elc (concat m "c"))
            (dest-el (expand-file-name el INSTALL-DIR))
            (dest-elc (expand-file-name elc INSTALL-DIR)))
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
          (list (cons el dest-el) (cons elc dest-elc))))))
   ALL-MODULES)
  (unless (or just-print (file-directory-p INSTALL-IMAGE-DIR))
    (make-directory INSTALL-IMAGE-DIR t))
  (mapc
   (lambda (img)
     (let ((src (concat "images/" img))
           (dest (expand-file-name img INSTALL-IMAGE-DIR)))
       (princ (format "%s -> %s\n" src dest))
       (unless just-print
         (unless (file-exists-p src)
           (error "%s not exists." src))
         (copy-file src dest t)
         (set-file-modes dest ?\644))))
   (directory-files "./images" nil "\\.xpm$")))

(defun fsvn-make-test ()
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

(defvar single-file-done nil)

(defun fsvn-make-single-file ()
  (let ((tmpfile "fsvn.el.tmp"))
    (setq single-file-done (cons "fsvn.el" nil))
    (with-temp-buffer
      (insert-file-contents "fsvn.el")
      (single-file-parse-required)
      (goto-char (point-min))
      (unless (re-search-forward "(provide 'fsvn)" nil t)
        (error "Unable find provide"))
      (forward-line -1)
      (mapc
       (lambda (m)
         (single-file-file m (current-buffer)))
       ALL-MODULES)
      (single-file-remove-duplicated-top-form)
      (single-file-remove-blank-page)
      (single-file-remove-blank-lines)
      (write-region (point-min) (point-max) tmpfile nil 'no-msg))))

(defun single-file-remove-blank-page ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\([\n]+\\)+" nil t)
      (replace-match "" nil nil nil 0))))

(defun single-file-remove-duplicated-top-form ()
  (save-excursion
    (goto-char (point-min))
    (let ((prev (point-min))
          forms form)
      (while (setq form
                   (condition-case err
                       (read (current-buffer))
                     (end-of-file nil)))
        (if (member form forms)
            (delete-region prev (point))
          (setq forms (cons form forms)))
        (setq prev (line-beginning-position 2))))))

(defun single-file-remove-blank-lines ()
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
        (cond
         ((not (looking-at "\n"))
          (setq count 0))
         ((= count 1)
          (delete-region (point) (line-beginning-position 2))
          (forward-line -1))
         (t
          (setq count (1+ count))))
        (forward-line 1)))))

(defun single-file-parse-required ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*(require '\\(fsvn-[^)]+\\))" nil t)
      (let ((module (match-string 1)))
        (replace-match "")
        (single-file-file (concat module ".el") (current-buffer))))))

(defun single-file-remove-provide ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*(provide '.*" nil t)
      (replace-match ""))))

(defun single-file-remove-comments ()
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^;;; Code.*" nil t)
      (error "Unable find header"))
    (forward-line 1)
    (delete-region (point-min) (point))
    (unless (re-search-forward "^;;; .*\\.el ends here$" nil t)
      (error "Unable find footer"))
    (forward-line 0)
    (delete-region (point) (point-max))))

(defun single-file-file (file buffer)
  (unless (member file single-file-done)
    (with-temp-buffer
      (insert-file-contents file)
      (single-file-parse-required)
      (single-file-remove-provide)
      (single-file-remove-comments)
      (append-to-buffer buffer (point-min) (point-max)))
    (setq single-file-done (cons file single-file-done))))

;;; fsvn-make.el ends here
