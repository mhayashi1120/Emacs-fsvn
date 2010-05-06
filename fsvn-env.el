;;; fsvn-env.el --- No dependent utility for fsvn


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:
;;



(require 'dired)



(defconst fsvn-space-char ?\040)

(defcustom fsvn-home-directory
  (expand-file-name "~/.fsvn/" )
  "*Directory of this package.  Must be set before load this file."
  :group 'fsvn
  :type  'directory)

(defun fsvn-mapitem (function sequence)
  "Like `mapcar' but return only non-nil values.
Argument FUNCTION see `mapcar'.
Argument SEQUENCE see `mapcar'."
  (let (ret value)
    (mapc
     (lambda (x)
       (when (setq value (funcall function x))
	 (setq ret (cons value ret))))
     sequence)
    (nreverse ret)))

(defmacro fsvn-loop (loop-count &rest form)
  "Execute FORM LOOP-COUNT times. LOOP-IDX is bound in FORM."
  `(let ((LOOP-IDX 0))
     (while (< LOOP-IDX ,loop-count)
       ,@form
       (setq LOOP-IDX (1+ LOOP-IDX)))))

(defmacro fsvn-swap (val1 val2)
  `(let (TMP)
     (setq TMP ,val2)
     (setq ,val2 ,val1)
     (setq ,val1 TMP)))

(defmacro fsvn-save-window-only (window &rest form)
  "Save selected WINDOW, not contain point.
Optional argument FORM evaluate Lisp form."
  `(let ((RETURN-WINDOW (get-buffer-window (current-buffer))))
     (unwind-protect
	 (progn
	   (select-window ,window)
	   ,@form)
       (when (window-live-p RETURN-WINDOW) (select-window RETURN-WINDOW)))))

(defun fsvn-cycle-next (list item)
  (let ((found (member item list)))
    (cond
     ((null found)
      (error "Non member element %s" item))
     ((cadr found)
      (cadr found))
     (t
      (car list)))))

(defun fsvn-forward-bytes (bytes)
  (let ((count 0)
	c)
    (while (< count bytes)
      (setq c (char-after))
      (setq count (+ (length (encode-coding-char c buffer-file-coding-system)) count))
      (forward-char 1))
    (unless (= count bytes)
      ;; or unrecognized format.
      (error "Assertion failed (coding-system problem?)"))
    (point)))

(defun fsvn-number-sequence (min max)
  (let ((i min)
	ret)
    (while (<= i max)
      (setq ret (cons i ret))
      (setq i (1+ i)))
    (nreverse ret)))

;; string,text,regexp utility

;;FIXME too heavy?
(defun fsvn-string-unibyte-only-p (string)
  (let ((un-str (string-make-unibyte string)))
    (when (string= un-str string)
      un-str)))

(defun fsvn-string-put-property (string property value)
  (put-text-property 0 (length string) property value string)
  string)

(defmacro fsvn-string-pad (string maxlen &rest form)
  `(let ((LEN (length string)))
     (if (> LEN maxlen)
	 (substring string 0 maxlen)
       ,@form)))

(defun fsvn-string-rpad (string maxlen &optional char)
  (fsvn-string-pad string maxlen (concat string (make-string (- maxlen LEN) (or char fsvn-space-char)))))

(defun fsvn-string-lpad (string maxlen &optional char)
  (fsvn-string-pad string maxlen (concat (make-string (- maxlen LEN) (or char fsvn-space-char)) string)))

(defun fsvn-string-rtrim (string maxlen)
  (let ((len (length string)))
    (if (> len maxlen)
	(substring string 0 maxlen)
      string)))

(defun fsvn-string-single-line (string &optional max)
  (let ((ret (replace-regexp-in-string "\n" " " string)))
    (if (and max (> (length ret) max))
	(substring ret 0 max)
      ret)))

(defun fsvn-string-rm-lspace (string)
  (replace-regexp-in-string "^[ \t\n]+" "" string))

(defun fsvn-string-rm-rspace (string)
  (replace-regexp-in-string "[ \t\n]+$" "" string))

(defun fsvn-string-rm-space (string)
  (replace-regexp-in-string "\\(?:^[ \t\n]+\\|[ \t\n]+$\\)" "" string))

(defun fsvn-string-force-number (string &optional default-value)
  "Return string as number, return zero if failed convert."
  (unless default-value
    (setq default-value 0))
  (condition-case err
      (string-to-number string)
    (error default-value)))

(defun fsvn-string-line-to-list (string &optional all)
  (with-temp-buffer
    (insert string)
    (fsvn-text-buffer-line-as-list all)))

(defun fsvn-string-convert-cs (string from-cs to-cs)
  (if (eq from-cs to-cs)
      (copy-sequence string)
    (decode-coding-string
     (encode-coding-string string from-cs)
     to-cs)))

(defun fsvn-string-balanced-quoting-p (string)
  (condition-case err
      (with-temp-buffer
	(insert string)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-syntax-forward "^\"")
	  (goto-char (or (scan-sexps (point) 1)
			 (point-max))))
	t)
    (scan-error nil)))

(if (fboundp 'assoc-string)
    (defalias 'fsvn-string-assoc 'assoc-string)
  (defun fsvn-string-assoc (key list &optional case-fold)
    (assoc key list)))

;;FIXME or some utility supplied? and not concern about syntax-table
(defun fsvn-tr1:wregex->regexp (wregex)
  ""
  (let ((lst (string-to-list wregex))
	tmp c n)
    (while lst
      (setq c (car lst))
      (setq n (cadr lst))
      (setq lst (cdr lst))
      (setq tmp
	    (cons
	     (cond
	      ((eq ?\\ c)
	       (cond
		((eq ?d n) (setq lst (cdr lst)) "[0-9]")
		((eq ?D n) (setq lst (cdr lst)) "[^0-9]")
		((eq ?w n) (setq lst (cdr lst)) "\\w")
		((eq ?W n) (setq lst (cdr lst)) "\\W")
		((eq ?s n) (setq lst (cdr lst)) "\\s ")
		((eq ?\) n) (setq lst (cdr lst)) ")")
		((eq ?\( n) (setq lst (cdr lst)) "(")
		(t  (concat "\\" (char-to-string n)))))
	      ((eq ?\( c) "\\(")
	      ((eq ?\) c) "\\)")
	      ((eq ?\| c) "\\|")
	      (t
	       (char-to-string c)))
	     tmp)))
    (apply 'concat (nreverse tmp))))



(defun fsvn-regexp-at (regexp)
  "Wrapper of `looking-at'"
  (when (looking-at regexp)
    (cons nil (match-data))))

(defun fsvn-regexp-match (regexp string &optional start)
  "Wrapper of `string-match'."
  (when (string-match regexp string start)
    (cons string (match-data))))

(defun fsvn-regexp-matched (matched-object subexp)
  "Wrapper of `match-string'."
  (let ((string (car matched-object))
	(matched (cdr matched-object)))
    (save-match-data
      (set-match-data matched)
      (match-string subexp string))))



(defun fsvn-member-regexp (regexp list)
  (catch 'match
    (while list
      (when (and (stringp (car list)) (string-match regexp (car list)))
	(throw 'match list))
      (setq list (cdr list)))))

(defun fsvn-member-startswith (start-string list)
  (fsvn-member-regexp (concat "^" (regexp-quote start-string)) list))

(defun fsvn-any-startswith (list string)
  "STRING startwith any LIST item."
  (catch 'match
    (while list
      (when (string-match (concat "^" (regexp-quote (car list))) string)
	(throw 'match list))
      (setq list (cdr list)))))

(defun fsvn-split-list (elt list)
  (let ((after list)
	before)
    (catch 'matched
      (while after
	(when (equal elt (car after))
	  (throw 'matched (cons (nreverse before) after)))
	(setq before (cons (car after) before))
	(setq after (cdr after)))
      (cons (nreverse before) nil))))



(defun fsvn-month-max-day (year month)
  (let (next-year next-month next-first last-day)
    (if (= month 12)
	(setq next-month 1
	      next-year (1+ year))
      (setq next-month (1+ month)
	    next-year year))
    (setq next-first (float-time (encode-time 0 0 0 1 next-month next-year)))
    (setq last-day (seconds-to-time (1- next-first)))
    (string-to-number (format-time-string "%d" last-day))))



(defun fsvn-lisp-save (value file)
  (let* ((fullname file)
	 lisp)
    (with-temp-buffer
      (erase-buffer)
      (setq lisp (copy-sequence value))
      (pp lisp (current-buffer))
      (let ((coding-system-for-write 'utf-8))
	(write-region (point-min) (point-max) fullname nil 'no-msg)))))

(defun fsvn-lisp-load (file)
  (let* ((fullname file)
	 lisp)
    (with-temp-buffer
      (setq lisp
	    (condition-case nil
		(progn
		  (let ((coding-system-for-read 'utf-8))
		    (insert-file-contents fullname))
		  (read (current-buffer)))
	      (error ()))))
    lisp))



(defun fsvn-defstruct-keyword-number-pair (spec)
  (let ((i 0))
    (mapcar
     (lambda (sym)
       (prog1
	   (cons (intern (concat ":" (symbol-name sym))) i)
	 (setq i (1+ i))))
     spec)))

(defmacro fsvn-defstruct (type &rest spec)
  `(progn
     (fsvn-defstruct-constructor ,type ,@spec)
     (fsvn-defstruct-s/getter ,type ,@spec)))

(defmacro fsvn-defstruct-constructor (type &rest spec)
  `(let ((SYM (quote ,(intern (concat (format "fsvn-struct-%s-make" (symbol-name type)))))))
     (fset SYM
	   (lambda (&rest args)
	     (let* ((alist (quote ,(fsvn-defstruct-keyword-number-pair spec)))
		    (struct (make-list (length alist) nil))
		    key val key-num)
	       (while args
		 (setq key  (car args))
		 (setq args (cdr args))
		 (setq val  (car args))
		 (setq args (cdr args))
		 (unless (keywordp key)
		   (error "'%s' is not a keyword" key))
		 (setq key-num (assoc key alist))
		 (if key-num
		     (setcar (nthcdr (cdr key-num) struct) val)
		   (error "'%s' is unknown" key)))
	       struct)))))

(defmacro fsvn-defstruct-s/getter (type &rest spec)
  `(let* ((TYPE-NAME (symbol-name (quote ,type)))
	  (KEYS (quote ,spec))
	  (LEN (length KEYS))
	  (INDEX 0)
	  member-name setter getter)
     (while (< INDEX LEN)
       (setq member-name (symbol-name (car KEYS)))
       (setq setter (intern (format "fsvn-struct-%s-set-%s" TYPE-NAME member-name)))
       (fset setter `(lambda (struct value) (setcar (nthcdr ,INDEX struct) value) struct))
       (setq getter (intern (format "fsvn-struct-%s-get-%s" TYPE-NAME member-name)))
       (fset getter `(lambda (struct) (nth ,INDEX struct)))
       (setq KEYS (cdr KEYS))
       (setq INDEX (1+ INDEX)))))



(defun fsvn-coding-system-name (cs)
  ;; FIXME almost all case ok but not correct.
  (upcase (symbol-name (coding-system-get (coding-system-base cs) 'mime-charset))))

(defvar fsvn-file-guessed-coding-system-threshold 30000)
(defun fsvn-file-guessed-coding-system (file)
  (let (cs)
    (setq cs (car (find-operation-coding-system 'insert-file-contents file)))
    (when (or (null cs) (eq cs 'undecided))
      (with-temp-buffer
	(insert-file-contents file nil 0 fsvn-file-guessed-coding-system-threshold)
	(setq cs buffer-file-coding-system)))
    cs))

(defun fsvn-wc-files-only-non-recursive-p (files)
  (catch 'non-file
    (mapc
     (lambda (file)
       (when (and (fsvn-file-exact-directory-p file)
		  (> (length (fsvn-directory-files file)) 0))
	 (throw 'non-file nil)))
     files)
    t))

(defun fsvn-file-directly-under-p (dir file)
  (string= (file-name-as-directory dir)
	   (file-name-directory (directory-file-name file))))

(defun fsvn-directory-files (directory &optional match)
  (directory-files directory nil (or match dired-re-no-dot)))

(defun fsvn-make-temp-file ()
  (let ((temporary-file-directory (fsvn-temp-directory)))
    (make-temp-file "fsvn")))

(defun fsvn-make-temp-directory (&optional prefix)
  (let ((temporary-file-directory (fsvn-temp-directory)))
    (make-temp-file "fsvn" t)))

(defun fsvn-make-temp-filename (file)
  (let ((i 1)
	name)
    (while (file-exists-p (setq name (format "%s.%d" file i)))
      (setq i (1+ i)))
    name))

(defun fsvn-cleanup-temp-directory ()
  "Cleanup temporary directory when load this file."
  (let ((using (fsvn-process-using-temp-files)))
    (mapc
     (lambda (file)
       (unless (fsvn-file-member file using)
	 (if (fsvn-file-exact-directory-p file)
	     (fsvn-delete-directory file)
	   (delete-file file))))
     (directory-files (fsvn-temp-directory) t dired-re-no-dot))))

(defun fsvn-process-using-temp-files ()
  (let (ret)
    (mapc
     (lambda (p)
       (when (string-match "^fsvn" (process-name p))
	 (mapc
	  (lambda (string)
	    (when (and (file-name-absolute-p string)
		       (file-exists-p string))
	      (setq ret (cons string ret))))
	  (process-command p))))
     (process-list))
    (nreverse ret)))

(defconst fsvn-temp-buffer-prefix " *Fsvn ")
(defvar fsvn-temp-buffer-p nil
  "Set t if buffer was made `fsvn-make-temp-buffer'.")

(defun fsvn-make-temp-buffer ()
  (let (tmp ret)
    (while (get-buffer (setq tmp (make-temp-name fsvn-temp-buffer-prefix))))
    (setq ret (generate-new-buffer tmp))
    (with-current-buffer ret
      (set (make-local-variable 'fsvn-temp-buffer-p) t))
    ret))

(defmacro fsvn-cleanup-buffer (each-buffer-condition)
  "`kill-buffer' if EACH-BUFFER-CONDITION return non-null value."
  `(let ((count 0))
     (save-excursion
       (mapc
	(lambda (b)
	  (set-buffer b)
	  (when (and ,each-buffer-condition
		     (or (null (get-buffer-process b))
			 (eq (process-status (get-buffer-process b)) 'exit)))
	    (kill-buffer (current-buffer))
	    (setq count (1+ count))))
	(buffer-list)))
     count))

(defun fsvn-files-unsaved-buffers (files)
  (let (ret tmp)
    (mapc
     (lambda (file)
       (cond
	((fsvn-file-exact-directory-p file)
	 (when (setq tmp (fsvn-directory-unsaved-buffers file))
	   (setq ret (nconc ret tmp))))
	(t
	 (let ((buffer (get-file-buffer file)))
	   (when (and buffer
		      (buffer-modified-p buffer))
	     (setq ret (cons buffer ret)))))))
     files)
    ret))

(defun fsvn-directory-unsaved-buffers (directory)
  (let ((regex (concat "^" (regexp-quote (directory-file-name directory)) "/"))
	ret)
    (mapc
     (lambda (buffer)
       (let ((file (buffer-file-name buffer)))
	 (when (and file
		    (string-match regex (fsvn-expand-file file))
		    (buffer-modified-p buffer))
	   (setq ret (cons buffer ret)))))
     (buffer-list))
    ret))


;; config directories

(defconst fsvn-temp-directory-dirs
  '("LogMessage" "workspace" "temp" "ediff" "backup" "auto-save-files" "cache"))

(defun fsvn-backup-file-directory ()
  "Backup directory."
  (fsvn-expand-file "backup" fsvn-home-directory))

(defun fsvn-auto-save-file-directory ()
  "Magic auto-save directory."
  (fsvn-expand-file "auto-save-files" fsvn-home-directory))

(defun fsvn-temp-directory ()
  "Temp directory."
  (fsvn-expand-file "temp" fsvn-home-directory))

(defun fsvn-logmessage-directory ()
  "Log message directory."
  (fsvn-expand-file "LogMessage" fsvn-home-directory))

(defun fsvn-workspace-directory ()
  "Temporary workspace directory."
  (fsvn-expand-file "workspace" fsvn-home-directory))

(defun fsvn-ediff-directory ()
  "Temporary `ediff' directory."
  (fsvn-expand-file "ediff" fsvn-home-directory))

(defun fsvn-cache-directory ()
  "Some data cached directory."
  (fsvn-expand-file "cache" fsvn-home-directory))

(defun fsvn-cache-command-directory ()
  (fsvn-expand-file "command" (fsvn-cache-directory)))

(defun fsvn-cache-repository-directory ()
  (fsvn-expand-file "repository" (fsvn-cache-directory)))



(defun fsvn-initialize-loading ()
  (fsvn-set-version)
  (unless (file-directory-p fsvn-home-directory)
    (make-directory fsvn-home-directory t))
  (mapc
   (lambda (dir)
     (let ((dirname (fsvn-expand-file dir fsvn-home-directory)))
       (unless (file-directory-p dirname)
	 (make-directory dirname))))
   fsvn-temp-directory-dirs)
  (fsvn-cleanup-temp-directory)
  (fsvn-build-subcommand)
  (fsvn-toggle-feature t 'no-msg))



(defun fsvn-insert-string-to-buffer (string buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (insert string))))

(defun fsvn-text-buffer-line-as-list (&optional all)
  (let ((regexp (if all "^.*$" "^.+$"))
	ret )
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(when (looking-at regexp)
	  (setq ret (cons (match-string-no-properties 0) ret)))
	(forward-line 1)))
    (nreverse ret)))

(defun fsvn-get-file-contents (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))



(put 'fsvn-loop 'lisp-indent-function 1)
(put 'fsvn-defstruct 'lisp-indent-function 1)
(put 'fsvn-save-window-only 'lisp-indent-function 1)

(provide 'fsvn-env)

;;; fsvn-env.el ends here
