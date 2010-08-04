;;; -*- Coding: utf-8 -*-
;;; mw32cmp.el --- Meadow compatibility for NTEmacs.
;;
;; 1. run following command in expanded directory.
;; svn export http://svn.meadowy.org/Meadow/trunk/lisp/international/mw32mci.el ./mw32mci.el
;; svn export http://svn.meadowy.org/Meadow/trunk/lisp/international/mw32misc.el ./mw32misc.el
;; svn export http://svn.meadowy.org/Meadow/trunk/lisp/international/mw32script.el ./mw32script.el
;;
;; 2. run following command in expanded directory.
;; patch < mw32el-ntemacs.patch
;;

;;; History:
;; 

;;; Commentary:
;;   
;;  Implemented by Elisp + reg.exe

;; TODO
;;  code cleaning
;;  Investigate Meadow's all extended function.

;;; Code:



(require 'mw32script)
(mw32script-make-pathext-regexp)
(load "mw32misc")



(defconst mw32cmp-function-alist
  '(
    (mw32-registry-get . mw32cmp-pseudo-registry-get)
    (mw32-registry-create-key . mw32cmp-pseudo-registry-create-key)
    (mw32-registry-delete-key . mw32cmp-pseudo-registry-delete-key)
    (mw32-registry-delete-value . mw32cmp-pseudo-registry-delete-value)
    (mw32-registry-list-keys . mw32cmp-pseudo-registry-list-keys)
    (mw32-registry-list-values . mw32cmp-pseudo-registry-list-values)
    (mw32-registry-set . mw32cmp-pseudo-registry-set)
    (mw32-registry-words-to-integer . mw32cmp-pseudo-registry-words-to-integer)
    (dos-to-unix-filename . mw32cmp-pseudo-dos-to-unix-filename)
    (unix-to-dos-filename . mw32cmp-pseudo-unix-to-dos-filename)
    (unix-to-dos-argument . mw32cmp-pseudo-unix-to-dos-argument)))

;; non `defun' for testing in meadow
(unless (featurep 'meadow)
  (mapc
   (lambda (x)
     (defalias (car x) (cdr x)))
   mw32cmp-function-alist))



;; external definitions
(defvar default-terminal-coding-system)

(defconst mw32cmp-registry-type-alist
  '(
    (registry-binary                     "REG_BINARY"                     mw32cmp-registry-parse-binary)
    (registry-dword                      "REG_DWORD"                      mw32cmp-registry-parse-dword)
    (registry-dword-little-endian        "REG_DWORD"                      mw32cmp-registry-parse-dword-little-endian)
    (registry-dword-big-endian           "REG_DWORD_BIG_ENDIAN"           mw32cmp-registry-parse-dword-big-endian)
    (registry-expand-sz                  "REG_EXPAND_SZ"                  mw32cmp-registry-parse-expand-sz)
    (registry-link                       "REG_LINK"                       mw32cmp-registry-parse-link)

    (registry-multi-sz                   "REG_MULTI_SZ"                   mw32cmp-registry-parse-multi-sz)
    ;;todo reg.exe return reg_none but regedit display this key as reg_dword
    (registry-qword                       "REG_NONE"                       mw32cmp-registry-parse-qword)
;;    (registry-none                       "REG_NONE"                       mw32cmp-registry-parse-none)
    (registry-qword                      "REG_QWORD"                      mw32cmp-registry-parse-qword)
    (registry-qword-little-endian        "REG_QWORD"                      mw32cmp-registry-parse-qword-little-endian)
    (registry-resource-list              "REG_RESOURCE_LIST"              mw32cmp-registry-parse-resource-list)
    (registry-sz                         "REG_SZ"                         mw32cmp-registry-parse-sz)
    (registry-full-resource-descriptor   "REG_FULL_RESOURCE_DESCRIPTOR"   mw32cmp-registry-parse-full-resource-descriptor)
    (registry-resource-requirements-list "REG_RESOURCE_REQUIREMENTS_LIST" mw32cmp-registry-parse-resource-requirements-list)
    ))

(defvar mw32cmp-pseudo-registry-default-name
  (let ((locale (w32-get-current-locale-id)))
    (cond
     ((= locale 1041)
      "<名前なし>")
     (t
      (error "Not supported locale"))))
  "reg.exe specification cannot determine key default value.")

(defmacro mw32cmp-pseudo-registry-narrow (key &rest form)
  `(let ((MW32CMP-KEY-REGEX (concat "^" (regexp-quote ,key)))
	 MW32CMP-START MW32CMP-END)
     (goto-char (point-min))
     (unless (re-search-forward (concat MW32CMP-KEY-REGEX "$") nil t)
       (error "failed searcy key %s" key))
     (setq MW32CMP-START (1+ (point)))
     (setq MW32CMP-END
	   (or (and (re-search-forward MW32CMP-KEY-REGEX nil t)
		    (line-beginning-position))
	       (point-max)))
     (narrow-to-region MW32CMP-START MW32CMP-END)
     (goto-char (point-min))
     ,@form))

(put 'mw32cmp-pseudo-registry-narrow 'lisp-indent-function 1)

;;todo registry key and value contains space?
;;todo REG_SZ like string must be contains in this regexp.
(defconst mw32cmp-pseudo-registry-value-regexp
  "^ +\\([^\t]+\\)\t\\([^\t]+\\)\t\\(.*\\)")

(defun mw32cmp-pseudo-registry-check ()
  (unless (executable-find "reg")
    (error "Unable get regisotry.")))

(defun mw32cmp-pseudo-registry-get (key &optional name)
  (mw32cmp-pseudo-registry-check)
  (if name
      (let ((args (list "query" key))
	    (name-regex mw32cmp-pseudo-registry-value-regexp)
	    ret reg)
	(setq args (nconc args (list "/v" name)))
	(with-temp-buffer
	  (unless (= (apply 'call-process "reg" nil t nil args) 0)
	    (signal 'error (list "Cannot open registry key" key)))
	  (mw32cmp-pseudo-registry-narrow key
	    (while (null ret)
	      (when (looking-at name-regex)
		(setq reg (mw32cmp-pseudo-registry-matched-name-object))
		(when (string= (nth 0 reg) name)
		  (setq ret (nthcdr 1 reg))))
	      (forward-line 1)
	      (when (eobp)
		(setq ret t)))))
	(if (eq ret t) nil ret))
    ;; get default value using another process
    (cdr (assoc "" (mw32cmp-pseudo-registry-list-values key t)))))

(defun mw32cmp-pseudo-registry-list-keys (key &optional with-data)
  (mw32cmp-pseudo-registry-check)
  (let (args regexp ret matcher)
    (setq args (list "query" key))
    (setq matcher
	  (if with-data
	      (lambda () 
		(let ((key (match-string 0))
		      (subkey (match-string 1)))
		  (cons subkey (mw32cmp-pseudo-registry-get key))))
	    (lambda () (match-string 1))))
    (with-temp-buffer
      (unless (= (apply 'call-process "reg" nil t nil args) 0)
	(signal 'error (list "Cannot open registry key" key)))
      (setq regexp (concat "^" (regexp-quote key) "\\\\?\\([^\\\\\n]+\\)$"))
      (goto-char (point-min))
      (while (not (eobp))
	(when (looking-at regexp)
	  (setq ret (cons (funcall matcher) ret)))
	(forward-line 1)))
    (nreverse ret)))

(defun mw32cmp-pseudo-registry-list-values (key &optional with-data)
  (mw32cmp-pseudo-registry-check)
  (let (args matcher)
    (setq args (list "query" key))
    (with-temp-buffer
      (unless (= (apply 'call-process "reg" nil t nil args) 0)
	(signal 'error (list "Cannot open registry key" key)))
      (setq matcher
	    (if with-data
		'mw32cmp-pseudo-registry-matched-name-object
	      'mw32cmp-pseudo-registry-matched-name))
      (mw32cmp-pseudo-registry-narrow key
	(let ((name-regex mw32cmp-pseudo-registry-value-regexp)
	      ret)
	  (while (not (eobp))
	    (when (looking-at name-regex)
	      (setq ret (cons (funcall matcher) ret)))
	    (forward-line 1))
	  (nreverse ret))))))

(defun mw32cmp-pseudo-registry-words-to-integer (lword hword &optional hhword hhhword))
(defun mw32cmp-pseudo-registry-create-key (key))
(defun mw32cmp-pseudo-registry-delete-key (key))
(defun mw32cmp-pseudo-registry-delete-value (key name))
(defun mw32cmp-pseudo-registry-set (key name data))

(defsubst mw32cmp-pseudo-registry-matched-name-object ()
  (let ((reg-name (match-string 1))
	(reg-type (match-string 2))
	(reg-value (match-string 3))
	name type value reg-define)
    (setq name (mw32cmp-pseudo-registry-key-name reg-name))
    (setq reg-define (mw32cmp-registry-type-converter reg-type))
    (setq type (nth 0 reg-define))
    (setq value (funcall (nth 2 reg-define) reg-value))
    (cons name (when value (cons value type)))))
  
(defsubst mw32cmp-pseudo-registry-matched-name ()
  (let ((reg-name (match-string 1))
	name)
    (setq name (mw32cmp-pseudo-registry-key-name reg-name))
    name))

(defsubst mw32cmp-pseudo-registry-key-name (reg-name)
  (if (string= reg-name mw32cmp-pseudo-registry-default-name)
      "" 
    reg-name))

(defun mw32cmp-registry-parse-binary (value) 
  (let ((i 0)
	(len (length value))
	hex char list)
    (while (< i len)
      (setq hex (substring value i (+ i 2)))
      (setq char (string-to-number hex 16))
      (setq list (cons char list))
      (setq i (+ i 2)))
    (concat (nreverse list))))

(defun mw32cmp-registry-parse-dword (value) 
  (unless (string-match "^0x\\([0-9a-z]+\\)$" value)
    (signal 'error (list "Not a hex value" value)))
  (let ((hex (match-string 1 value))
	val)
    ;;FIXME quick hack this is not a time value..
    (setq val (seconds-to-time (string-to-number hex 16)))
    (cons (nth 0 val) (nth 1 val))))

(defun mw32cmp-registry-parse-dword-little-endian (value) value)
(defun mw32cmp-registry-parse-dword-big-endian (value) value)
(defun mw32cmp-registry-parse-expand-sz (value) value)
(defun mw32cmp-registry-parse-link (value) value)
(defun mw32cmp-registry-parse-multi-sz (value) 
  (split-string value "\\\\0" t))
(defun mw32cmp-registry-parse-none (value) value)
(defun mw32cmp-registry-parse-qword (value) 
  (let ((i 0)
	(len (length value))
	hhex lhex list)
    (while (< i len)
      ;;todo big-endian/little-endian system dependent value?
      (setq hhex (substring value i (+ i 2)))
      (setq lhex (substring value (+ i 2) (+ i 4)))
      (setq list (cons (string-to-number (concat lhex hhex) 16) list))
      (setq i (+ i 4)))
    list))

(defun mw32cmp-registry-parse-qword-little-endian (value) value)
(defun mw32cmp-registry-parse-resource-list (value) value)
(defun mw32cmp-registry-parse-sz (value)
  ;;FIXME correct variable?
  (encode-coding-string value default-terminal-coding-system))
(defun mw32cmp-registry-parse-full-resource-descriptor (value) value)
(defun mw32cmp-registry-parse-resource-requirements-list (value) value)

(defun mw32cmp-registry-type-converter (type)
  (let* ((alist mw32cmp-registry-type-alist)
	 (defined
	   (catch 'found
	     (mapc
	      (lambda (cell)
		(when (string= (nth 1 cell) type)
		  (throw 'found cell)))
	      alist)
	     nil)))
    (unless defined
      (error "No matched registry type"))
    defined))

(defun mw32cmp-pseudo-dos-to-unix-filename (filename)
  (replace-regexp-in-string "\\\\" "/" filename))

(defun mw32cmp-pseudo-unix-to-dos-filename (file)
  (replace-regexp-in-string "/" "\\\\" file))

(defun mw32cmp-pseudo-unix-to-dos-argument (filename ep h2sp qp s2isp)
  ;;todo
  filename)



;; (defadvice call-process 
;;   (before mw32cmp-call-process-argument-editing 
;; 	  (program &optional infile buffer display &rest args) disable)
;;   (let ((ret (mw32cmp-argument-editing program args)))
;;     (setq program (car ret)
;; 	  args (cdr ret))))

;; (defadvice call-process-region
;;   (before mw32cmp-call-process-region-argument-editing 
;; 	  (start end program &optional delete buffer display &rest args) disable)
;;   (let ((ret (mw32cmp-argument-editing program args)))
;;     (setq program (car ret)
;; 	  args (cdr ret))))

;; (defadvice start-process 
;;   (before mw32cmp-start-process-argument-editing 
;; 	  (name buffer program &rest program-args) disable)
;;   (let ((ret (mw32cmp-argument-editing program program-args)))
;;     (setq program (car ret)
;; 	  program-args (cdr ret))))

;; (defun mw32cmp-argument-editing (program raw-args)
;;   (let (command edited editor args
;; 		edited-command edited-arg)
;;     (if (setq command (executable-find program))
;; 	(progn
;; 	  (setq editor (find-process-argument-editing-function command))
;; 	  (when editor
;; 	    (setq edited (funcall editor (cons command raw-args)))
;; 	    (if (consp edited)
;; 		(progn
;; 		  (setq edited-command (car edited)
;; 			edited-arg  (cdr edited)))
;; 	      ;; FIXME if command contains space
;; 	      (when (string-match "^[ \t]*\"?\\([^ \t\"]+\\)\"?\\(?:[ \t]+\\(.*\\)\\)?" edited)
;; 		(setq edited-command (match-string 1 edited)
;; 		      edited-arg  (match-string 2 edited)))))
;; 	  (cons edited-command (if (listp edited-arg) edited-arg (list edited-arg))))
;;       (cons program raw-args))))

;; (setq default-process-argument-editing-function 'identity)

(provide 'mw32cmp)

;;; mw32cmp.el ends here
