;;; fsvn-proc.el --- process utility for fsvn


;;; History:
;; 

;;; Commentary:
;; 

(require 'fsvn-deps)



(defvar process-environment)
(defvar default-buffer-file-coding-system)



(defvar fsvn-process-environment-lang nil)
(defmacro fsvn-process-environment (&rest form)
  `(let ((process-environment (copy-sequence process-environment)))
     (setenv "LC_MESSAGES" (or fsvn-process-environment-lang "C"))
     ,@form))

(defun fsvn-process-coding-system (args)
  (catch 'found
    (when (and args (stringp (car args)))
      (when (member (car args) '("diff" "blame"))
	;; get guessed coding-system if arg file is local
	(throw 'found (fsvn-guess-file-contents-coding-system (cdr args)))))
    (mapc
     (lambda (arg)
       (when (string= arg "--xml")
	 (throw 'found fsvn-svn-common-coding-system)))
     (fsvn-flatten-command-args args))
    nil))

(defun fsvn-start-process (buffer &rest args)
  (fsvn-process-environment
   (let ((coding-system-for-read (fsvn-process-coding-system args))
	 (real-args (fsvn-flatten-command-args args)))
     (fsvn-debug real-args)
     (apply 'start-process "fsvn" buffer fsvn-svn-command real-args))))

(defun fsvn-start-command (subcommand buffer &rest args)
  (apply 'fsvn-start-process buffer subcommand args))

(defun fsvn-start-command-display (subcommand buffer &rest args)
  (let ((commandline (concat (fsvn-build-command-string subcommand args) "\n\n"))
	proc)
    (fsvn-insert-string-to-buffer commandline buffer)
    (setq proc (fsvn-start-command subcommand buffer args))
    proc))

(defun fsvn-call-process (buffer &rest args)
  "Execute `call-process' with variable `fsvn-svn-command'.
This is synchronous call, so cannot handle password prompt. Append --non-interactive arg
explicitly in calling function.
"
  (fsvn-process-environment
   (let ((coding-system-for-read (fsvn-process-coding-system args))
	 (real-args (fsvn-flatten-command-args args)))
     (when (and (bufferp buffer) (> (buffer-size buffer) 0))
       (with-current-buffer buffer
	 (goto-char (point-max))))
     (fsvn-debug real-args)
     (prog1
	 (apply 'call-process fsvn-svn-command nil buffer nil real-args)
       (fsvn-debug buffer)))))

(defun fsvn-call-command (subcommand buffer &rest args)
  (apply 'fsvn-call-process buffer subcommand (fsvn-command-append-argument subcommand args)))

(defun fsvn-call-command-display (subcommand buffer &rest args)
  "`call-process' and insert executed command line top of buffer."
  (let ((commandline (concat (fsvn-build-command-string
			      subcommand
			      (fsvn-command-append-argument subcommand args)) "\n\n")))
    (fsvn-insert-string-to-buffer commandline buffer)
    (apply 'fsvn-call-command subcommand buffer args)))

(defun fsvn-call-command-discard (subcommand &rest args)
  "`call-process' and discard executed command output.
If error occur in process (exit status non zero value) then raise error."
  (with-temp-buffer
    (unless (= (apply 'fsvn-call-command subcommand t args) 0)
      (signal 'fsvn-command-error (list subcommand args (buffer-string))))
    t))

(defun fsvn-command-append-argument (subcommand args)
  (if (fsvn-svn-subcommand-accepted-argument subcommand "--non-interactive")
      (cons "--non-interactive" args)
    args))

(defun fsvn-process-add-sentinel (proc sentinel)
  (let ((current (process-sentinel proc))
	new)
    (setq new
	  (if current
	      (fsvn-process-create-actor current sentinel)
	    sentinel))
    (set-process-sentinel proc new)))

(defun fsvn-process-add-filter (proc filter)
  (let ((current (process-filter proc))
	new)
    (setq new
	  (if current
	      (fsvn-process-create-actor current filter)
	    filter))
    (set-process-filter proc new)))

(defun fsvn-process-create-actor (current added)
  `(lambda (proc event)
     (mapc
      (lambda (actor)
	(funcall actor proc event))
      '(,current ,added))))

(defmacro fsvn-async-let (varlist &rest body)
  "Asynchronous process execute sequentially.
BODY each form that return process object then stop executing BODY.
Execute remain of BODY if process exited normally in process-sentinel.
Like `let' binding, varlist binded while executing BODY."
  `(let ,varlist
     (let (fsvn-var-list)
       (mapc
	(lambda (x)
	  (setq fsvn-var-list
		(cons
		 (cond
		  ((consp x)
		   (car x))
		  ((symbolp x)
		   x)
		  (t
		   (error "Unknown type")))
		 fsvn-var-list)))
	',varlist)
       (fsvn-async-executor ',body fsvn-var-list))))

(defun fsvn-async-create-wrapper (fsvn-original-actor fsvn-var-alist fsvn-sentinel-p)
  "Create process sentinel/filter FSVN-ORIGINAL-ACTOR that executed in FSVN-VAR-ALIST"
  `(lambda (fsvn-async-proc fsvn-async-event)
     (let ,fsvn-var-alist
       ,(if fsvn-original-actor
	    `(,fsvn-original-actor fsvn-async-proc fsvn-async-event)
	  `(with-current-buffer (process-buffer fsvn-async-proc)
	     (save-excursion
	       (goto-char (point-max))
	       (insert fsvn-async-event))))
       ,(when fsvn-sentinel-p
	  `(when (= (process-exit-status fsvn-async-proc) 0)
	     (with-current-buffer (process-buffer fsvn-async-proc)
	       (fsvn-async-executor 
		(process-get fsvn-async-proc 'fsvn-async-remain-forms)
		',(mapcar 'car fsvn-var-alist))))))))

(defun fsvn-async-executor (fsvn-exec-list fsvn-var-list)
  (let (fsvn-ret-return fsvn-exec-form)
    (while fsvn-exec-list
      (setq fsvn-exec-form (car fsvn-exec-list))
      (setq fsvn-exec-list (cdr fsvn-exec-list))
      (setq fsvn-ret-return (eval fsvn-exec-form))
      (when (processp fsvn-ret-return)
	(let ((fsvn-var-alist
	       (mapcar
		(lambda (var)
		  (cons var (cons (list 'quote (eval var)) nil)))
		fsvn-var-list)))
	  (process-put fsvn-ret-return 'fsvn-async-remain-forms fsvn-exec-list)
	  (set-process-sentinel fsvn-ret-return 
				(fsvn-async-create-wrapper (process-sentinel fsvn-ret-return) fsvn-var-alist t))
	  (set-process-filter fsvn-ret-return 
			      (fsvn-async-create-wrapper (process-filter fsvn-ret-return) fsvn-var-alist nil)))
	(setq fsvn-exec-list nil)))
    fsvn-ret-return))

(defmacro fsvn-process-event-handler (proc event &rest form)
  `(with-current-buffer (process-buffer ,proc)
     (save-excursion
       ,@form)))

(defmacro fsvn-process-exit-handler (proc event &rest form)
  `(when (eq (process-status proc) 'exit)
     (fsvn-process-event-handler proc event
       ,@form)))

(defun fsvn-flatten-command-args (list)
  (let (ret)
    (mapc
     (lambda (x)
       (cond
	((null x))
	((listp x)
	 (setq ret (nconc (nreverse (fsvn-flatten-command-args x)) ret)))
	((stringp x)
	 (cond
	  ((fsvn-url-repository-p x)
	   (setq ret (cons (fsvn-url-encode-string x) ret)))
	  ((string-match fsvn-diff-subcommand-arg-regexp x)
	   ;; for `diff'
	   (let ((arg (match-string 1 x))
		 (url (match-string 2 x)))
	     (setq ret (cons (format "--%s=%s" arg (fsvn-url-encode-string url)) ret))))
	  (t
	   (setq ret (cons x ret)))))
	((numberp x)
	 (setq ret (cons (number-to-string x) ret)))
	(t
	 (error "Assertion fail"))))
     list)
    (nreverse ret)))

(defun fsvn-build-command-string (subcommand &rest args)
  (let ((real-args (fsvn-flatten-command-args args)))
    (mapconcat
     'identity
     (append (list fsvn-svn-command subcommand) real-args)
     " ")))

(defun fsvn-guess-file-contents-coding-system (args)
  (let (ignore)
    (catch 'guessed
      (mapc
       (lambda (arg)
	 (cond
	  (ignore
	   (setq ignore nil))
	  ((string-match fsvn-diff-subcommand-arg-regexp arg)
	   (let ((url (match-string 2 arg))
		 cs magic-name)
	     (when (fsvn-url-repository-p url)
	       (when (setq cs (fsvn-config-repository-default-coding-system url))
		 (throw 'guessed cs))
	       (setq magic-name (fsvn-magic-create-name url))
	       (unless (file-directory-p magic-name)
		 (fsvn-file-guessed-coding-system magic-name)))))
	  ((string-match "^--[a-zA-Z]" arg)
	   (setq ignore (assoc arg '("--targets" "--file"))))
	  ((not (fsvn-url-local-p arg)))
	  (t
	   (let ((file (fsvn-magic-create-name arg)))
	     (unless (file-directory-p file)
	       (throw 'guessed (fsvn-file-guessed-coding-system file)))))))
       (fsvn-flatten-command-args args))
      default-buffer-file-coding-system)))



(put 'fsvn-process-event-handler 'lisp-indent-function 2)
(put 'fsvn-process-exit-handler 'lisp-indent-function 2)
(put 'fsvn-async-let 'lisp-indent-function 1)



(provide 'fsvn-proc)

;;; fsvn-proc.el ends here
