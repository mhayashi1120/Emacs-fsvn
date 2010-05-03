;;; fsvn-minibuf.el --- Read from minibuffer utility for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:
;;



(require 'fsvn-deps)



(defvar minibuffer-local-map)
(defvar last-command)
(defvar this-command)
(defvar quit-flag)
(defvar unread-command-events)



(defun fsvn-read-versioned-directory (&optional prompt)
  (let (dir message-log-max)
    (setq prompt (or prompt "Versioned directory: "))
    (while (and (setq dir (fsvn-read-directory-name prompt))
		(not (fsvn-directory-versioned-p dir)))
      (message "Non versioned directory.")
      (sit-for 1))
    dir))

(defun fsvn-read-file-under-versioned (prompt init-value)
  (let ((init init-value)
	file message-log-max)
    (while (and (setq file (fsvn-expand-file
			    (read-file-name prompt nil nil nil
					    (and init (fsvn-urlrev-filename init)))))
		(not (fsvn-directory-under-versioned-p file)))
      (setq init file)
      (message "Non versioned directory.")
      (sit-for 1))
    file))

(defun fsvn-read-mkdir-directory (dir)
  (if (fsvn-url-repository-p dir)
      (fsvn-completing-read-url "Directory: " (fsvn-url-as-directory dir) t)
    (fsvn-read-directory-name "Directory: " (fsvn-url-as-directory dir) nil nil)))

(defun fsvn-read-directory-name (prompt &optional dir default-dirname mustmatch initial)
  (fsvn-expand-file (read-directory-name prompt dir default-dirname mustmatch initial)))

(defun fsvn-read-file-name (prompt &optional dir default-filename mustmatch initial predicate)
  (setq dir (file-name-as-directory (or dir default-directory)))
  (fsvn-expand-file (read-file-name prompt dir default-filename mustmatch initial predicate)))

(defun fsvn-read-number (prompt &optional default)
  "Read a numeric value in the minibuffer, prompting with PROMPT.
DEFAULT specifies a default value to return if the user just types RET.
The value of DEFAULT is not a number, allow to enter a nil value."
  (let ((n nil))
    (while
	(progn
	  (let ((str (read-from-minibuffer prompt nil nil nil nil
					   (and (numberp default)
						(number-to-string default)))))
	    (condition-case nil
		(setq n (cond
			 ((and default (not (numberp default)) (zerop (length str))) nil)
			 ((zerop (length str)) default)
			 ((stringp str) (read str))))
	      (error nil)))
	  (unless (or (numberp n) (and default (null n)))
	    (message "Please enter a number.")
	    (sit-for 1)
	    t)))
    n))

(defvar fsvn-read-subcommand-history nil)
(defun fsvn-read-subcommand (alist)
  (let ((readed  (completing-read
		  "svn subcommand: "
		  alist
		  nil t nil 'fsvn-read-subcommand-history)))
    (cdr (assoc readed alist))))

(defun fsvn-read-svn-subcommand ()
  (fsvn-read-subcommand fsvn-svn-subcommand-completion-alist))

(defun fsvn-read-svnadmin-subcommand ()
  (fsvn-read-subcommand fsvn-svnadmin-subcommand-completion-alist))

(defvar fsvn-read-propname-history nil)
(defun fsvn-read-propname (file)
  (let (propname)
    (while (null propname)
      (setq propname (completing-read "SVN Property: "
				      (fsvn-propname-completion-alist file)
				      nil nil nil 'fsvn-read-propname-history))
      (unless (fsvn-svn-valid-propname-p propname)
	(let (message-log-max)
	  (message "%s is not valid svn property name." propname)
	  (sit-for 1)
	  (setq propname nil))))
    propname))

(defvar fsvn-read-revprop-history nil)
(defun fsvn-read-revprop ()
  (completing-read "SVN Revprop: "
		   fsvn-revprop-list nil nil nil 'fsvn-read-revprop-history))

(defvar fsvn-read-changelist-history nil)
(defun fsvn-read-changelist-name ()
  (read-from-minibuffer "Changelist Name: " nil nil nil 'fsvn-read-changelist-history))

(defun fsvn-read-resolve-accept-arg ()
  (let ((args (cdr (fsvn-reading-subcommand fsvn-svn-subcommand-arguments-alist "resolve" t)))
	completions ret)
    (unless args
      (error "svn `resolve' not supported."))
    (setq completions (fsvn-subcommand-create-completions '("--accept") nil args))
    (setq ret (completing-read "Resolve `--accept' arg: " completions nil 'no nil nil ))
    (when (string= ret "")
      (error "Accept arg must be selected"))
    ret))

;; this implement cannot treat correctly `@' contains filename
;; (defun fsvn-read-url-with-revision (&optional prompt)
;;   (let* ((urlrev (fsvn-completing-read-urlrev prompt))
;; 	 (url (fsvn-urlrev-url urlrev))
;; 	 (rev (fsvn-completing-read-revision 
;; 	       (format "%s %s Revision: " (or prompt "URL") url)
;; 	       (fsvn-urlrev-revision urlrev))))
;;     (fsvn-url-urlrev url rev)))

(defun fsvn-read-url-with-revision (&optional prompt default-urlrev only-repository)
  (let* ((default-url (and default-urlrev (fsvn-urlrev-url default-urlrev)))
	 (default-rev (and default-urlrev (fsvn-urlrev-revision default-urlrev)))
	 (url (fsvn-completing-read-url prompt default-url only-repository))
	 (rev (fsvn-completing-read-revision 
	       (format "%s %s Revision: " (or prompt "URL") url)
	       default-rev)))
    (fsvn-url-urlrev url rev)))


;; completing read utility

;; todo refactor fsvn-completion-window-show
(defun fsvn-completion-window-control (string collections)
  (let ((list (all-completions string collections)))
    (if (= (length list) 1)
	(fsvn-completion-window-delete)
      (fsvn-completion-window-show list))))

(defun fsvn-completing-file-name (url)
  (when (string-match "/\\([^/]+/?\\)$" url)
    (match-string 1 url)))

(defun fsvn-processing-filename (string)
  (if (string-match "/\\([^/]+\\)$" string)
      (match-string 1 string)
    ""))

(defconst fsvn-completion-buffer " *Fsvn completion*")

(defvar fsvn-completion-saved-configuration nil)
(defvar fsvn-completion-saved-applicant nil)

(defun fsvn-completion-window-show (list)
  (unless fsvn-completion-saved-configuration
    (setq fsvn-completion-saved-configuration (current-window-configuration)))
  (let (win)
    (if (and list (equal fsvn-completion-saved-applicant list)
	     (setq win (get-buffer-window fsvn-completion-buffer)))
	(save-selected-window
	  (select-window win)
	  (condition-case err
	      (scroll-up)
	    (end-of-buffer (progn (goto-char (point-min))))))
      (with-output-to-temp-buffer fsvn-completion-buffer
	(display-completion-list list))
      (setq fsvn-completion-saved-applicant list))))

(defun fsvn-completion-window-delete ()
  (when (and fsvn-completion-saved-configuration
	     (window-configuration-p fsvn-completion-saved-configuration))
    (set-window-configuration fsvn-completion-saved-configuration)
    (setq fsvn-completion-saved-configuration nil
	  fsvn-completion-saved-applicant nil)))


(defun fsvn-completing-create-collection (list)
  (mapcar
   (lambda (x)
     (cons
      (cond
       ((numberp x)
	(number-to-string x))
       ((stringp x)
	x)
       (t
	(error "Not supported")))
      nil))
   list))

(defun fsvn-completing-non-word-class ()
  (concat "^" fsvn-completing-word-class))

(defvar fsvn-revision-read-history nil)

(defvar fsvn-completing-revision-map nil)
(defvar fsvn-completing-word-class " \n\t")

(unless fsvn-completing-revision-map
  (setq fsvn-completing-revision-map
	(let ((map (copy-keymap minibuffer-local-map)))

	  (define-key map "\C-i" 'fsvn-completing-revision)
	  (define-key map " " 'fsvn-completing-revision)

	  map)))

(defun fsvn-completing-read-revision (&optional prompt initial default)
  "Read revision string from minibuffer.
"
  (setq default (or default "HEAD"))
  (catch 'done
    (let ((value (when initial (fsvn-get-revision-string initial)))
	  completions)
      (while t
	(setq value
	      (read-from-minibuffer
	       (or prompt "Revision: ")
	       value
	       fsvn-completing-revision-map nil
	       'fsvn-revision-read-history))
       (setq completions (all-completions value fsvn-revision-string-list))
	(cond
	 ((string= value "")
	  (setq value default))
	 ((> (length completions) 1)
	  (setq value (try-completion value fsvn-revision-string-list)))
	 ((= (length completions) 1)
	  (when (string= (car completions) value)
	    (throw 'done value))
	  (setq value (car completions)))
	 ((or (fsvn-revision-date-p value)
	      (fsvn-revision-number-p value))
	  (throw 'done value)))))))

(defun fsvn-completing-revision ()
  (interactive)
  (let ((value (fsvn-completing-current-value)))
    (cond
     ((string-match "^[0-9]+$" value)
      ;; do nothing
      )
     ((string-match "^{" value)
      ;; do nothing
      )
     (t
      (fsvn-completing-revision-symbol value))
     )))

(defcustom fsvn-completing-max-year 2099
  "*"
  :group 'fsvn
  :type 'integer)

(defun fsvn-completing-revision-symbol (value)
  (let ((comp (try-completion value fsvn-revision-string-list)))
    (cond
     ((eq comp t))
     ((and (stringp comp) (> (length comp) 0))
      (delete-char (- (length value)))
      (insert comp))
     (t
      (fsvn-completion-window-control value fsvn-revision-string-list)))))

(defun fsvn-completing-read-revision-range (current)
  (let* ((from (fsvn-completing-read-revision "Revision From: " (cdr current)))
	 (to (fsvn-completing-read-revision (format "Revision %s -> Revision To: " from) (car current))))
    (cons from to)))

(defun fsvn-completing-current-value ()
  ""
  (let ((point (point)))
    (save-excursion
      (skip-chars-backward (fsvn-completing-non-word-class))
      (buffer-substring (point) point))))



(defvar fsvn-completing-read-repository-history nil)

(defvar fsvn-completing-repository-map nil)
(unless fsvn-completing-repository-map
  (setq fsvn-completing-repository-map
	(let ((map (copy-keymap minibuffer-local-map)))
	  (define-key map "\C-i" 'fsvn-completing-url)
	  (define-key map " " 'fsvn-completing-url)
	  map)))

(defvar fsvn-completing-repository-only-repos nil)

(defun fsvn-completing-read-url (&optional prompt default only-repository)
  (fsvn-completing-read-url-initialize)
  (setq fsvn-completing-repository-only-repos only-repository)
  (let ((url-string))
    (while (or (null url-string)
	       (and (not (fsvn-url-local-p url-string))
		    (not (fsvn-url-repository-p url-string))))
      (setq url-string (read-from-minibuffer
		 (or prompt "URL: ")
		 (or url-string default) fsvn-completing-repository-map nil
		 'fsvn-completing-read-repository-history)))
    (fsvn-expand-url url-string)))

(defun fsvn-completing-read-urlrev (&optional prompt default only-repository)
  (fsvn-url-string-to-urlrev (fsvn-completing-read-url prompt default only-repository)))

(defun fsvn-completing-read-url-initialize ()
  (setq fsvn-completion-repository-cache nil))

(defun fsvn-completing-url ()
  (interactive)
  ;;todo contains space char filename
  (skip-chars-forward "^/ \n\t")
  (let ((value (fsvn-completing-current-value)))
    (cond
     ((or (string= value "") (try-completion value fsvn-svn-url-scheme-list))
      (fsvn-completing-repos-scheme))
     ((or fsvn-completing-repository-only-repos
	  (fsvn-completing-url-repository-p value))
      (fsvn-completing-repository-url))
     ((fsvn-url-local-p value)
      (fsvn-completing-local-file))
     (t
      (fsvn-completing-non-applicant)))))

(defun fsvn-completing-repos-scheme ()
  (let* ((contents (fsvn-completing-current-value))
	 comp)
    (setq comp (try-completion contents fsvn-svn-url-scheme-segment-list))
    (unless (string= comp "")
      (fsvn-completing-url-clear-segment)
      (if (stringp comp)
	  (insert comp)
	(insert contents)))
    (fsvn-completion-window-control contents fsvn-svn-url-scheme-list)))

(defun fsvn-completing-repository-url ()
  (let* ((value (fsvn-completing-current-value)))
    ;; todo when file:// then complete local directory.
    (cond
     ((fsvn-completing-url-local-repository-p value)
      (fsvn-completing-local-file))
     ((eq (char-before) ?:)
      (fsvn-completing-repos-root))
     ((fsvn-completing-url-host-segment-p value)
      (fsvn-completing-repos-root))
     (t
      (fsvn-completing-repos-file)))))

(defun fsvn-completing-repos-root ()
  ;;todo gathered in buffers -> file cache. which one.
  (let ((value (fsvn-completing-current-value))
	(applicant (fsvn-gather-root))
	comp)
    (fsvn-completion-window-control value applicant)
    (when (and (setq comp (try-completion value applicant))
	       (stringp comp))
      (fsvn-completing-url-clear-sentence)
      (insert comp))))

(defun fsvn-completing-repos-file ()
  (let* ((value (fsvn-completing-current-value))
	 (name (fsvn-processing-filename value))
	 (dirname (fsvn-completing-previous-segment value))
	 applicant comp)
    (setq applicant (fsvn-completing-repos-alist dirname))
    (fsvn-completion-window-control name applicant)
    (when (and (setq comp (try-completion name applicant))
	       (stringp comp))
      (fsvn-completing-url-clear-segment)
      (insert comp))))

(defun fsvn-completing-local-file ()
  (let ((init (fsvn-completing-current-value)))
    (when (fsvn-completing-url-clear-segment)
      (insert (fsvn-file-name-nondirectory init)))
    (let* ((value (fsvn-completing-current-value))
	   (name (fsvn-processing-filename value))
	   (dirname (fsvn-completing-local-file-previous-segment value))
	   (regexp (and (string= "" name) (concat "^" (regexp-quote name))))
	   applicant comp)
      (setq applicant (fsvn-completing-dir-files dirname regexp))
      (fsvn-completion-window-control name applicant)
      (when (setq comp (try-completion name applicant))
	(fsvn-completing-url-clear-segment)
	(if (stringp comp)
	    (insert comp)
	  (insert name))))))

(defun fsvn-completing-non-applicant ()
  "No applicant as local directory."
  (let ((init (fsvn-completing-current-value)))
    (fsvn-completing-url-clear-segment)
    (insert (fsvn-expand-file init))
    (let (applicant comp)
      (setq applicant (fsvn-completing-dir-files default-directory (concat "^" (regexp-quote init))))
      (fsvn-completion-window-control init applicant)
      (when (setq comp (try-completion init applicant))
	(fsvn-completing-url-clear-segment)
	(insert comp)))))

(defun fsvn-completing-dir-files (dir &optional match)
  (let ((name))
    (mapcar
     (lambda (file)
       (setq name
	     (if (eq t (car (file-attributes file)))
		 (concat file "/")
	       file))
       (fsvn-completing-file-name name))
     (directory-files dir t (or match dired-re-no-dot)))))

(defun fsvn-completing-url-clear-sentence ()
  (let ((start (fsvn-completing-url-start-sentence))
	(end (fsvn-completing-url-end-sentence)))
    (if (= start end)
	nil
      (delete-region start end)
      t)))
  
(defun fsvn-completing-url-start-sentence ()
  (save-excursion 
    (skip-chars-backward (fsvn-completing-non-word-class))
    (point)))

(defun fsvn-completing-url-end-sentence ()
  (save-excursion 
    (skip-chars-forward (fsvn-completing-non-word-class))
    (point)))

(defun fsvn-completing-url-clear-segment ()
  (let ((start (fsvn-completing-url-start-segment))
	(end (fsvn-completing-url-end-segment)))
    (if (= start end)
	nil
      (delete-region start end)
      t)))

(defun fsvn-completing-url-start-segment ()
  (save-excursion 
    (skip-chars-backward "^/ \t\n")
    (point)))

(defun fsvn-completing-url-end-segment ()
  (save-excursion 
    (skip-chars-forward "^/ \t\n")
    (point)))

(defun fsvn-completing-local-file-previous-segment (string)
  (cond
   ((string-match "^file:///\\([a-zA-Z]:.*\\)/[^/]*$" string)
    (match-string 1 string))
   ((string-match "^file:///\\([a-zA-Z]:\\)" string)
    (match-string 1 string))
   (t
    (fsvn-completing-previous-segment string))))

(defun fsvn-completing-previous-segment (string)
  (cond
   ((fsvn-file-name-root-p string)
    string)
   ((string-match "^\\(.*\\)/$" string)
    (match-string 1 string))
   ((string-match "^\\(.*\\)/[^/]+$" string)
    (match-string 1 string))))

(defun fsvn-completing-url-host-segment-p (contents)
  (let ((regexp (concat "^" (regexp-opt (fsvn-delete "file" fsvn-svn-url-scheme-list) t) ":/+\\([^/]+\\)?$")))
    (string-match regexp contents)))

(defun fsvn-completing-url-local-repository-p (contents)
  (let ((regexp "^file:///"))
    (when (string-match regexp contents)
      (not (fsvn-any-startswith (fsvn-gather-root) contents)))))

(defvar fsvn-completion-repository-cache nil)

(defun fsvn-completing-repos-alist (url)
  (let ((dir (fsvn-urlrev-url (directory-file-name url)))
	tmp)
    (unless (fsvn-string-assoc dir fsvn-completion-repository-cache)
      (setq tmp
	    (mapcar
	     (lambda (entry)
	       (let ((name (fsvn-xml-lists->list->entry=>name$ entry)))
		 ;;todo
		 (setq name (replace-regexp-in-string " " "%20" name))
		 (cons
		  (if (eq (fsvn-xml-lists->list->entry.kind entry) 'dir)
		      (concat name "/")
		    name)
		  nil)))
	     ;; append HEAD probablly svn bug of parse url.
	     (fsvn-get-ls (fsvn-url-urlrev dir "HEAD"))))
      (setq fsvn-completion-repository-cache
	    (cons
	     (cons dir tmp)
	     fsvn-completion-repository-cache)))
    (cdr (fsvn-string-assoc dir fsvn-completion-repository-cache))))

(defun fsvn-completing-url-repository-p (url)
  (string-match (concat "^" (regexp-opt fsvn-svn-url-scheme-list) ":") url))



(defvar fsvn-read-subcommand-args-map nil
  "Local keymap for minibuffer to read fsvn long options.")

(unless fsvn-read-subcommand-args-map
  (setq fsvn-read-subcommand-args-map
	(let ((map (copy-keymap minibuffer-local-map)))

	  (define-key map " "    'self-insert-command)
	  (define-key map "\C-i" 'fsvn-subcommand-args-completion)
	  map)))

(defvar fsvn-reading-subcommand nil)
(defun fsvn-reading-subcommand (alist subcommand non-global)
  (let ((tmp (cdr (copy-sequence (assoc subcommand alist))))
	ret)
    (setq ret tmp)
    (when non-global
      (setq ret nil)
      (mapc
       (lambda (x)
	 (unless (member (caar x) fsvn-svn-subcommand-global-options)
	   (setq ret (cons x ret))))
       tmp)
      (setq ret (nreverse ret)))
    (cons subcommand ret)))

(defun fsvn-read-subcommand-args (subcommand &optional non-global &rest default-args)
  (setq fsvn-completion-saved-configuration nil)
  (setq default-args (fsvn-flatten-command-args default-args))
  (unless fsvn-reading-subcommand
    (error "Subcommand %s not found" subcommand))
  (unwind-protect
      (fsvn-complete-reding-expand-arguments
       (read-from-minibuffer (format "Args for `%s' " subcommand)
			     (mapconcat 'identity default-args " ")
			     fsvn-read-subcommand-args-map))
    (setq fsvn-completion-saved-configuration nil)
    (setq fsvn-reading-subcommand nil)))

(defun fsvn-read-svn-subcommand-args (subcommand &optional non-global &rest default-args)
  (setq fsvn-reading-subcommand
	(fsvn-reading-subcommand fsvn-svn-subcommand-arguments-alist subcommand non-global))
  (apply 'fsvn-read-subcommand-args subcommand non-global default-args))

(defun fsvn-read-svnadmin-subcommand-args (subcommand &rest default-args)
  (setq fsvn-reading-subcommand
	(fsvn-reading-subcommand fsvn-svnadmin-subcommand-arguments-alist subcommand nil))
  (apply 'fsvn-read-subcommand-args subcommand nil default-args))

(defun fsvn-subcommand-args-completion ()
  (interactive)
  ;; space == 0x20 or tab == 0x9 or end-of-buffer
  (unless (memq (char-after) '(#x20 #x9 nil))
    ;; todo fsvn-complete-reading-argument-syntax
    (forward-word))
  (let* ((args (cdr fsvn-reading-subcommand))
	 (prevs (fsvn-complete-reading-contents (point)))
	 (curr (fsvn-complete-reading-previous-argument prevs))
	 (completions (fsvn-subcommand-create-completions prevs curr args))
	 (applicant (all-completions (or curr "") completions))
	 (complete (try-completion (or curr "") completions)))
    (cond
     ((= (length applicant) 0)
      (fsvn-completion-window-show nil))
     ((= (length applicant) 1)
      (fsvn-completion-window-delete)
      (fsvn-complete-reading-previous-delete)
      (insert (car applicant))
      (fsvn-complete-reading-temp-message " [Sole Match]"))
     (complete
      (fsvn-complete-reading-previous-delete)
      (insert complete)
      (fsvn-completion-window-show applicant))
     (t
      (fsvn-completion-window-show applicant)))))

(defun fsvn-complete-reading-temp-message (m)
  "Show temporary message in minibuffer.
referenced mew-complete.el"
  (let ((savemax (point-max)))
    (save-excursion
      (goto-char (point-max))
      (insert m))
    (let ((inhibit-quit t))
      (sit-for 0.5)
      (delete-region savemax (point-max))
      (when quit-flag
	(setq quit-flag nil)
	(setq unread-command-events (list 7))))))

(defun fsvn-complete-reading-previous-delete ()
  (let ((end (point))
	start)
    (skip-chars-backward (fsvn-completing-non-word-class))
    (setq start (point))
    (delete-region start end)))

(defun fsvn-complete-reading-previous-argument (prevs)
  (let ((c (char-before)))
    ;; space == 0x20 or tab == 0x9
    (if (memq c '(#x20 #x9))
	nil
      (car (nreverse (copy-seq prevs))))))

(defun fsvn-complete-reading-contents (&optional point)
  (let* ((contents (buffer-substring (point-min) (or point (point-max))))
	 (start (next-single-property-change 0 'read-only contents)))
    ;; start means end of propmt string.
    (unless start
      (setq start (length contents)))
    (fsvn-complete-reading-split-arguments (substring contents start))))

(defvar fsvn-complete-reading-argument-syntax
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\x020 "-" st)
    (modify-syntax-entry ?\x09 "-" st)
    st))

(defun fsvn-complete-reding-expand-arguments (string)
  (let (tmp)
    ;;todo quoted string.
    (mapcar
     (lambda (x)
       (cond
	((and (string-match "^-[^-]$" x)
	      (setq tmp (fsvn-subcommand-assoc-argument x (cdr fsvn-reading-subcommand))))
	 ;; get long option
	 (caar tmp))
	(t
	 x)))
     (fsvn-complete-reading-split-arguments string))))

(defun fsvn-complete-reading-split-arguments (string)
  (with-temp-buffer
    (let ((st fsvn-complete-reading-argument-syntax)
	  prev ret)
      (set-syntax-table st)
      (insert string)
      (goto-char (point-min))
      (skip-syntax-forward "-")
      (setq prev (point))
      (condition-case err
	  (while (not (eobp))
	    (forward-sexp)
	    (setq ret (cons (buffer-substring prev (point)) ret))
	    (skip-syntax-forward "-")
	    (setq prev (point)))
	(scan-error
	 (setq ret (cons (buffer-substring prev (point-max)) ret))))
      (nreverse ret))))

(defun fsvn-subcommand-create-completions (prevs current all-applicant)
  (let ((collection (fsvn-subcommand-create-completions-toplevel all-applicant))
	(next-applicant all-applicant)
	prev completions item)
    (while prevs
      (setq prev (car prevs))
      (setq prevs (cdr prevs))
      (setq completions (all-completions prev collection))
      (setq item (car (member prev completions)))
      (cond
       ((and (null prevs) current)
	)
       ((null item)
	(setq next-applicant all-applicant))
       ((not (string= item prev))
	(setq next-applicant all-applicant))
       ((null next-applicant)
	(setq next-applicant (cdr (fsvn-subcommand-assoc-argument item all-applicant))))
       (t
	;; completely match then enter node.
	(setq next-applicant (cdr (fsvn-subcommand-assoc-argument item next-applicant)))))
      (cond
       ((null next-applicant)
	(setq collection (fsvn-subcommand-create-completions-toplevel all-applicant)))
       ((atom next-applicant)
	(setq collection nil))
       (t
	(setq collection (fsvn-subcommand-create-completions-toplevel next-applicant)))))
    (when (and (null current) (null next-applicant))
      (setq collection (fsvn-subcommand-create-completions-toplevel all-applicant)))
    collection))

(defun fsvn-subcommand-create-completions-toplevel (applicant)
  (cond
   ((null applicant)
    nil)
   ((atom applicant)
    nil)
   (t
    (let (ret)
      (mapc
       (lambda (x)
	 (let ((opt (car x)))
	   (mapc
	    (lambda (c)
	      (cond
	       ((null c))
	       (t
		(setq ret (cons (cons c (cdr x)) ret)))))
	    ;; car must appear and is long option cdr is short option
	    (list (car opt) (cdr opt)))))
       applicant)
      (nreverse ret)))))



;;FIXME
(defun fsvn-completing-point-string ()
  (let ((init (point))
	start end)
    (cond
     ((save-excursion
	(let (quote-type)
	  (when (fsvn-non-escaped-previous-quoted)
	    (forward-char 1)
	    (setq start (point))
	    (setq quote-type (match-string 0))
	    (when (fsvn-non-escaped-next-quoted quote-type)
	      (when (and (>= init start)
			 (< init (point)))
		(setq end (1- (point))))))))
      (buffer-substring start end))
     ((memq (char-before) `(,fsvn-space-char ?\t ?\" ?\' ?\n))
      nil)
     (t
      (save-excursion
	(skip-chars-backward (fsvn-completing-non-word-class))
	(setq start (point))
	(skip-chars-forward (fsvn-completing-non-word-class))
	(setq end (point))
	(buffer-substring start end))))))

(defun fsvn-non-escaped-previous-quoted ()
  (let ((init (point))
	c ret)
    (while (and (re-search-backward "\"\\|'" nil t)
		(setq ret t)
		(setq c (char-before))
		(= c ?\\))
      (setq ret nil))
    (unless ret
      (goto-char init))
    ret))

(defun fsvn-non-escaped-next-quoted (quote-type)
  (let ((init (point))
	c ret)
    (while (and (re-search-forward (format "%s" quote-type) nil t)
		(setq ret t)
		(setq c (char-before (1- (point))))
		(= c ?\\))
      (setq ret nil))
    (unless ret
      (goto-char init))
    ret))



(provide 'fsvn-minibuf)

;;; fsvn-minibuf.el ends here
