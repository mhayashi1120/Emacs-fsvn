;;; fsvn-browse.el --- Dired like mode for Subversion working-copy/repository.


;;; History:
;; 

;;; Commentary:
;; 

(require 'fsvn-mode)
(require 'fsvn-logview)
(require 'fsvn-propview)
(require 'fsvn-select)
(require 'fsvn-popup)
(require 'fsvn-msgedit)
(require 'fsvn-dired)



(defvar transient-mark-mode)



(fsvn-defstruct browse-file
  name directory-p)

(defconst fsvn-browse-ls-dir-status-length 1)
(defconst fsvn-browse-ls-status-column 4)
(defconst fsvn-browse-ls-revision-length 6)
(defconst fsvn-browse-ls-author-length 15)
(defconst fsvn-browse-ls-size-length 7)
(defconst fsvn-browse-ls-dir-status-column
  (+ fsvn-browse-ls-status-column
     fsvn-svn-status-length
     1))
(defconst fsvn-browse-ls-revision-column
  (+ fsvn-browse-ls-dir-status-column
     fsvn-browse-ls-dir-status-length
     1))
(defconst fsvn-browse-ls-author-column
  (+ fsvn-browse-ls-revision-column
     fsvn-browse-ls-revision-length
     1))
(defconst fsvn-browse-ls-size-column
  (+ fsvn-browse-ls-author-column
     fsvn-browse-ls-author-length
     1))
(defconst fsvn-browse-ls-date-column
  (+ fsvn-browse-ls-size-column
     fsvn-browse-ls-size-length
     1))

(defconst fsvn-browse-re-status-ignored "^....I")
(defconst fsvn-browse-re-mark "^[^ \n]")
(defconst fsvn-browse-re-dir "^..d ")
(defconst fsvn-browse-re-symlink "^..l ")
(defconst fsvn-browse-re-root "^ \\(Root\\): \\(.+\\)")
(defconst fsvn-browse-re-revision "^ \\(Revision\\): \\(.+\\)")
(defconst fsvn-browse-re-subdir "^ \\(Path\\): \\(.*\\)")
(defconst fsvn-browse-re-format-subdir "^ Path: %s$")
(defconst fsvn-browse-re-locked-user
  (format "^.\\{%d\\}+\\(\\[[^]\n]+\\]\\)" fsvn-browse-ls-author-column))

(defconst fsvn-browse-buffer-local-variables
  '(
    (fsvn-browse-subdir-alist)
    (fsvn-browse-repos-p)
    (revert-buffer-function . 'fsvn-browse-revert-buffer)
    (font-lock-defaults . '(fsvn-browse-font-lock-keywords t nil nil beginning-of-line))
    (fsvn-browse-buffer-files-status-process)
    (fsvn-browse-buffer-directories-status-process)
    (fsvn-browse-buffer-cleanup-process)
    (fsvn-buffer-repos-root)
    (fsvn-buffer-revision)
    (fsvn-browse-ls-comparer)
    (dired-directory)
    ))

(defconst fsvn-browse-mode-line-process
  '(
    (fsvn-browse-buffer-files-status-process " Checking file status... ")
    (fsvn-browse-buffer-directories-status-process " Checking directory status... ")
    (fsvn-browse-buffer-cleanup-process " Cleaning...")
    ))

(defconst fsvn-browse-ls-comparator-alist
  '(
    (repository fsvn-browse-ls-entry-name-comparer fsvn-browse-ls-entry-time-comparer)
    (working-copy fsvn-browse-ls-file-name-comparer fsvn-browse-ls-file-time-comparer)
    ))

(defvar fsvn-browse-ls-comparer nil)

(defvar fsvn-browse-repos-p nil)
(defvar fsvn-browse-subdir-alist nil)
(defvar fsvn-browse-buffer-files-status-process nil)
(defvar fsvn-browse-buffer-directories-status-process nil)
(defvar fsvn-browse-buffer-cleanup-process nil)
(defvar fsvn-browse-file-name-function 'fsvn-browse-point-urlrev)

(defvar fsvn-browse-font-lock-keywords nil)
(setq fsvn-browse-font-lock-keywords
      (list

       ;; Directory headers.
       (list fsvn-browse-re-root '(1 fsvn-header-key-face) '(2 fsvn-header-face))
       (list fsvn-browse-re-revision '(1 fsvn-header-key-face) '(2 fsvn-header-face))

       (list fsvn-browse-re-subdir '(1 fsvn-header-key-face) '(2 fsvn-header-face))

       ;; Fsvn marks.
       (list fsvn-browse-re-mark '(0 fsvn-mark-face))

       ;; We make heavy use of MATCH-ANCHORED, since the regexps don't identify the
       ;; file name itself.  We search for Fsvn defined regexps, and then use the
       ;; Fsvn defined function `fsvn-move-to-filename' before searching for the
       ;; simple regexp ".+".  It is that regexp which matches the file name.
       ;;
       ;; Marked files.
       (list (concat "^[" (char-to-string fsvn-mark-mark-char) "]")
	     '(".+" (fsvn-move-to-filename) nil (0 fsvn-marked-face)))

       ;; Flagged files.
       (list (concat "^[" (char-to-string fsvn-mark-delete-char) "]")
	     '(".+" (fsvn-move-to-filename) nil (0 fsvn-flagged-face)))

       ;; Locked by other user.
       (list fsvn-browse-re-locked-user '(1 fsvn-warning-face))

       ;; People who are paranoid about security would consider this more
       ;; important than other things such as whether it is a directory.
       ;; But we don't want to encourage paranoia, so our default
       ;; should be what's most useful for non-paranoids. -- rms.
       ;; Subdirectories.
       (list fsvn-browse-re-dir
	     '(".+" (fsvn-move-to-filename) nil (0 fsvn-directory-face)))

       (list fsvn-browse-re-symlink
	     '(".+" (fsvn-move-to-filename) nil (0 fsvn-symlink-face)))

       (list fsvn-browse-re-status-ignored
	     '(".+" (fsvn-move-to-filename) nil (0 fsvn-ignored-face)))
       ))

(defvar fsvn-browse-diff-map nil)
(setq fsvn-browse-diff-map
      (let ((map (make-sparse-keymap)))
	(suppress-keymap map)
	(define-key map "=" 'fsvn-browse-diff-base)
	(define-key map "l" 'fsvn-browse-diff-local)
	(define-key map "e" 'fsvn-browse-ediff-base)
	map))

(defvar fsvn-browse-prefix-map nil)
(setq fsvn-browse-prefix-map
      (let ((map (make-sparse-keymap)))
	(suppress-keymap map)

	(define-key map "!" 'fsvn-command)
	(define-key map "+" 'fsvn-browse-mkdir)
	(define-key map "=" fsvn-browse-diff-map)
	(define-key map "C" 'fsvn-browse-commit-path)
	(define-key map "E" 'fsvn-browse-export-path)
	(define-key map "I" 'fsvn-browse-info-path)
	(define-key map "K" 'fsvn-browse-unlock-selected)
	(define-key map "L" 'fsvn-browse-log-path)
	(define-key map "P" 'fsvn-browse-propview-path)
	(define-key map "S" 'fsvn-browse-switch-path)
	(define-key map "T" 'fsvn-browse-remove-changelist-selected)
	(define-key map "U" 'fsvn-browse-update-path)
	(define-key map "\C-c" 'fsvn-browse-commit-path)
;; 	(define-key map "\C-f" 'fsvn-browse-find-directory)
	(define-key map "\C-p" 'fsvn-browse-propview-this)
	(define-key map "\C-r" 'fsvn-browse-resolved-selected)
	(define-key map "\C-vc" 'fsvn-browse-copy-this)
	(define-key map "\C-vm" 'fsvn-browse-move-this)
	(define-key map "\C-v\ec" 'fsvn-browse-cleanup-path)
	(define-key map "\C-vr" 'fsvn-browse-revert-selected)
	(define-key map "\C-vR" 'fsvn-browse-revert-path)
	(define-key map "\C-va" 'fsvn-browse-add-selected)
	(define-key map "\C-v\C-c" 'fsvn-browse-copy-selected)
	(define-key map "\C-v\C-d" 'fsvn-browse-delete-selected)
	(define-key map "\C-v\C-m" 'fsvn-browse-move-selected)
	(define-key map "\C-bm" 'fsvn-browse-safe-move-this)
	(define-key map "\C-bc" 'fsvn-browse-safe-copy-this)
;; 	(define-key map "\C-b\C-m" 'fsvn-browse-safe-move-selected)
;; 	(define-key map "\C-b\C-c" 'fsvn-browse-safe-copy-selected)
	(define-key map "\ec" 'fsvn-global-cleanup-buffer)
	(define-key map "\ei" 'fsvn-browse-add-prop-svn:ignore)
	(define-key map "a" 'fsvn-browse-add-selected)
	(define-key map "b" 'fsvn-browse-blame-this)
	(define-key map "c" 'fsvn-browse-commit-selected)
	(define-key map "d" 'fsvn-browse-delete-selected)
	(define-key map "e" 'fsvn-browse-export-this)
	(define-key map "h" 'fsvn-show-svn-help)
	(define-key map "i" 'fsvn-browse-info-selected)
	(define-key map "k" 'fsvn-browse-lock-selected)
	(define-key map "l" 'fsvn-browse-log-this)
	(define-key map "m" 'fsvn-browse-magic-head)
	(define-key map "o" 'fsvn-browse-open-repository)
	(define-key map "p" 'fsvn-browse-propview-this)
	(define-key map "t" 'fsvn-browse-add-changelist-selected)
	(define-key map "u" 'fsvn-browse-update-selected)
	(define-key map "w" 'fsvn-dired-copy-repository-url)
	(define-key map "\er" 'fsvn-browse-resolve-selected)
	map))

(defvar fsvn-browse-mode-map nil)
(setq fsvn-browse-mode-map
      (let ((map (make-sparse-keymap)))
	(suppress-keymap map)
	(fsvn-readonly-mode-keymap map)

	;; todo see dired-mode-map
	(define-key map " " 'scroll-up)
	(define-key map "%" (make-sparse-keymap))
	(define-key map "%d" 'fsvn-browse-mark-delete-regexp)
	(define-key map "%m" 'fsvn-browse-mark-file-regexp)
	(define-key map "=" fsvn-browse-diff-map)
	(define-key map "U" 'fsvn-browse-mark-all-unmark)
	(define-key map "\C-c" fsvn-browse-prefix-map)
	(define-key map "\C-m" 'fsvn-browse-file-this)
	(define-key map "\C-n" 'fsvn-next-file)
	(define-key map "\C-p" 'fsvn-previous-file)
	(define-key map "\d" 'scroll-down)
	(define-key map "^" 'fsvn-browse-up-directory)
	(define-key map "d" 'fsvn-browse-mark-file-delete)
	(define-key map "g" 'revert-buffer)
	(define-key map "m" 'fsvn-browse-mark-file-mark)
	(define-key map "n" 'fsvn-next-file)
	(define-key map "p" 'fsvn-previous-file)
	(define-key map "s" 'fsvn-browse-toggle-sort)
	(define-key map "u" 'fsvn-browse-mark-file-unmark)

	(define-key map "+" 'fsvn-dired-create-directory)
	(define-key map "B" 'fsvn-dired-do-byte-compile)
	(define-key map "C" 'fsvn-dired-do-copy)
	(define-key map "D" 'fsvn-dired-do-delete)
	(define-key map "L" 'fsvn-dired-do-load)
	(define-key map "R" 'fsvn-dired-do-rename)
	(define-key map "W" 'fsvn-dired-copy-filename-fullpath)
	(define-key map "Z" 'fsvn-dired-do-compress)
	(define-key map "w" 'fsvn-dired-copy-filename-as-kill)
	(define-key map "x" 'fsvn-dired-do-marked-delete)
	(define-key map "y" 'fsvn-dired-show-file-type)
	(define-key map "~" 'fsvn-dired-mark-delete-backup-files)
	map))

(defcustom fsvn-browse-mode-hook nil
  "*Run at the very end of `fsvn-browse-mode'."
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-browse-before-commit-hook nil
  "*Run before prepared file-select-buffer. Called with a list argument as filenames."
  ;; TODO called one arg then type is not hook?
  :group 'fsvn
  :type 'hook)

(defcustom fsvn-browse-cleanup-buffer t
  "*Kill `fsvn-browse-mode' buffer,  when leave.
Effected `fsvn-browse-up-directory' or `fsvn-browse-file-this' to directory."
  :group 'fsvn
  :type 'boolean)

;; * fsvn-browse-mode internal function

(defun fsvn-browse-mode ()
  (fsvn-global-initialize-mode)
  (use-local-map fsvn-browse-mode-map)
  (setq major-mode 'fsvn-browse-mode)
  (setq mode-name "Fsvn Browse")
  (setq truncate-lines t)
  (fsvn-make-buffer-variables fsvn-browse-buffer-local-variables)
  (fsvn-browse-setup-mode-line)
  (font-lock-mode 1)
  (font-lock-fontify-buffer))

(defconst fsvn-browse-dired-confirm-alist
  '(
    (fsvn-unsaved " *fsvn Unsaved*" "Save %s ")
    (fsvn-browse-update-selected " *svn Update*" "Update %s ")
    (fsvn-browse-resolved-selected " *svn Resolved*" "Resolved %s ")
    (fsvn-browse-add-selected " *svn Add*" "Add %s ")
    (fsvn-browse-delete-selected " *svn Delete*" "Delete %s ")
    (fsvn-browse-lock-selected " *svn Lock*" "Lock %s ")
    (fsvn-browse-unlock-selected " *svn Unock*" "Unlock %s ")
    (fsvn-browse-add-changelist-selected " *svn Add Changelist*" "Add Changelist %s ")
    (fsvn-browse-remove-changelist-selected " *svn Remove Changelist*" "Remove Changelist %s ")
    (fsvn-browse-revert-selected " *svn Revert*" "Revert %s ")
    (fsvn-browse-add-prop-svn:ignore " *svn Add to svn:ignore*" "Ignore %s ")
    (fsvn-browse-toggle-prop-svn:needs-lock " *svn Toggle svn:needs-lock*" "Needs-Lock %s ")
    (fsvn-browse-move-selected " *svn Move*" "Move to: ")
    (fsvn-browse-copy-selected " *svn Copy*" "Copy to: ")
    (fsvn-browse-export-this " *svn Export*" "Export %s")
    (fsvn-browse-svn:externals-selected " *svn Externals*" "Externals Property from %s ")
    (load " *Load*" "Load %s ")
    (byte-compile " *Byte-Compile*" "Byte-compile %s ")
    (delete " *Deletions*" "Delete %s ")
    )
  "KEY BUF-NAME PROMPT")

(defun fsvn-browse-dired-confirm (objects op-symbol &optional confirmer)
  (let (file-list buf-name prompt message)
    (setq file-list
	  (mapcar 
	   (lambda (x)
	     (cond
	      ((bufferp x)
	       (buffer-name x))
	      ((stringp x)
	       (fsvn-file-name-nondirectory x))
	      (t
	       (error "Not supported type"))))
	   objects))
    (setq message (cdr (assq op-symbol fsvn-browse-dired-confirm-alist)))
    (setq buf-name (nth 0 message)
	  prompt (nth 1 message))
    (dired-mark-pop-up
     buf-name op-symbol file-list (or confirmer 'y-or-n-p)
     (format prompt (dired-mark-prompt t file-list)))))

(defun fsvn-browse-file-size-text (size)
  (fsvn-string-lpad
   (cond
    ((< size 1000000)
     (number-to-string size))
    ((< size 1000000000.0)
     (format "%0.1fM" (/ size 1000000.0)))
    (t
     (format "%0.1fG" (/ size 1000000000.0))))
   fsvn-browse-ls-size-length))

(defun fsvn-browse-status-draw-status (target=cl)
  ;; todo cl not have path
  (let* ((path (fsvn-xml-status->target.path target=cl))
	 entries)
    (fsvn-save-browse-directory-excursion path
      (setq entries (fsvn-xml-status->target&cl->entries target=cl))
      (mapc
       (lambda (entry)
	 (fsvn-browse-draw-status-internal entry))
       entries))))

(defun fsvn-browse-status-process-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (fsvn-browse-process-unlock proc)
    (when (= (process-exit-status proc) 0)
      (mapc
       (lambda (target)
	 (fsvn-browse-status-draw-status target))
       (fsvn-xml-parse-status)))
    (kill-buffer (current-buffer))))

(defun fsvn-browse-status-process-filter (proc event)
  (fsvn-process-event-handler proc event
    (fsvn-debug event)
    (goto-char (point-max))
    (insert event)))

(defun fsvn-browse-ls-insert-repos-directory (info directory-url)
  (let* ((entries (fsvn-get-ls directory-url))
	 (path (fsvn-info-repos-path info)))
    (setq entries (sort entries fsvn-browse-ls-comparer))
    (fsvn-browse-draw-path path)
    (mapc
     (lambda (x)
       (fsvn-browse-ls-insert-repos-entry x))
     entries)))

(defun fsvn-browse-ls-insert-wc-directory (directory)
  (let* (file-entries status-entries status-entry entries)
    (setq file-entries (fsvn-browse-ls-directory-files directory))
    (setq status-entries (fsvn-get-directory-files-status directory))
    (setq entries (fsvn-browse-ls-merge-wc-entries directory file-entries status-entries))
    (fsvn-browse-draw-path directory)
    (mapc
     (lambda (entry)
       (fsvn-browse-ls-insert-wc-entry (car entry) (cdr entry)))
     entries)))

(defun fsvn-browse-ls-merge-wc-entries (directory file-entries status-entries)
  (let (status-hash ret status path)
    ;; create hash for a lot of files.
    ;;   3000 files 82sec -> 13sec.
    (setq status-hash (make-hash-table :test 'fsvn-file-hash-test))
    (mapc
     (lambda (status-entry)
       (puthash (fsvn-xml-status->target->entry.path status-entry) status-entry status-hash))
     status-entries)
    (mapc
     (lambda (file)
       (setq status (gethash (directory-file-name file) status-hash))
       (setq ret (cons (cons file status) ret)))
     file-entries)
    (mapc
     (lambda (status-entry)
       (setq path (fsvn-xml-status->target->entry.path status-entry))
       (unless (or (fsvn-file= path directory) (fsvn-file-assoc path ret))
	 (setq ret (cons (cons path status-entry) ret))))
     status-entries)
    (sort ret fsvn-browse-ls-comparer)))

(defun fsvn-browse-ls-entry-name-comparer (ent1 ent2)
  (string-lessp
   (fsvn-xml-lists->list->entry=>name$ ent1)
   (fsvn-xml-lists->list->entry=>name$ ent2)))

(defun fsvn-browse-ls-file-name-comparer (file1 file2)
  (string-lessp
   (if (stringp file1) file1 (car file1))
   (if (stringp file2) file2 (car file2))))

(defun fsvn-browse-ls-file-time-comparer (file1 file2)
  (let ((attr1 (file-attributes (if (stringp file1) file1 (car file1))))
	(attr2 (file-attributes (if (stringp file2) file2 (car file2)))))
    (cond
     ((and attr1 attr2)
      (not (time-less-p (nth 5 attr1) (nth 5 attr2))))
     ((and attr1 (null attr2))
      t)
     ((and (null attr1) attr2)
      nil)
     (t
      nil))))

(defun fsvn-browse-ls-entry-time-comparer (ent1 ent2)
  (not (time-less-p
	(fsvn-xml-lists->list->entry=>commit=>date$ ent1)
	(fsvn-xml-lists->list->entry=>commit=>date$ ent2))))

(defun fsvn-browse-ls-directory-files (directory)
  (let ((files (directory-files directory nil dired-re-no-dot))
	ret)
    (mapc
     (lambda (file)
       (unless (or (string= file (fsvn-meta-dir-name)))
	 (setq ret (cons (fsvn-expand-file file directory) ret))))
     files)
    ret))

(defun fsvn-browse-redraw-wc-file-entry (file)
  (let ((dir (fsvn-file-name-directory2 file))
	(status-entry (fsvn-get-file-status file)))
    (fsvn-save-browse-directory-excursion dir
      (cond
       ((null status-entry)
	(fsvn-browse-remove-wc-file-entry-internal file))
       ((fsvn-browse-goto-file file)
	(fsvn-browse-draw-status-this-line status-entry))
       (t
	(fsvn-browse-add-wc-file-entry file status-entry))))))

(defun fsvn-browse-add-wc-file-entry (file &optional status)
  "Add working copy file entry.
FILE
STATUS `t' means force to add entry."
  (let ((dir (fsvn-file-name-directory2 file))
	(status-entry (or status (fsvn-get-file-status file)))
	(filename (fsvn-file-name-nondirectory file)))
    (fsvn-save-browse-directory-excursion dir
      (when (and status-entry
		 (not (fsvn-browse-goto-file file)))
	(when (eq status-entry t)
	  (setq status-entry nil))
	(fsvn-browse-add-wc-raw-entry dir filename file status-entry)
	nil))))

(defun fsvn-browse-add-wc-raw-entry (dir filename file &optional status-entry)
  (catch 'done
    (fsvn-browse-each-file f dir
      (unless (funcall fsvn-browse-ls-comparer f filename)
	(let (buffer-read-only)
	  (forward-line 0)
	  (fsvn-browse-ls-insert-wc-entry file status-entry))
	(throw 'done t)))))

(defun fsvn-browse-remove-wc-file-entry (file)
  (let ((dir (fsvn-file-name-directory2 file)))
    (fsvn-save-browse-directory-excursion dir
      (fsvn-browse-remove-wc-file-entry-internal file))))

(defun fsvn-browse-remove-wc-file-entry-internal (file)
  (save-excursion
    (when (fsvn-browse-goto-file file)
      (fsvn-browse-remove-current-entry))))

(defun fsvn-browse-remove-current-entry ()
  (let (buffer-read-only start end)
    (forward-line 0)
    (setq start (point))
    (setq end (save-excursion (forward-line 1) (point)))
    (delete-region start end)))

(defun fsvn-browse-ls-insert-repos-entry (entry)
  (let ((dirp (eq (fsvn-xml-lists->list->entry.kind entry) 'dir))
	(filename (fsvn-xml-lists->list->entry=>name$ entry)))
    (fsvn-set-filename-property filename)
    (insert (format "  %c %s %s %s %s %s\n"
		    (if dirp ?d fsvn-space-char)
		    (fsvn-browse-ls-revision entry)
		    (fsvn-browse-ls-author-column entry)
		    (fsvn-browse-file-size-text (fsvn-safe-xml-lists->list->entry=>size$ entry))
		    (format-time-string fsvn-generic-datetime-format (fsvn-xml-lists->list->entry=>commit=>date$ entry))
		    filename))))

(defun fsvn-browse-ls-insert-wc-entry (filename &optional status-entry)
  (let* ((file-entry (fsvn-create-local-file-entry filename))
	 (dirp (fsvn-local-file.directory-p file-entry))
	 (linkp (fsvn-local-file.symlink-p file-entry))
	 (filename (fsvn-local-file.name file-entry))
	 size time)
    (setq size
	  (or (fsvn-local-file.size file-entry)
	      0))
    (setq time
	  (or (fsvn-local-file.modified-time file-entry)
	      (fsvn-xml-status->target->entry=>wc-status=>commit=>date$ status-entry)))
    (fsvn-set-filename-property filename)
    (insert (format "  %c %s %s %s %s %s %s %s\n"
		    (cond (dirp ?d) (linkp ?l) (t fsvn-space-char))
		    (fsvn-status-get-status status-entry)
		    "."
		    (fsvn-browse-status-revision status-entry)
		    (fsvn-browse-status-wc-author-column status-entry)
		    (fsvn-browse-file-size-text size)
		    (format-time-string fsvn-generic-datetime-format time)
		    filename
		    ))))

(defun fsvn-browse-ls-revision (entry)
  (fsvn-string-lpad 
   (number-to-string (fsvn-xml-lists->list->entry=>commit.revision entry))
   fsvn-browse-ls-revision-length))

(defun fsvn-browse-status-revision (entry)
  (let ((rev (fsvn-xml-status->target->entry=>wc-status=>commit.revision entry)))
    (fsvn-string-lpad (if (numberp rev) (number-to-string rev) rev)
		      fsvn-browse-ls-revision-length)))

(defun fsvn-browse-ls-author-string (author locker)
  (fsvn-string-rpad
   (if locker
       (concat "[" locker "]")
     author)
   fsvn-browse-ls-author-length))

(defun fsvn-browse-ls-author-column (entry)
  (fsvn-browse-ls-author-string (fsvn-xml-lists->list->entry=>commit=>author$ entry)
			   (fsvn-xml-lists->list->entry=>lock=>owner$ entry)))

(defun fsvn-browse-status-wc-author-column (entry)
  (fsvn-browse-ls-author-string (fsvn-xml-status->target->entry=>wc-status=>commit=>author$ entry)
			   (fsvn-xml-status->target->entry=>wc-status=>lock=>owner$ entry)))

(defun fsvn-browse-status-repos-author-column (entry)
  (fsvn-browse-ls-author-string (fsvn-xml-status->target->entry=>wc-status=>commit=>author$ entry)
			   (fsvn-xml-status->target->entry=>repos-status=>lock=>owner$ entry)))

(defun fsvn-browse-status-author-column (entry)
  (if (fsvn-xml-status->target->entry=>repos-status entry)
      (fsvn-browse-status-repos-author-column entry)
    (fsvn-browse-status-wc-author-column entry)))

(defmacro fsvn-browse-wc-only (&rest form)
  `(if fsvn-browse-repos-p
       (message "this command only executable in working copy.")
     ,@form))

(defmacro fsvn-browse-repos-only (&rest form)
  `(if (not fsvn-browse-repos-p)
       (message "this command only executable in repository.")
     ,@form))

(defmacro fsvn-browse-each-file (var path &rest form)
  "Move point and execute form.
VAR set `fsvn-current-filename'
PATH is each executed path."
  `(save-excursion
     (let (,var ret)
       (when (fsvn-browse-goto-directory (or ,path (fsvn-browse-current-path)))
	 (when (fsvn-browse-goto-first-file)
	   (while (setq ,var (fsvn-current-filename))
	     (setq ret (cons (progn ,@form) ret))
	     (fsvn-next-file))
	   (nreverse ret))))))

(defmacro fsvn-browse-mark-if (predicate msg)
  `(let ((count 0))
     (save-excursion
       (goto-char (point-min))
       (while (not (eobp))
	 (when ,predicate
	   (fsvn-browse-put-mark-point fsvn-mark-mark-char))
	 (forward-line 1)))
     (if ,msg (message "%s %s%s %s%s."
		       count
		       ,msg
		       (dired-plural-s count)
		       (if (eq fsvn-mark-mark-char fsvn-space-char) "un" "")
		       "marked"))))

(defun fsvn-browse-process-locked-p ()
  fsvn-browse-buffer-files-status-process)

(defun fsvn-browse-process-lock (proc)
  (setq fsvn-browse-buffer-files-status-process proc))

(defun fsvn-browse-process-unlock (proc)
  (let ((buffer (fsvn-find-buffer-by-variable 'fsvn-browse-buffer-files-status-process proc)))
    (when buffer
      (with-current-buffer buffer
	(setq fsvn-browse-buffer-files-status-process nil)))))

(defun fsvn-browse-status (info directory)
  "non recurse status check."
  (let (proc args)
    (setq args (list "--xml" "--no-ignore" "--non-recursive" "--non-interactive"))
    (when (fsvn-config-browse-show-update (fsvn-xml-info->entry=>repository=>root$ info))
      (setq args (nconc args (list "--show-updates"))))
    (setq args (nconc args (list directory)))
    (setq proc (fsvn-start-command "status"
				   (fsvn-make-temp-buffer)
				   args))
    (set-process-sentinel proc 'fsvn-browse-status-process-sentinel)
    (set-process-filter proc 'fsvn-browse-status-process-filter)
    (fsvn-browse-process-lock proc)))

(defun fsvn-browse-cleanup-process-sentinel (proc event)
  (fsvn-process-exit-handler proc event
    (let ((buffer (process-get proc 'fsvn-browse-cleaning-buffer)))
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (setq fsvn-browse-buffer-cleanup-process nil))))
    (if (= (process-exit-status proc) 0)
	(message "Cleanup process finished.")
      (message "Cleanup process failed."))
    (kill-buffer (current-buffer))))

(defun fsvn-browse-setup-mode-line ()
  (or (assq 'fsvn-browse-buffer-files-status-process mode-line-process)
      (setq mode-line-process
	    (append fsvn-browse-mode-line-process mode-line-process))))

(defun fsvn-browse-goto-directory (directory)
  (let ((saved (point)))
    (goto-char (point-min))
    (if (re-search-forward (format fsvn-browse-re-format-subdir (regexp-quote directory)) nil t)
	(progn
	  (forward-line 0)
	  (forward-char 1)
	  (point))
      (goto-char saved)
      nil)))

(defun fsvn-browse-goto-file (file)
  (let ((filename (fsvn-urlrev-filename file))
	point)
    (setq point (catch 'found
		  (fsvn-browse-each-file f nil
		    (when (fsvn-file= f filename)
		      (throw 'found (point))))
		  nil))
    (when point
      (goto-char point))))

(defun fsvn-browse-goto-first-file ()
  (let ((saved (point)))
    (when (or (re-search-backward fsvn-browse-re-subdir nil t)
	      (save-excursion (forward-line 0) (looking-at fsvn-browse-re-subdir)))
      (forward-line 2)
      (or (fsvn-move-to-filename)
	  (and (goto-char saved)
	       nil)))))

(defmacro fsvn-browse-with-move-dir-status (dir &rest form)
  `(save-excursion
     (when (fsvn-browse-goto-file ,dir)
       (fsvn-browse-move-to-dir-status)
       ,@form)))

(defun fsvn-browse-move-to-dir-status ()
  (forward-line 0)
  (forward-char fsvn-browse-ls-dir-status-column))

(defun fsvn-browse-put-dir-status-current (&optional mark)
  (let (buffer-read-only)
    (delete-char 1)
    (insert (or mark ?.))
    (setq buffer-undo-list nil)
    (set-buffer-modified-p nil)))

(defun fsvn-browse-get-dir-status-current ()
  (char-after))

(defun fsvn-browse-draw-dir-status (dir &optional mark)
  (fsvn-browse-with-move-dir-status dir
    (let ((current (char-after)))
      (when (fsvn-dir-status-stronger-than mark current)
	(fsvn-browse-put-dir-status-current mark)))))

(defun fsvn-browse-put-status-if-weak-1 (file mark)
  (fsvn-browse-put-status-if-weak-internal file mark 0))

(defun fsvn-browse-put-status-if-weak-2 (file mark)
  (fsvn-browse-put-status-if-weak-internal file mark 1))

(defun fsvn-browse-put-status-if-weak-internal (file mark column)
  (let ((curr (fsvn-browse-get-status-internal file column)))
    (when (and curr (or (= curr ?.) (fsvn-file-status-stronger-than mark curr)))
      (fsvn-browse-put-status-internal file mark column))))

(defun fsvn-browse-put-status-1 (file mark)
  (fsvn-browse-put-status-internal file mark 0))

(defun fsvn-browse-put-status-2 (file mark)
  (fsvn-browse-put-status-internal file mark 1))

(defun fsvn-browse-put-status-3 (file mark)
  (fsvn-browse-put-status-internal file mark 2))

(defun fsvn-browse-put-status-4 (file mark)
  (fsvn-browse-put-status-internal file mark 3))

(defun fsvn-browse-put-status-5 (file mark)
  (fsvn-browse-put-status-internal file mark 4))

(defun fsvn-browse-put-status-6 (file mark)
  (fsvn-browse-put-status-internal file mark 5))

(defmacro fsvn-browse-with-move-status (file column &rest form)
  `(save-excursion
     (when (fsvn-browse-goto-file ,file)
       (forward-line 0)
       (forward-char (+ fsvn-browse-ls-status-column ,column))
       ,@form)))

(defun fsvn-browse-put-status-internal (file mark column)
  (fsvn-browse-with-move-status file column
    (let (buffer-read-only)
      (save-excursion
	(delete-char 1)
	(insert mark)
	(setq buffer-undo-list nil)
	(set-buffer-modified-p nil)))))

(defun fsvn-browse-get-status-internal (file column)
  (fsvn-browse-with-move-status file column
    (char-after)))

(defun fsvn-browse-draw-status-internal (entry)
  (save-excursion
    (when (fsvn-browse-goto-file (fsvn-xml-status->target->entry.path entry))
      (fsvn-browse-draw-status-this-line entry))))

(defun fsvn-browse-draw-file-status (file)
  (let ((status (fsvn-get-file-status file)))
    (when status
      (fsvn-browse-draw-status-internal status))))

(defun fsvn-browse-draw-status-this-line (&optional status-entry)
  (let (status author)
    (when status-entry
      (setq status  (fsvn-status-get-status status-entry))
      (setq author (fsvn-browse-status-author-column status-entry)))
    (setq status (or status (make-string fsvn-svn-status-length ?.)))
    (let (buffer-read-only)
      (forward-line 0)
      ;; goto status first column
      (forward-char fsvn-browse-ls-status-column)
      (delete-char fsvn-svn-status-length)
      (insert status)
      (when author
	(fsvn-browse-draw-author-this-line author))
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil))))

(defun fsvn-browse-draw-attr-this-line ()
  (let* ((attr (file-attributes (fsvn-browse-point-url)))
	 (size (nth 7 attr))
	 (time (nth 5 attr))
	 buffer-read-only start end)
    (forward-line 0)
    ;; goto status first column
    (forward-char fsvn-browse-ls-size-column)
    (setq start (point)
	  end (1- (next-single-property-change (point) 'fsvn-filename nil (line-end-position))))
    (delete-region start end)
    (fsvn-browse-file-size-text size)
    (insert (fsvn-browse-file-size-text size) " "
	    (format-time-string fsvn-generic-datetime-format time))
    (setq buffer-undo-list nil)
    (set-buffer-modified-p nil)))

(defun fsvn-browse-draw-author-this-line (user-string)
  (let (buffer-read-only)
    (forward-line 0)
    (forward-char fsvn-browse-ls-author-column)
    (delete-char fsvn-browse-ls-author-length)
    (insert user-string)))

(defun fsvn-browse-clear-status (file)
  (when (fsvn-browse-goto-file file)
    (fsvn-browse-draw-status-this-line)))

(defun fsvn-browse-put-mark (mark-char file)
  (save-excursion
    (when (fsvn-browse-goto-file file)
      (fsvn-browse-put-mark-point mark-char))))

(defun fsvn-browse-put-mark-point (mark-char)
  (save-excursion
    (forward-line 0)
    (let ((buffer-read-only))
      (delete-char 1)
      (insert (or mark-char fsvn-space-char))
      (buffer-modified-p nil))))

(defun fsvn-browse-put-mark-current-file (mark)
  (cond
   ((fsvn-current-filename)
    (fsvn-browse-put-mark-point mark)
    (fsvn-next-file))
   ((fsvn-browse-current-path)
    (fsvn-browse-each-file file nil
      (fsvn-browse-put-mark-point mark)))
   (t
    (message "Cannot put mark on this line.")
    (fsvn-next-file))))

(defun fsvn-browse-switch-directory-buffer (directory-urlrev &optional revert)
  (if (fsvn-url-local-p directory-urlrev)
      (fsvn-browse-draw-local-directory directory-urlrev revert)
    (fsvn-browse-draw-repos-directory directory-urlrev revert))
  (set-visited-file-modtime (current-time))
  (setq buffer-read-only t)
  (switch-to-buffer (current-buffer))
  (run-hooks 'fsvn-browse-mode-hook))

(defun fsvn-browse-draw-repos-directory (directory-urlrev &optional type)
  (let (buffer info comparer)
    (set-buffer
     (cond
      ((eq type 'revert)
       (setq info (fsvn-get-info-entry directory-urlrev))
       (when (null info)
	 (error "%s is not a repository" directory-urlrev))
       (current-buffer))
      ((setq buffer (fsvn-get-exists-browse-buffer directory-urlrev))
       buffer)
      (t
       (setq type 'draw)
       (setq info (fsvn-get-info-entry directory-urlrev))
       (when (null info)
	 (error "%s is not a repository" directory-urlrev))
       (fsvn-browse-create-repos-buffer info))))
    ;;todo bug when revision changed.
    (setq comparer fsvn-browse-ls-comparer)
    (when (memq type '(revert draw))
      (let (buffer-read-only)
	(fsvn-browse-mode)
	(erase-buffer)
	(setq fsvn-browse-ls-comparer (or comparer 'fsvn-browse-ls-entry-name-comparer))
	(fsvn-browse-set-repos-local-variables info)
	(fsvn-browse-draw-topmost-header info)
	(fsvn-browse-ls-insert-repos-directory info directory-urlrev)
	(set-buffer-modified-p nil))
      (fsvn-browse-set-repos-directory info)
      (fsvn-browse-goto-first-file))))

(defun fsvn-browse-draw-local-directory (directory &optional type)
  (let ((parent-info (fsvn-working-copy-info directory))
	buffer comparer)
    (unless parent-info
      (error "Cannot draw `%s'" directory))
    (set-buffer
     (cond
      ((eq type 'revert)
       (current-buffer))
      ((setq buffer (fsvn-get-exists-browse-buffer directory))
       buffer)
      (t
       (setq type 'draw)
       (fsvn-browse-create-local-buffer directory))))
    (setq comparer fsvn-browse-ls-comparer)
    (cond
     ((memq type '(revert draw))
      (let (buffer-read-only)
	(fsvn-browse-mode)
	(erase-buffer)
	(setq fsvn-browse-ls-comparer (or comparer 'fsvn-browse-ls-file-name-comparer))
	(fsvn-browse-set-wc-local-variables parent-info directory)
	(fsvn-browse-draw-topmost-header parent-info)
	(fsvn-browse-ls-insert-wc-directory directory)
	(set-buffer-modified-p nil))
      (fsvn-browse-set-wc-directory (file-name-as-directory directory))
      (when (fsvn-directory-versioned-p directory)
	(fsvn-browse-status parent-info directory)
	(fsvn-run-recursive-status directory))
      (fsvn-browse-goto-first-file))
     ;; ((dired-directory-changed-p directory)
     ;; todo not works correctly
     ;;       (message "%s"
     ;; 	       (substitute-command-keys
     ;; 		"Directory has changed on disk; type \\[revert-buffer] to update Dired")))
     )))

(defun fsvn-browse-draw-path (directory)
  (goto-char (point-min))
  (if (re-search-forward (format fsvn-browse-re-format-subdir directory) nil t)
      (progn
	(replace-match "")
	(delete-char 1)
	(when (looking-at "^[ ]*\n")
	  (replace-match "")))
    (goto-char (point-max)))
  (insert (format " Path: %s\n" (fsvn-url-decode-string directory)))
  (insert "\n"))

(defun fsvn-browse-draw-topmost-header (info)
  (save-excursion
    (let (revision root)
      (goto-char (point-min))
      (when (re-search-forward fsvn-browse-re-revision nil t)
	(replace-match "")
	(delete-char 1))
      (when (re-search-forward fsvn-browse-re-root nil t)
	(replace-match "")
	(delete-char 1)
	(when (looking-at "^[ ]*\n")
	  (replace-match "")))
      (goto-char (point-min))
      (setq revision (fsvn-xml-info->entry.revision info))
      (setq root (fsvn-xml-info->entry=>repository=>root$ info))
      (insert (format " Revision: %d\n" revision))
      (insert (format " Root: %s\n" root))
      (insert "\n"))))

(defun fsvn-browse-set-wc-directory (dir)
  (fsvn-set-default-directory dir)
  (setq dired-directory default-directory)
  (set (make-local-variable 'list-buffers-directory) default-directory))

(defun fsvn-browse-set-repos-directory (info)
  (let* ((url (fsvn-xml-info->entry=>url$ info))
	 (rev (fsvn-xml-info->entry.revision info))
	 (urlrev (fsvn-url-urlrev url rev)))
    (fsvn-set-default-directory (fsvn-magic-create-name urlrev))
    (setq dired-directory default-directory)
    ;;   (set (make-local-variable 'list-buffers-directory) default-directory)
    ;;todo if already exists file.
    (unless (file-directory-p default-directory)
      (make-directory default-directory t))))

(defun fsvn-browse-set-repos-local-variables (info)
  (setq fsvn-buffer-repos-root (fsvn-xml-info->entry=>repository=>root$ info))
  (setq fsvn-buffer-revision (fsvn-xml-info->entry.revision info))
  (setq fsvn-browse-repos-p t)
  (let* ((subdir (fsvn-browse-subdir (fsvn-info-repos-path info) t)))
    (fsvn-browse-subdir!info subdir info)))

(defun fsvn-browse-set-wc-local-variables (info directory)
  (setq fsvn-buffer-repos-root (fsvn-xml-info->entry=>repository=>root$ info))
  (setq fsvn-buffer-revision (fsvn-xml-info->entry.revision info))
  (setq fsvn-browse-repos-p nil)
  (let* ((subdir (fsvn-browse-subdir directory nil)))
    (fsvn-browse-subdir!info subdir info)))

(defun fsvn-browse-create-repos-buffer (info)
  (generate-new-buffer (fsvn-url-decode-string (fsvn-xml-info->entry=>url$ info))))

(defun fsvn-browse-create-local-buffer (directory)
  (generate-new-buffer (fsvn-file-name-nondirectory directory)))

(defmacro fsvn-browse-subdir.setter (container key value)
  `(let (cell)
     (unless (setq cell (assq ,key (cdr (symbol-value ,container))))
       (setq cell (cons ,key nil))
       (nconc (symbol-value ,container) (list cell)))
     (setcdr cell ,value)))

(defmacro fsvn-browse-subdir.getter (container key)
  `(cdr (assq ,key (cdr (symbol-value ,container)))))

(defun fsvn-browse-subdir.info (subdir)
  (fsvn-browse-subdir.getter 'subdir 'info))

(defun fsvn-browse-subdir!info (subdir data)
  (fsvn-browse-subdir.setter 'subdir 'info data))

(defun fsvn-browse-subdir-directory-url (subdir)
  (let ((info (fsvn-browse-subdir.info subdir)))
    (if (not fsvn-browse-repos-p)
	(car subdir)
      (fsvn-url-urlrev
       (fsvn-xml-info->entry=>url$ info)
       (fsvn-xml-info->entry.revision info)))))

(defun fsvn-browse-subdir (path repos-p)
  ;;todo case fold
  (let* ((canon (directory-file-name path))
	 (key (if repos-p canon (fsvn-expand-file canon)))
	 (subdir (fsvn-string-assoc key fsvn-browse-subdir-alist)))
    (when (string= key "")
      (setq key "/"))
    (unless subdir
      (setq subdir (cons key nil))
      (setq fsvn-browse-subdir-alist
	    (cons subdir fsvn-browse-subdir-alist)))
    subdir))

(defun fsvn-browse-get-files ()
  "If region not activate, get current line's filename."
  (let (temp)
    (cond
     ((and transient-mark-mode mark-active)
      (fsvn-browse-get-region-files))
     ((setq temp (fsvn-browse-get-marked-files))
      temp)
     ((setq temp (funcall fsvn-browse-file-name-function))
      (list temp))
     (t
      nil))))

(defun fsvn-browse-mark-matched-file (regexp mark)
  (let ((count 0))
    (fsvn-browse-each-file file nil
      (when (string-match regexp file)
	(fsvn-browse-put-mark-point mark)
	(setq count (1+ count))))
    (message (if (= count 1) "1 file marked."
	       "%d files marked") count)))

(defun fsvn-browse-get-marked-files (&optional mark)
  (let* ((marker-char (or mark fsvn-mark-mark-char))
	 (regex (concat "^" (regexp-quote (char-to-string marker-char))))
	 ret temp)
    (save-excursion
      (when (fsvn-browse-goto-first-file)
	(forward-line 0)
	(while (and (not (eobp))
		    (setq temp (funcall fsvn-browse-file-name-function)))
	  (when (looking-at regex)
	    (setq ret (cons temp ret)))
	  (forward-line 1))
	(nreverse ret)))))

(defun fsvn-browse-get-region-files ()
  (let ((start (region-beginning))
	(end (region-end))
	temp ret)
    (save-excursion
      (goto-char start)
      (while (and (>= end (point))
		  (fsvn-move-to-filename))
	(when (setq temp (funcall fsvn-browse-file-name-function))
	  (setq ret (cons temp ret)))
	(forward-line 1)))
    (nreverse ret)))

(defun fsvn-browse-propview-mode (file directory-p)
  (let ((win-configure (current-window-configuration))
	(root fsvn-buffer-repos-root)
	(working-dir
	 (if (fsvn-url-local-p file)
	     (fsvn-browse-current-directory-url)
	   (fsvn-browse-current-magic-directory))))
    ;; for proplist mode
    (with-current-buffer (fsvn-proplist-get-buffer)
      (fsvn-proplist-mode)
      (fsvn-proplist-setup-window)
      (setq fsvn-default-window-configuration (current-window-configuration))
      (setq fsvn-previous-window-configuration win-configure)
      (setq fsvn-buffer-repos-root root)
      (setq fsvn-buffer-target-file file)
      (setq fsvn-buffer-target-directory-p directory-p)
      (setq fsvn-proplist-target-mode 'properties)
      (fsvn-set-default-directory working-dir)
      (setq buffer-read-only t)
      (fsvn-proplist-draw-list file)
      (fsvn-proplist-goto-first-property)
      (fsvn-proplist-draw-value (fsvn-proplist-current-propname))
      (run-hooks 'fsvn-proplist-mode-hook))
    (switch-to-buffer (fsvn-proplist-get-buffer))))

(defun fsvn-browse-revert-buffer (ignore-auto noconfirm)
  (let ((file (fsvn-current-filename))
	(dir (fsvn-browse-current-directory-url))
	;;FIXME when multible subdir and D mark
	(marked (fsvn-browse-get-marked-files))
	(opoint (point)))
    (mapc
     (lambda (subdir)
       (let* ((dir (car subdir))
	      (url (fsvn-browse-subdir-directory-url subdir)))
	 (fsvn-browse-goto-directory dir)
	 (fsvn-browse-switch-directory-buffer url 'revert)
	 (mapc
	  (lambda (file)
	    (fsvn-browse-put-mark fsvn-mark-mark-char file))
	  marked)))
     fsvn-browse-subdir-alist)
    (or (and dir
	     (fsvn-browse-goto-directory dir)
	     file
	     (fsvn-browse-goto-file file))
	(goto-char opoint))))

(defmacro fsvn-browse-open-log-edit (&rest form)
  `(let ((root fsvn-buffer-repos-root)
	 (browse-buffer (current-buffer))
	 (win-configure (current-window-configuration)))
     (with-current-buffer (fsvn-message-edit-get-buffer)
       (fsvn-message-edit-mode)
       ,@form
       (setq fsvn-previous-window-configuration win-configure)
       (setq fsvn-buffer-repos-root root)
       (run-hooks 'fsvn-message-edit-mode-hook))))

(defun fsvn-browse-add-file-select (files args)
  (when (and (fsvn-select-file-prepared-buffer)
	     (not (y-or-n-p "File select buffer already prepared.  Discard it? ")))
    (error "Kill `%s' Buffer manually" fsvn-select-file-buffer-name))
  (let* ((browse-buffer (current-buffer))
	 (win-configure (current-window-configuration))
	 (root fsvn-buffer-repos-root)
	 win)
    (mapc
     (lambda (b)
       (when (and b (buffer-live-p b))
	 (kill-buffer b)))
     (list (fsvn-select-file-prepared-buffer)))
    (with-current-buffer (fsvn-select-file-get-buffer)
      (fsvn-select-file-mode)
      (fsvn-select-file-draw-root root)
      (fsvn-select-file-draw-add-applicant files)
      (fsvn-select-file-first-file)
      (setq fsvn-select-file-source-files files)
      (setq fsvn-select-file-done 'fsvn-select-file-add)
      (setq fsvn-select-file-subcommand-args args)
      (setq fsvn-previous-window-configuration win-configure)
      (setq fsvn-buffer-repos-root root)
      (setq buffer-read-only t)
      (run-hooks 'fsvn-select-file-mode-hook))
    (fsvn-browse-setup-add-window)))

(defun fsvn-browse-commit-mode (files args)
  (when (and (fsvn-select-file-prepared-buffer)
	     (fsvn-message-edit-prepared-buffer))
    (fsvn-browse-setup-commit-window t)
    (unless (y-or-n-p "Log edit buffer already prepared. Discard it? ")
      (error "Type C-c C-q for Quit Edit.")))
  (let* ((unsaved (fsvn-files-unsaved-buffers files))
	 (browse-buffer (current-buffer))
	 (win-configure (current-window-configuration))
	 (root fsvn-buffer-repos-root)
	 win)
    ;; todo consider confirm argument
    (when (and unsaved (fsvn-browse-dired-confirm unsaved 'fsvn-unsaved))
      (mapc
       (lambda (buffer)
	 (with-current-buffer buffer
	   (save-buffer)))
       unsaved))
    (mapc
     (lambda (b)
       (when (and b (buffer-live-p b))
	 (kill-buffer b)))
     (list (fsvn-select-file-prepared-buffer)
	   (fsvn-message-edit-prepared-buffer)))
    ;; fsvn-message-edit-mode prior than fsvn-select-file-mode
    (with-current-buffer (fsvn-select-file-get-buffer)
      (fsvn-select-file-mode)
      (setq fsvn-select-file-source-files files)
      (setq fsvn-select-file-done 'fsvn-select-file-commit)
      (setq fsvn-previous-window-configuration win-configure)
      (setq fsvn-buffer-repos-root root)
      (setq buffer-read-only t)
      (fsvn-select-file-draw-root root)
      (fsvn-select-file-draw-commit-applicant files)
      (fsvn-select-file-first-file)
      (run-hooks 'fsvn-select-file-mode-hook))
    (fsvn-browse-open-log-edit
     (when (fsvn-config-tortoise-property-use root)
       (fsvn-tortoise-commit-initialize))
     (setq fsvn-message-edit-done 'fsvn-message-edit-commit)
     (when (member "--no-unlock" args)
       (fsvn-message-edit-toggle-no-unlock t))
     (when (member "--keep-changelist" args)
       (fsvn-message-edit-toggle-keep-changelist t))
     (setq fsvn-message-edit-subcommand-args args))
    (fsvn-browse-setup-commit-window)))

(defun fsvn-browse-delete-log-edit (files args)
  (fsvn-browse-open-log-edit
   (setq fsvn-message-edit-target-files files)
   (setq fsvn-message-edit-subcommand-args args)
   (setq fsvn-message-edit-done 'fsvn-message-edit-delete))
  (fsvn-browse-setup-log-edit-window))

(defun fsvn-browse-mkdir-log-edit (dir)
  (fsvn-browse-open-log-edit
   (setq fsvn-message-edit-subcommand-args (list dir))
   (setq fsvn-message-edit-done 'fsvn-message-edit-mkdir))
  (fsvn-browse-setup-log-edit-window))

;;TODO no use?
(defun fsvn-browse-lock-log-edit (files args)
  (fsvn-browse-open-log-edit
   (setq fsvn-message-edit-subcommand-args (fsvn-flatten-command-args (list files args)))
   (setq fsvn-message-edit-done 'fsvn-message-edit-lock))
  (fsvn-browse-setup-log-edit-window))

(defun fsvn-browse-setup-log-edit-window ()
  (delete-other-windows)
  (let ((win (split-window)))
    (set-window-buffer win (fsvn-message-edit-get-buffer))
    (set-frame-selected-window (selected-frame) win)))

(defun fsvn-browse-setup-add-window ()
  (delete-other-windows)
  (let* ((browse (current-buffer))
	 (select (fsvn-select-file-get-buffer))
	 (browse-win (split-window))
	 (file-win (selected-window))
	 done-command quit-command)
    (set-window-buffer browse-win browse)
    (set-window-buffer file-win select)
    (set-frame-selected-window (selected-frame) file-win)
    (fsvn-set-buffer-local-variable
     select
     'fsvn-default-window-configuration (current-window-configuration))
    (setq done-command 'fsvn-select-file-done)
    (setq quit-command 'fsvn-select-file-quit)
    (message
     (substitute-command-keys (concat "Type \\[" (symbol-name done-command) "] to finish edit, \
\\[" (symbol-name quit-command) "] to quit edit.")))))

(defun fsvn-browse-setup-commit-window (&optional no-msg)
  (delete-other-windows)
  (let* ((log (fsvn-message-edit-get-buffer))
	 (fselect (fsvn-select-file-get-buffer))
	 (log-win (split-window))
	 (fselect-win (selected-window))
	 (root fsvn-buffer-repos-root)
	 sel-buffer sel-window)
    (set-window-buffer log-win log)
    (set-window-buffer fselect-win fselect)
    ;; move focus to log edit buffer.
    (if (fsvn-config-commit-default-file-select-p root)
	(setq sel-window fselect-win
	      sel-buffer fselect)
      (setq sel-window log-win
	    sel-buffer log))
    (set-frame-selected-window (selected-frame) sel-window)
    (let ((f (lambda (buffer)
	       (fsvn-set-buffer-local-variable
		buffer
		'fsvn-default-window-configuration (current-window-configuration)))))
      (funcall f log)
      (funcall f fselect))
    (switch-to-buffer sel-buffer)
    (unless no-msg
      (fsvn-message-edit-commit-show-message))))

;; ** current point function

(defun fsvn-browse-point-directory-p ()
  "Return non-nil when current point indicate directory."
  (save-excursion
    (forward-line 0)
    (looking-at fsvn-browse-re-dir)))

(defun fsvn-browse-point-symlink-p ()
  "Return non-nil when current point indicate symlink."
  (save-excursion
    (forward-line 0)
    (looking-at fsvn-browse-re-symlink)))

;;TODO symlink?
(defun fsvn-browse-point-file-p ()
  "Return non-nil when current point indicate file."
  (and (fsvn-current-filename) (not (fsvn-browse-point-directory-p))))

(defun fsvn-browse-point-repository-urlrev ()
  "Current point URL."
  (if fsvn-browse-repos-p
      (fsvn-browse-point-urlrev)
    (let ((info (fsvn-get-info-entry (fsvn-current-filename))))
      (fsvn-xml-info->entry=>url$ info))))

(defun fsvn-browse-point-url ()
  "Current point URL."
  (cond
   ((null (fsvn-current-filename))
    nil)
   ((not fsvn-browse-repos-p)
    (fsvn-expand-file (fsvn-current-filename) (fsvn-browse-current-path)))
   (t
    (fsvn-expand-url
     (fsvn-current-filename)
     (fsvn-expand-url
      (fsvn-browse-current-path)
      (fsvn-browse-current-root))))))

(defun fsvn-browse-point-canonicalized-urlrev ()
  (let ((urlrev (fsvn-browse-point-urlrev)))
    (if (string-match "@" (fsvn-url-filename urlrev))
	(concat urlrev "@")
      urlrev)))

(defun fsvn-browse-point-urlrev ()
  (let ((url (fsvn-browse-point-url)))
    (when url
      (if fsvn-browse-repos-p
	  (fsvn-url-urlrev url (fsvn-browse-current-revision))
	url))))

(defun fsvn-browse-current-magic-directory ()
  (let ((url (fsvn-browse-current-repository-url)))
    (when url
      (fsvn-magic-create-name url))))

(defun fsvn-browse-point-magic-name ()
  (let ((urlrev (fsvn-browse-point-repository-urlrev)))
    (when urlrev
      (fsvn-magic-create-name urlrev))))

(defun fsvn-browse-current-path ()
  "Return top of buffer PATH."
  (save-excursion
    ;; current line is Path
    (forward-line 1)
    (cond
     ((or (re-search-backward fsvn-browse-re-subdir nil t)
	  ;; search first path
	  (re-search-forward fsvn-browse-re-subdir nil t))
      (match-string-no-properties 2)))))

(defun fsvn-browse-current-root ()
  "Current buffer's repository root."
  fsvn-buffer-repos-root)

(defun fsvn-browse-current-info ()
  "Current point source info."
  (let ((subdir (fsvn-string-assoc (fsvn-browse-current-path) fsvn-browse-subdir-alist)))
    (fsvn-browse-subdir.info subdir)))

(defun fsvn-browse-current-revision ()
  "Return current buffer indicate revision."
  fsvn-buffer-revision)

(defun fsvn-browse-current-directory ()
  (file-name-as-directory
   (cond
    ((not fsvn-browse-repos-p)
     (fsvn-browse-current-path))
    (t
     default-directory))))

(defun fsvn-browse-current-directory-url ()
  (cond
   ((not fsvn-browse-repos-p)
    (fsvn-browse-current-path))
   (t
    (fsvn-expand-url
     (fsvn-browse-current-path)
     (fsvn-browse-current-root)))))

(defun fsvn-browse-current-directory-urlrev ()
  "current point URL."
  (cond
   ((not fsvn-browse-repos-p)
    (fsvn-browse-current-path))
   (t
    (fsvn-url-urlrev
     (fsvn-browse-current-repository-url)
     (fsvn-browse-current-revision)))))

(defun fsvn-browse-current-repository-url ()
  (if fsvn-browse-repos-p
      (fsvn-expand-url
       (fsvn-browse-current-path)
       (fsvn-browse-current-root))
    (let* ((subdir (fsvn-browse-subdir (fsvn-browse-current-path) nil))
	   (info (fsvn-browse-subdir.info subdir)))
      (fsvn-xml-info->entry=>url$ info))))

;; ** interactive arguments

(defmacro fsvn-browse-cmd-wc-only (&rest form)
  `(if fsvn-browse-repos-p
       (error "this command only executable in working copy.")
     ,@form))

(defun fsvn-browse-cmd-this-file ()
  (let ((urlrev (fsvn-browse-point-urlrev)))
    (unless urlrev
      (error "No file on this line"))
    (list urlrev)))

(defun fsvn-browse-cmd-this-file-arg (subcommand &optional default-args)
  (let ((file (car (fsvn-browse-cmd-this-file)))
	(args
	 (if current-prefix-arg
	     (fsvn-read-svn-subcommand-args subcommand t default-args)
	   default-args)))
    (if args
	(list file args)
      (list file))))

(defun fsvn-browse-cmd-urlrev-list ()
  "Return selected files, for working copy or repository (url with revision).
This means svn editing subcommand (delete, add, move...) doesn't work."
  (list (fsvn-browse-cmd-target-list)))

(defun fsvn-browse-cmd-url-list ()
  "Return selected files, for working copy or repository (url with non-revision).
This means svn editing subcommand (delete, add, move...) work."
  (let ((fsvn-browse-file-name-function 'fsvn-browse-point-url))
    (list (fsvn-browse-cmd-target-list))))

(defun fsvn-browse-cmd-arg (subcommand &optional default-args)
  (let ((args
	 (if current-prefix-arg
	     (fsvn-read-svn-subcommand-args subcommand t default-args)
	   default-args)))
    (if args (list args) nil)))

(defun fsvn-browse-cmd-url-list-arg (subcommand &optional default-args)
  (let ((targets (car (fsvn-browse-cmd-url-list)))
	(args
	 (if current-prefix-arg
	     (fsvn-read-svn-subcommand-args subcommand t default-args)
	   default-args)))
    (if args
	(list targets args)
      (list targets))))

(defun fsvn-browse-cmd-target-list ()
  (let ((files (fsvn-browse-get-marked-files fsvn-mark-mark-char)))
    (unless files
      (when (setq files (funcall fsvn-browse-file-name-function))
	(setq files (list files))))
    (unless files
      (error "No file on this line"))
    files))

(defun fsvn-browse-this-file-struct ()
  (let ((name (fsvn-browse-point-urlrev))
	(dirp (fsvn-browse-point-directory-p)))
    (unless name
      (error "No file on this line"))
    (list (fsvn-struct-browse-file-make :name name :directory-p dirp))))

(defun fsvn-browse-cmd-copy/move-read-args (subcommand var)
  (if current-prefix-arg
      (fsvn-read-svn-subcommand-args subcommand t (symbol-value var))
    (symbol-value var)))

(defun fsvn-browse-cmd-read-copy-file ()
  (let ((from (fsvn-browse-point-urlrev))
	to args)
    (unless from
      (error "No file on this line"))
    (setq to (fsvn-read-file-under-versioned "Copy To: " from))
    (setq args (fsvn-browse-cmd-copy/move-read-args "copy" 'fsvn-default-args-copy))
    (list from to args)))

(defun fsvn-browse-cmd-read-copy-files ()
  (let ((files (car (fsvn-browse-cmd-urlrev-list)))
	dir args)
    (setq dir 
	  (fsvn-browse-dired-confirm files 'fsvn-browse-copy-selected 'fsvn-read-versioned-directory))
    (setq args (fsvn-browse-cmd-copy/move-read-args "copy" 'fsvn-default-args-copy))
    (list files dir args)))

(defun fsvn-browse-cmd-read-move-file ()
  (fsvn-browse-cmd-wc-only
   (let ((from (fsvn-browse-point-urlrev))
	 to args)
     (unless from
       (error "No file on this line"))
     (setq to (fsvn-read-file-under-versioned "Move To: " from))
     (setq args (fsvn-browse-cmd-copy/move-read-args "move" 'fsvn-default-args-move))
     (list from to args))))

(defun fsvn-browse-cmd-read-move-files ()
  (fsvn-browse-cmd-wc-only
   (let ((files (car (fsvn-browse-cmd-url-list)))
	 dir args)
     (setq dir 
	   (fsvn-browse-dired-confirm files 'fsvn-browse-move-selected 'fsvn-read-versioned-directory))
     (setq args (fsvn-browse-cmd-copy/move-read-args "move" 'fsvn-default-args-move))
     (list files dir args))))

(defun fsvn-browse-cmd-wc-arg (subcommand &optional default-args)
  (fsvn-browse-cmd-wc-only
   (fsvn-browse-cmd-arg subcommand default-args)))

(defun fsvn-browse-cmd-wc-files-arg (subcommand &optional default-args)
  (fsvn-browse-cmd-wc-only
   (fsvn-browse-cmd-url-list-arg subcommand default-args)))

(defun fsvn-browse-cmd-wc-files ()
  (fsvn-browse-cmd-wc-only
   (fsvn-browse-cmd-url-list)))

(defun fsvn-browse-cmd-wc-changelist-args ()
  "Return selected files in working copy and `changelist' name."
  (fsvn-browse-cmd-wc-only
   (let ((tmp (fsvn-browse-cmd-url-list)))
     (cons (fsvn-read-changelist-name) tmp))))

(defun fsvn-browse-cmd-wc-switch-args (default-args)
  "Return `switch' arguments name."
  (fsvn-browse-cmd-wc-only
     (let ((repository (fsvn-completing-read-url 
			"Switch to URL: "
			(fsvn-url-as-directory (fsvn-browse-current-repository-url))
			t))
	   (args
	    (if current-prefix-arg
		(fsvn-read-svn-subcommand-args "switch" t default-args)
	      default-args)))
    (if args
	(list repository args)
      (list repository)))))

(defun fsvn-browse-cmd-wc-resolve-args ()
  "Return selected files in working copy and `resolve' --accept args."
  (fsvn-browse-cmd-wc-only
   (let ((tmp (fsvn-browse-cmd-url-list)))
     (cons (fsvn-read-resolve-accept-arg) tmp))))

(defun fsvn-browse-cmd-read-svn:externals-selected ()
  (let ((src-files (car (fsvn-browse-cmd-urlrev-list)))
	dest confirmer)
    (cond
     ((= (length src-files) 1)
      (setq confirmer
	    (lambda (prompt)
	      (fsvn-read-file-under-versioned prompt nil)))
      (setq dest (fsvn-browse-dired-confirm 
		  src-files 'fsvn-browse-svn:externals-selected confirmer))
      (list src-files dest))
     (t
      (setq dest (fsvn-browse-dired-confirm 
		  src-files 'fsvn-browse-svn:externals-selected 'fsvn-read-versioned-directory))
      (list src-files dest)))))

;; * fsvn-browse-mode interactive command

(defun fsvn-browse-up-directory ()
  "Upward directory."
  (interactive)
  (let* ((path (fsvn-browse-current-path))
	 (filename (fsvn-file-name-nondirectory path))
	 (paren-dir (fsvn-file-name-directory2 path))
	 (prev-buffer (current-buffer)))
    (cond
     (fsvn-browse-repos-p
      (let ((above (fsvn-urlrev-dirname (fsvn-browse-current-directory-urlrev))))
	(fsvn-browse-switch-directory-buffer above))
      (fsvn-browse-goto-file filename))
     ((not (fsvn-file-versioned-directory-p path))
      (dired paren-dir)
      (dired-goto-file path))
     (t
      (fsvn-browse-switch-directory-buffer paren-dir)
      (fsvn-browse-goto-file filename)))
    (when fsvn-browse-cleanup-buffer
      (kill-buffer prev-buffer))))

(defun fsvn-browse-file-this (urlrev)
  "View file or directory."
  (interactive (list (fsvn-browse-point-urlrev)))
  (let ((prev-buffer (current-buffer)))
    ;;TODO symlink
    (cond
     ((fsvn-browse-point-file-p)
      (let* ((file
	      (if fsvn-browse-repos-p
		  (fsvn-magic-create-name urlrev)
		urlrev))
	     (prev (current-buffer)))
	(fsvn-view-buffer (fsvn-get-view-buffer file))))
     ((and urlrev
	   (fsvn-url-local-p urlrev))
      (dired urlrev)
      (when fsvn-browse-cleanup-buffer
	(kill-buffer prev-buffer)))
     ((fsvn-browse-point-directory-p)
      (fsvn-browse-switch-directory-buffer urlrev)
      (when fsvn-browse-cleanup-buffer
	(kill-buffer prev-buffer)))
     (t
      (message "No file on this line.")))))

(defun fsvn-browse-mark-file-delete ()
  "Put `delete' mark current file or directory in working copy."
  (interactive)
  (fsvn-browse-wc-only
   (fsvn-browse-put-mark-current-file fsvn-mark-delete-char)))

(defun fsvn-browse-mark-file-mark ()
  "Put mark current file or directory."
  (interactive)
  (fsvn-browse-put-mark-current-file fsvn-mark-mark-char))

(defun fsvn-browse-mark-file-unmark ()
  "Remote any mark current file or directory."
  (interactive)
  (fsvn-browse-put-mark-current-file nil))

(defun fsvn-browse-mark-all-unmark (char)
  "Remote any mark current file or directory."
  (interactive "cRemove marks (RET means all): ")
  (let ((regexp (format "^%c" (if (eq char 13) ?. char)))
	(case-fold-search nil)
	(count 0))
    (fsvn-browse-each-file file nil
      (when (save-excursion
	      (forward-line 0)
	      (and (looking-at regexp)
		   (not (looking-at "^ "))))
	(fsvn-browse-put-mark-point nil)
	(setq count (1+ count))))
    (message (if (= count 1) "1 mark removed"
	       "%d marks removed") count)))

(defun fsvn-browse-mark-file-regexp (regexp &optional mark)
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    " files (regexp): "))
	 (if current-prefix-arg fsvn-space-char fsvn-mark-mark-char)))
  (fsvn-browse-mark-matched-file regexp mark))

(defun fsvn-browse-mark-delete-regexp (regexp)
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    " files (regexp): "))))
  (fsvn-browse-mark-matched-file regexp fsvn-mark-delete-char))

(defun fsvn-browse-open-repository ()
  "Open repository by `fsvn-browse-mode'"
  (interactive)
  (unless (fsvn-browse-current-path)
    (error "This point has no directory"))
  (let* ((info (fsvn-browse-subdir.info (fsvn-browse-subdir (fsvn-browse-current-path) fsvn-browse-repos-p)))
	 (urlrev (fsvn-url-urlrev (fsvn-xml-info->entry=>url$ info) (fsvn-completing-read-revision)))
	 (file (fsvn-current-filename)))
    (fsvn-browse-switch-directory-buffer urlrev)
    (when file
      (fsvn-browse-goto-file file))))

(defun fsvn-browse-magic-head ()
  "Open repository by `fsvn-magic'."
  (interactive)
  (dired (fsvn-browse-current-magic-directory)))

(defun fsvn-browse-commit-selected (files &optional args)
  "Prepare `commit' buffer for selected FILES.
"
  (interactive (fsvn-browse-cmd-wc-files-arg "commit" fsvn-default-args-commit))
  (run-hook-with-args 'fsvn-browse-before-commit-hook files)
  (fsvn-browse-commit-mode files args))

(defun fsvn-browse-commit-path (&optional args)
  "Prepare `commit' buffer for changing files in this directory.
"
  (interactive (fsvn-browse-cmd-wc-arg "commit" fsvn-default-args-commit))
  (fsvn-browse-commit-selected (list (fsvn-browse-current-path))))

(defun fsvn-browse-cleanup-path (&optional args)
  (interactive (fsvn-browse-cmd-wc-arg "cleanup" fsvn-default-args-cleanup))
  (if (or (not (interactive-p))
	  current-prefix-arg
	  (fsvn-confirm-prompt 'fsvn-browse-cleanup-path "Svn: Cleanup directory? "))
      (let ((buffer (fsvn-make-temp-buffer))
	    proc)
	(setq proc (fsvn-start-command "cleanup" buffer args))
	(process-put proc 'fsvn-browse-cleaning-buffer (current-buffer))
	(setq fsvn-browse-buffer-cleanup-process proc)
	(fsvn-process-add-sentinel proc 'fsvn-browse-cleanup-process-sentinel)
	proc)
    (message "(No svn Cleanup performed)")))

(defun fsvn-browse-mkdir (directory)
  "Execute `mkdir' with `--parents'
"
  (interactive (list (fsvn-read-mkdir-directory (fsvn-browse-current-directory-url))))
  (if fsvn-browse-repos-p
      (fsvn-browse-mkdir-log-edit directory)
    (fsvn-call-process-with-popup "mkdir"
				  (when (fsvn-svn-subcommand-accepted-argument "mkdir" "--parents") "--parents")
				  directory)
    (fsvn-browse-add-wc-file-entry directory)))

(defun fsvn-browse-update-selected (files &optional args)
  "Execute `update' for selected FILES.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-wc-files-arg "update" fsvn-default-args-update))
  (if (or (not (interactive-p))
	  current-prefix-arg
	  (fsvn-browse-dired-confirm files 'fsvn-browse-update-selected))
      (let ((proc (fsvn-start-process-with-popup "update" args files)))
	(fsvn-process-add-filter proc 'fsvn-process-filter-for-update)
	proc)
    (message "(No svn Update performed)")))

(defun fsvn-browse-update-path (&optional args)
  "Execute `update' for current directory.
Optional ARGS (with prefix arg) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-wc-arg "update" fsvn-default-args-update))
  (if (or (not (interactive-p))
	  current-prefix-arg
	  (fsvn-confirm-prompt 'fsvn-browse-update-path "Svn: Update current directory? "))
      (let ((proc (fsvn-start-process-with-popup "update" args)))
	(fsvn-process-add-filter proc 'fsvn-process-filter-for-update)
	proc)
    (message "(No svn Update performed)")))

(defun fsvn-browse-switch-path (repository &optional args)
  "Execute `switch' for current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-wc-switch-args fsvn-default-args-switch))
  (fsvn-start-process-with-popup "switch" args repository))

(defun fsvn-browse-resolved-selected (files &optional args)
  "Execute `resolved' for current directory.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-wc-files-arg "resolved" fsvn-default-args-resolved))
  (if (or (not (interactive-p))
	  current-prefix-arg
	  (fsvn-browse-dired-confirm files 'fsvn-browse-resolved-selected))
      (fsvn-parse-result-instant-sentinel
       (fsvn-start-process-with-popup "resolved" args files)
       'fsvn-parse-result-cmd-resolved)
    (message "(No svn Resolved performed)")))

;;todo not implements add dialog recursively
;; (defun fsvn-browse-add-path (&optional args)
;;   "Execute `add' recursively for current directory with confirmation buffer.
;; Optional ARGS (with prefix arg) means read svn subcommand arguments.
;; "
;;   (interactive (fsvn-browse-cmd-wc-arg "add" fsvn-browse-add-path-args))
;;   (fsvn-browse-add-file-select  args)
;;   )

(defun fsvn-browse-add-selected (files &optional args)
  "Execute `add' for selected FILES.
If ARGS contains `--non-recursive' or `-N', then confirm buffer will be shown.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-wc-files-arg "add" fsvn-default-args-add))
  (cond
   ((and (not (fsvn-wc-files-only-non-recursive-p files))
	 (member "--non-recursive" args))
    (fsvn-browse-add-file-select files args))
   ((or (not (interactive-p))
	current-prefix-arg
	(fsvn-browse-dired-confirm files 'fsvn-browse-add-selected))
    (fsvn-parse-result-instant-sentinel
     (fsvn-start-process-with-popup "add" args files)
     'fsvn-parse-result-cmd-add))
   (t
    (message "(No svn Add performed)"))))

(defun fsvn-browse-delete-selected (files &optional args)
  "Execute `delete' for selected FILES.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-url-list-arg "delete" fsvn-default-args-delete))
  (cond
   (fsvn-browse-repos-p
    (fsvn-browse-delete-log-edit files args))
   ((or (not (interactive-p))
	current-prefix-arg
	(fsvn-browse-dired-confirm files 'fsvn-browse-delete-selected))
    (fsvn-parse-result-instant-sentinel
     (fsvn-start-process-with-popup "delete" args files)
     'fsvn-parse-result-cmd-delete))
   (t
    (message "(No svn Delete performed)"))))

(defun fsvn-browse-lock-selected (files &optional args)
  "Execute `lock' for selected FILES.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-url-list-arg "lock" fsvn-default-args-lock))
  (if (or (not (interactive-p))
	  current-prefix-arg
	  (fsvn-browse-dired-confirm files 'fsvn-browse-lock-selected))
      (fsvn-parse-result-cmd-lock
       (fsvn-call-process-multi-with-popup "lock" files args))
    (message "(No svn Lock performed)")))

(defun fsvn-browse-unlock-selected (files &optional args)
  "Execute `unlock' for selected FILES.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-url-list-arg "unlock" fsvn-default-args-unlock))
  (if (or (not (interactive-p))
	  current-prefix-arg
	  (fsvn-browse-dired-confirm files 'fsvn-browse-unlock-selected))
      (fsvn-parse-result-cmd-unlock
       (fsvn-call-process-multi-with-popup "unlock" files args))
    (message "(No svn Unlock performed)")))

(defun fsvn-browse-export-this (file to-file &optional args)
  "Execute `export' for point FILE to TO-FILE.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive 
   (let ((file (car (fsvn-browse-cmd-this-file))))
     (list
      file
      (fsvn-read-file-name "Export to: ")
      (fsvn-browse-cmd-arg "export" fsvn-default-args-export))))
  (fsvn-start-process-with-popup "export" file args to-file))

(defun fsvn-browse-export-path (to-dir &optional args)
  "Execute `export' for current directory to TO-DIR
Optional ARGS (with prefix arg) means read svn subcommand arguments.
"
  (interactive 
   (list 
    (fsvn-read-directory-name "Export to: ")
    (fsvn-browse-cmd-arg "export" fsvn-default-args-export)))
  (let ((from-dir (fsvn-browse-current-directory-urlrev)))
    (if (or (not (interactive-p))
	    current-prefix-arg
	    (fsvn-confirm-prompt 'fsvn-browse-export-path "Svn: Export current directory? "))
	(fsvn-start-process-with-popup "export" args from-dir to-dir)
      (message "(No svn Export performed)"))))

(defun fsvn-browse-add-changelist-selected (name files)
  (interactive (fsvn-browse-cmd-wc-changelist-args))
  (if (or (not (interactive-p))
	  (fsvn-browse-dired-confirm files 'fsvn-browse-add-changelist-selected))
      (fsvn-start-process-with-popup "changelist" name files)
    (message "(No svn Changelist performed)")))

(defun fsvn-browse-remove-changelist-selected (files)
  (interactive (fsvn-browse-cmd-wc-files))
  (if (or (not (interactive-p))
	  (fsvn-browse-dired-confirm files 'fsvn-browse-remove-changelist-selected))
      (fsvn-start-process-with-popup "changelist" "--remove" files)
    (message "(No svn Changelist performed)")))

(defun fsvn-browse-resolve-selected (accept-arg files)
  "Execute `resolve' for selected FILES."
  (interactive (fsvn-browse-cmd-wc-resolve-args))
  (fsvn-start-process-with-popup "resolve" "--accept" accept-arg files))

(defun fsvn-browse-move-this (src-file dest-file &optional args)
  "Execute `move' for point SRC-FILE to DEST-FILE.
Optional ARGS (with prefix arg) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-move-file))
  (fsvn-start-copy/move-process-with-popup "move" (list src-file) dest-file args))

;;NOTE 1.4.x cannot copy/move multiple files
(defun fsvn-browse-move-selected (src-files dest &optional args)
  "Execute `move' for selected SRC-FILES to DEST
Optional ARGS (with prefix arg) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-move-files))
  (fsvn-start-copy/move-process-with-popup "move" src-files dest args))

(defun fsvn-browse-copy-this (src-file dest-file &optional args)
  "Execute `copy' for point SRC-FILE to DEST-FILE.
Optional ARGS (with prefix arg) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-copy-file))
  (fsvn-start-copy/move-process-with-popup "copy" (list src-file) dest-file args))

;;NOTE 1.4.x cannot copy/move multiple files
(defun fsvn-browse-copy-selected (src-files dest &optional args)
  "Execute `copy' for selected SRC-FILES to DEST
Optional ARGS (with prefix arg) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-read-copy-files))
  (fsvn-start-copy/move-process-with-popup "copy" src-files dest args))

(defun fsvn-browse-safe-move-this (src-file dest-file &optional args)
  "Execute `move' for current file as SRC-FILE to DEST-FILE.
Same as `fsvn-browse-move-this' but allows you to DEST-FILE existence.
This means DEST-FILE contents will be preserved.
This is usefull for integrate other source management.
"
  (interactive (fsvn-browse-cmd-read-move-file))
  (if (not (file-exists-p dest-file))
      (apply 'fsvn-browse-move-this src-file dest-file args)
    (let (tmpfile)
      (cond
       ((and (fsvn-file-exact-file-p src-file)
	     (fsvn-file-exact-file-p dest-file)))
       ((and (fsvn-file-exact-file-p src-file)
	     (fsvn-file-exact-directory-p dest-file))
	(setq dest-file (fsvn-expand-file (fsvn-file-name-nondirectory src-file) dest-file)))
       ((and (fsvn-file-exact-directory-p src-file)
	     (fsvn-file-exact-directory-p dest-file)))
       (t
	(error "if SRC-FILE is directory, DEST-FILE must be a directory.")))
      (setq tmpfile (fsvn-make-temp-filename src-file))
      (rename-file dest-file tmpfile t)
      (unwind-protect 
	  (fsvn-call-process-with-popup "move" (list src-file) dest-file args)
	(cond
	 ((fsvn-file-exact-directory-p tmpfile)
	  (fsvn-copy-directory tmpfile dest-file))
	 (t
	  (rename-file tmpfile dest-file t)))))))

(defun fsvn-browse-safe-copy-this (src-file dest-file &optional args)
  "Execute `copy' for current file as SRC-FILE to DEST-FILE.
Same as `fsvn-browse-copy-this' but allows you to DEST-FILE existence.
This means DEST-FILE contents will be preserved.
This is usefull for integrate other source management.
"
  (interactive (fsvn-browse-cmd-read-copy-file))
  (if (not (file-exists-p dest-file))
      (apply 'fsvn-browse-copy-this src-file dest-file args)
    (let (tmpfile)
      (setq tmpfile (fsvn-make-temp-filename src-file))
      (rename-file dest-file tmpfile t)
      (unwind-protect 
	  (fsvn-call-process-with-popup "copy" (list src-file) dest-file args)
	(cond
	 ((fsvn-file-exact-directory-p tmpfile)
	  (fsvn-copy-directory tmpfile dest-file))
	 (t
	  (rename-file tmpfile dest-file t)))))))

(defun fsvn-browse-revert-selected (files &optional args)
  "Execute `revert' for selected FILES.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive (fsvn-browse-cmd-wc-files-arg "revert" fsvn-default-args-revert))
  (if (or (not (interactive-p))
	  current-prefix-arg
	  (fsvn-browse-dired-confirm files 'fsvn-browse-revert-selected))
      (fsvn-parse-result-instant-sentinel
       (fsvn-start-process-with-popup "revert" args files)
       'fsvn-parse-result-cmd-revert)
    (message "(No svn Revert performed)")))

(defun fsvn-browse-revert-path (&optional args)
  "Execute `revert' for current directory.
With \\[universal-argument] prefix, can read optional arguments.
"
  (interactive (fsvn-browse-cmd-wc-arg "revert" fsvn-default-args-revert))
  (if (or (not (interactive-p))
	  current-prefix-arg
	  (fsvn-confirm-prompt 'fsvn-browse-revert-path "Svn: Revert current directory? "))
      (fsvn-parse-result-instant-sentinel
       (fsvn-start-process-with-popup "revert" (fsvn-browse-current-directory-url) args)
       'fsvn-parse-result-cmd-revert)
    (message "(No svn Revert performed)")))

(defun fsvn-browse-log-this (file-struct)
  "Execute `log' for current file."
  (interactive (fsvn-browse-this-file-struct))
  (fsvn-open-log-view-mode
   (fsvn-struct-browse-file-get-name file-struct)
   (fsvn-struct-browse-file-get-directory-p file-struct)))

(defun fsvn-browse-log-path ()
  "Execute `log' for current directory."
  (interactive)
  (fsvn-open-log-view-mode (fsvn-browse-current-directory-urlrev) t))

(defun fsvn-browse-propview-this (file-struct)
  (interactive (fsvn-browse-this-file-struct))
  (fsvn-browse-propview-mode
   (fsvn-struct-browse-file-get-name file-struct)
   (fsvn-struct-browse-file-get-directory-p file-struct)))

(defun fsvn-browse-propview-path ()
  (interactive)
  (fsvn-browse-propview-mode (fsvn-browse-current-directory-url) t))

(defun fsvn-browse-info-selected (files &optional args)
  (interactive (fsvn-browse-cmd-url-list-arg "info" fsvn-default-args-info))
  (fsvn-call-process-multi-with-popup "info" files args))

(defun fsvn-browse-info-path (&optional args)
  (interactive (fsvn-browse-cmd-wc-arg "info" fsvn-default-args-info))
  (fsvn-call-process-with-popup "info" args))

(defun fsvn-browse-blame-this (file &optional args)
  (interactive (fsvn-browse-cmd-this-file-arg "blame" fsvn-default-args-blame))
  (fsvn-start-process-with-popup "blame" file args))

(defun fsvn-browse-add-prop-svn:ignore (files)
  (interactive (fsvn-browse-cmd-wc-files))
  (if (or (not (interactive-p))
	  (fsvn-browse-dired-confirm files 'fsvn-browse-add-prop-svn:ignore))
      (mapc
       (lambda (cell)
	 (fsvn-add-prop-svn:ignore (car cell) (cdr cell)))
       (fsvn-group-by-directory files))
    (message "(No svn Ignore performed)")))

(defun fsvn-browse-toggle-prop-svn:needs-lock (files)
  (interactive (fsvn-browse-cmd-url-list))
  (if (or (not (interactive-p))
	  (fsvn-browse-dired-confirm files 'fsvn-browse-toggle-prop-svn:needs-lock))
      (mapc
       (lambda (file)
	 (fsvn-set-prop-svn:needs-lock file (not (fsvn-get-prop-svn:needs-lock file))))
       files)
    (message "(No svn Needs-Lock performed)")))

(defun fsvn-browse-toggle-sort ()
  (interactive)
  (let ((key
	 (if fsvn-browse-repos-p 'repository 'working-copy)))
    (setq fsvn-browse-ls-comparer
	  (fsvn-cycle-next
	   (cdr (assq key fsvn-browse-ls-comparator-alist))
	   fsvn-browse-ls-comparer))
    (revert-buffer)))

(defun fsvn-browse-diff-base (file &optional args)
  (interactive (fsvn-browse-cmd-this-file-arg "diff" fsvn-default-args-diff))
  (fsvn-browse-wc-only
   (let ((diff-args (list file args)))
     (fsvn-diff-call-process diff-args))))

(defun fsvn-browse-ediff-base (file)
  (interactive (fsvn-browse-cmd-this-file))
  (fsvn-browse-wc-only
   (when (file-directory-p file)
     (error "\"%s\" is directory" file))
   (let* ((urlrev (fsvn-url-urlrev file "BASE"))
	  (tmpfile (fsvn-ediff-make-temp-file urlrev)))
     (unless (fsvn-save-file urlrev tmpfile t)
       (error "Save file failed"))
     (fsvn-ediff-files tmpfile file))))

(defun fsvn-browse-diff-local (file)
  "Same as `dired-diff'."
  (interactive (fsvn-browse-cmd-this-file))
  (let ((file2 (fsvn-read-file-name "Local file: " nil nil t))
	(switches
	 (if (stringp diff-switches)
	     diff-switches
	   (mapconcat 'identity diff-switches " "))))
    (diff file2 file switches)))

(defun fsvn-browse-rename-case-missing-file (file)
  "This occation if windows environment."
  (interactive (fsvn-browse-cmd-this-file))
  (fsvn-rename-case-missing-file file))

;; not works 1.4.x
(defun fsvn-browse-svn:externals-selected (src-files dest)
  "Create `svn:externals' property SRC-FILES to DEST
DEST must be a directory when SRC-FILES is multiple.
When SRC-FILES is single list, DEST allows non existence filename."
  (interactive (fsvn-browse-cmd-read-svn:externals-selected))
  (let (targetdir prop externals value)
    ;;todo
    (cond
     ((fsvn-file-exact-directory-p dest)
      (setq targetdir dest)
      (setq prop (fsvn-get-propget "svn:externals" targetdir))
      (when prop
	(setq externals
	      (split-string prop "\n" t)))
      (mapc
       (lambda (src)
	 (setq value (format "%s %s" (fsvn-file-relative src targetdir) (fsvn-file-name-nondirectory src)))
	 (setq externals (cons value externals)))
       src-files))
     ((file-exists-p dest)
      (error "Destination file already exists."))
     (t
      (setq targetdir (fsvn-file-name-directory dest))
      (setq prop (fsvn-get-propget "svn:externals" targetdir))
      (when prop
	(setq externals
	      (split-string prop "\n" t)))
      (setq value (format "%s %s" (fsvn-file-relative (car src-files) targetdir) (fsvn-file-name-nondirectory dest)))
      (setq externals (cons value externals))))
    (fsvn-start-process-with-popup "propset" "svn:externals" (mapconcat 'identity externals "\n") targetdir)
    ;;todo popup something
    ;;     (setq prop (fsvn-get-propget "svn:externals" targetdir))
    ;;todo parse current prop and check it
    ;;     (split-string prop "\n" t)
    ;;todo other repository
    ))

(defun fsvn-browse-search-guessed-moved-files (file file-versioned-p)
  ;;todo hard coding
  (let ((dir (fsvn-file-name-parent-directory file 4)))
    (fsvn-mapitem
     (lambda (f)
       (let ((versioned (fsvn-get-ls f)))
	 (cond
	  ((and file-versioned-p versioned))
	  ((and (not file-versioned-p) (null versioned)))
	  (t
	   f))))
    (fsvn-search-same-name-files dir file 6))))

(defun fsvn-browse-search-moved/copied-file (target-file)
  (interactive (fsvn-browse-cmd-this-file))
  (let (files
	src-file dest-file file 
	file-versioned target-versioned done)
    (setq target-versioned (fsvn-get-ls target-file))
    (if target-versioned
	(setq src-file target-file)
      (setq dest-file target-file))
    (setq files (fsvn-browse-search-guessed-moved-files target-file target-versioned))
    (while files
      (setq file (car files))
      ;;todo interactive command
      (setq file-versioned (fsvn-get-ls file))
      (if target-versioned
	  (setq dest-file file)
	(setq src-file file))
      (cond
       ((y-or-n-p (format "Move %s to %s? " src-file dest-file))
	(fsvn-browse-safe-move-this src-file dest-file)
	(setq done t)
	(setq files nil))
       ((y-or-n-p (format "Copy %s to %s? " src-file dest-file))
	(fsvn-browse-safe-copy-this src-file dest-file)
	(setq done t)
	(setq files nil)))
      (setq files (cdr files)))
  (unless done
    (message "Cannot do anything."))))



(put 'fsvn-browse-each-file 'lisp-indent-function 2)
(put 'fsvn-browse-with-move-dir-status 'lisp-indent-function 1)
(put 'fsvn-browse-with-move-status 'lisp-indent-function 2)

(provide 'fsvn-browse)

;;; fsvn-browse.el ends here
