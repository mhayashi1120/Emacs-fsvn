;;; fsvn-dev.el --- Experimental implementation.

;;; History:
;; 

;;; Commentary:
;; 

;;; Code:



;; destination url -> msgedit
;; mv -> switch
(defun fsvn-browse-move-this-in-repository (src-file to-url &optional args)
  "Execute `move' for repository file corresponding local file.
Optional ARGS (with \\[universal-argument]) means read svn subcommand arguments.
"
  (interactive)
  )



(defcustom fsvn-browse-guessed-moved-parent-threshold 4
  "*"
  :group 'fsvn
  :type 'integer)

(defun fsvn-browse-search-guessed-moved-files (file file-versioned-p)
  (let ((dir (fsvn-file-name-parent-directory file fsvn-browse-guessed-moved-parent-threshold)))
    (fsvn-mapitem
     (lambda (f)
       (let ((versioned (fsvn-get-ls f)))
	 (cond
	  ((and file-versioned-p versioned))
	  ((and (not file-versioned-p) (null versioned)))
	  (t
	   f))))
     ;;TODO hard-coding
    (fsvn-search-same-name-files dir file 6))))

(defun fsvn-browse-search-moved/copied-file (target-file)
  (interactive (fsvn-browse-cmd-read-wc-this-file))
  (fsvn-browse-wc-only
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
       (setq file-versioned (fsvn-get-ls file))
       (if target-versioned
	   (setq dest-file file)
	 (setq src-file file))
       ;;todo interactive command
       ;;todo electric
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
       (message "Cannot do anything.")))))



;; TODO similar to fsvn-get-files-logs
(defun fsvn-logs-multiple-url (urlrevs)
  "Gather non-duplicated log entries."
  (let (entries)
    (mapc
     (lambda (urlrev)
       (let ((entry (fsvn-get-file-logs urlrev)))
	 (setq entries (fsvn-logs-unique-merge entries entry))))
     urlrevs)
    (sort entries (lambda (l1 l2) (< (fsvn-xml-log->logentry.revision l1) (fsvn-xml-log->logentry.revision l2))))))



(add-hook 'fsvn-log-list-mode-hook
	  (lambda ()
	    (define-key fsvn-log-list-mode-map "\C-c\C-t" 'fsvn-log-list-revert-to-revision)))

(defun fsvn-log-list-revert-to-revision (urlrev local-file)
  (interactive (fsvn-log-list-cmd-read-revert-to-revision))
  (fsvn-async-let ((urlrev urlrev)
		   (local-file local-file))
    (fsvn-popup-start-process "delete" (list local-file))
    (fsvn-popup-start-copy/move-process "copy" (list urlrev) local-file)))



;; testing

(defconst fsvn-xml-accessor-prefix "fsvn-xml-")

(defun fsvn-xml-create-accessor (dtd paren multi-nodes)
  (let* ((base-nodes (append paren (list (car dtd))))
	 (base-name (concat fsvn-xml-accessor-prefix (fsvn-xml-create-accessor-node base-nodes multi-nodes)))
	 (attrs (fsvn-xml-node-attributes dtd))
	 (name (symbol-name (car dtd)))
	 (children (fsvn-xml-node-children dtd)))
    (list
     (mapcar
      (lambda (attr)
	(concat base-name  "." (symbol-name (car attr))))
      attrs)
     (cond
      ((atom children)
       (concat fsvn-xml-accessor-prefix (fsvn-xml-create-accessor-node paren multi-nodes) "=" name))
      (t
       (mapcar
	(lambda (child)
	  (fsvn-xml-create-accessor child base-nodes multi-nodes))
	children))))))

(defun fsvn-xml-create-accessor-node (paren multi-nodes)
  (let (seq)
    (setq seq (fsvn-xml-accessor-multi-most-match paren multi-nodes))
    (cond
     ((or (null seq)
	  (equal seq paren))
      (mapconcat 'symbol-name paren "->"))
     (t
      (mapconcat 'symbol-name seq "=>")))))

(defun fsvn-xml-accessor-multi-most-match (nodes multi-nodes)
  (let (max)
    (mapc
     (lambda (seq)
       (let ((len (length seq))
	     (i 0)
	     node)
	 (catch 'unmatch
	   (while (< i len)
	     (setq node (nth i nodes))
	     (unless (eq node (nth i seq))
	       (throw 'unmatch t))
	     (setq i (1+ i)))
	   (when (> len (length max))
	     (setq max seq)))))
     multi-nodes)
    max))



(defvar fsvn-bugreport-mail-address "mhayashi1120@gmail.com")

;;TODO
(defvar fsvn-bugreport-salutation
  "
# If you are a Japanese, please write in Japanese:-)

Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of fsvn*.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"fsvn.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
")

(defun fsvn-bugreport ()
  (interactive)
  (let ((pkgname "fsvn.el"))
    (reporter-submit-bug-report
     fsvn-maintainer-mail-address
     pkgname
     (apropos-internal "^fsvn-" 'boundp)
     nil nil fsvn-bugreport-salutation)
    (mail-position-on-field "subject")
    (insert pkgname "; Bug report" )
    (unless (y-or-n-p 
	     (concat 
	      "This bug report may contain privacy information (Like password).\n"
	      "Please delete manually. OK? " ))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))




(require 'easymenu)

(defconst fsvn-browse-mode-menu-spec
  '("fsvn"
    ["Update" fsvn-browse-update-path t]
    ["Commit" fsvn-browse-commit-path t]
    ["Log" fsvn-browse-logview-path t]
    ["Diff" fsvn-browse-diff-this t]
    "----"
    ("Manipulate Current Directory"
     ["Commit" fsvn-browse-commit-path t]
     ["Export" fsvn-browse-export-path t]
     ["Info" fsvn-browse-info-path t]
     ["Log" fsvn-browse-logview-path t]
     ["Merge" fsvn-browse-merge-path t]
     ["Mkdir" fsvn-browse-mkdir t]
     ["Proplist" fsvn-browse-propview-path t]
     ["Revert" fsvn-browse-revert-path t]
     ["Update" fsvn-browse-update-path t]
     ["Switch" fsvn-browse-switch-path t]
     ["Cleanup" fsvn-browse-cleanup-path t]
     ["Upgrade source tree" fsvn-browse-upgrade-source-tree t]
     )
    ("Repository Browser"
     ["Magic" fsvn-browse-magic-head t]
     ["Browser" fsvn-browse-open-repository t]
     ["Mergeinfo" fsvn-browse-mergeinfo-path t]
     ["Branch" fsvn-browse-create-branch t]
     ["Tag" fsvn-browse-create-tag t]
     )
    ("Manipulate Files"
     ["Delete selected" fsvn-browse-delete-selected t]
     ["Add selected" fsvn-browse-add-selected t]
     ["Revert selected" fsvn-browse-revert-selected t]
     ["Copy selected" fsvn-browse-copy-selected t]
     ["Move selected" fsvn-browse-move-selected t]
     ["Lock selected" fsvn-browse-lock-selected t]
     ["Unlock selected" fsvn-browse-unlock-selected t]
     ["Update selected" fsvn-browse-update-selected t]
     ["Add to changelist" fsvn-browse-add-changelist-selected t]
     ["Remove from changelist" fsvn-browse-remove-changelist-selected t]
     ["Commit selected" fsvn-browse-commit-selected t]
     ["Info selected" fsvn-browse-info-selected t]
     ["Externals" fsvn-browse-svn:externals-selected t]
     ["Resolved selected" fsvn-browse-resolved-selected t]
     ["Resolve selected" fsvn-browse-resolve-selected t]
     ["Toggle svn:needs-lock selected" fsvn-browse-prop-toggle-svn:needs-lock-selected t]
     ["Toggle svn:ignore selected" fsvn-browse-prop-add-svn:ignore-selected t]
     )
    ("Manipulate A File"
     ["Proplist of point" fsvn-browse-propview-this t]
     ["Merge to point" fsvn-browse-merge-this t]
     ["Mergeinfo of point" fsvn-browse-mergeinfo-this t]
     ["EDiff" fsvn-browse-ediff-this t]
     ["Diff" fsvn-browse-diff-this t]
     ["Blame/Annotate" fsvn-browse-blame-this t]
     ["Export" fsvn-browse-export-this t]
     ["Log" fsvn-browse-logview-this t]
     ["Copy" fsvn-browse-copy-this t]
     ["Move" fsvn-browse-move-this t]
     ["Copy in repository" fsvn-browse-copy-this-in-repository t]
     ["Safe Copy" fsvn-browse-safe-copy-this t]
     ["Safe Move" fsvn-browse-safe-move-this t]
     ["Fix Filename Case" fsvn-browse-rename-case-missing-file t]
     ["Paste properties" fsvn-browse-paste-properties-to-this t]
     )
    "----"
    ("Emmulate Dired"
     ["Mark Regexp" fsvn-browse-mark-file-regexp t]
     ["Mark Delete Regexp" fsvn-browse-mark-delete-regexp t]
     ["Unmark All" fsvn-browse-mark-all-unmark t]
     ["Unmark File" fsvn-browse-mark-file-unmark t]
     ["Mark Delete" fsvn-browse-mark-file-delete t]
     ["Mark" fsvn-browse-mark-file-mark t]
     ["Diff" fsvn-browse-diff-local t]
     ["Next" fsvn-browse-next-file t]
     ["Previous" fsvn-browse-previous-file t]
     ["Sort" fsvn-browse-toggle-sort t]
     ["View" fsvn-browse-file-this t]
     ["Up" fsvn-browse-up-directory t]
     )
    ))

(easy-menu-define fsvn-browse-mode-menu
  fsvn-browse-mode-map
  "Menu used in Fsvn Browse mode."
  fsvn-browse-mode-menu-spec)

(defconst fsvn-log-list-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-log-list-mode-menu
  fsvn-log-list-mode-map
  "Menu used in Fsvn Log List mode."
  fsvn-log-list-mode-menu-spec)

(defconst fsvn-log-sibling-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-log-sibling-mode-menu
  fsvn-log-sibling-mode-map
  "Menu used in Fsvn Log Sibling mode."
  fsvn-log-sibling-mode-menu-spec)

(defconst fsvn-log-message-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-log-message-mode-menu
  fsvn-log-message-mode-map
  "Menu used in Fsvn Log Message mode."
  fsvn-log-message-mode-menu-spec)

(defconst fsvn-proplist-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-proplist-mode-menu
  fsvn-proplist-mode-map
  "Menu used in Fsvn Property List mode."
  fsvn-proplist-mode-menu-spec)

(defconst fsvn-propedit-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-propedit-mode-menu
  fsvn-propedit-mode-map
  "Menu used in Fsvn Property Edit mode."
  fsvn-propedit-mode-menu-spec)

(defconst fsvn-process-list-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-process-list-mode-menu
  fsvn-process-list-mode-map
  "Menu used in Fsvn Processes mode."
  fsvn-process-list-mode-menu-spec)

(defconst fsvn-popup-result-mode-menu-spec
  '("fsvn"
     ["Kill running process" fsvn-popup-result-kill-process t]
     ["Send password" fsvn-popup-result-send-password t]
     ["Send input string to process" fsvn-popup-result-send-string t]
    ))

(easy-menu-define fsvn-popup-result-mode-menu
  fsvn-popup-result-mode-map
  "Menu used in Fsvn Result mode."
  fsvn-popup-result-mode-menu-spec)

(defconst fsvn-select-file-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-select-file-mode-menu
  fsvn-select-file-mode-map
  "Menu used in Fsvn File Select mode."
  fsvn-select-file-mode-menu-spec)

(defconst fsvn-message-edit-mode-menu-spec
  '("fsvn"

    ))

(easy-menu-define fsvn-message-edit-mode-menu
  fsvn-message-edit-mode-map
  "Menu used in Fsvn Log Message Edit mode."
  fsvn-message-edit-mode-menu-spec)




(provide 'fsvn-dev)

;;; fsvn-dev.el ends here
