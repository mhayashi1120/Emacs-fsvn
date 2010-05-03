;;; fsvn-xml.el --- XML utility for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

;;; Code:
;;



(require 'xml)



;; xml utility

(defun fsvn-xml-get-node (node)
  (let ((lst (fsvn-xml-node-children node)))
    (when (> (length lst) 1)
      (error "Node is not atom"))
    (car lst)))

(defun fsvn-xml-get-child (node child-name)
  (car (xml-get-children node child-name)))

(defun fsvn-xml-get-children (node child-name)
  (xml-get-children node child-name))

(defun fsvn-xml-node-attributes (node)
  (xml-node-attributes node))

(defun fsvn-xml-get-attribute (node attribute)
  (xml-get-attribute node attribute))

(defun fsvn-xml-get-attribute-or-nil (node attribute)
  (xml-get-attribute-or-nil node attribute))

(defun fsvn-xml-get-atom-child (node key)
  (fsvn-xml-get-node (assq key node)))

(defun fsvn-xml-get-text (node index)
  (let (ret)
    (mapc
     (lambda (n)
       (when (stringp n)
	 (setq ret (cons n ret))))
     (fsvn-xml-node-children node))
    (nth index (nreverse ret))))

(defun fsvn-xml-get-atom-child-safe (node key)
  (and node (fsvn-xml-get-node (assq key node))))

(defun fsvn-xml-get-atom-child-nullable (node key)
  (if (assq key node)
      (fsvn-xml-get-atom-child node key)
    nil))

(defun fsvn-xml-node-children (node)
  (xml-node-children node))



;; xml definition

(defconst fsvn-xml-proplist-dtd-alist
  '(properties
    nil
    (target
     ((path . fsvn-expand-file))
     (property
      ((name . identity))))
    (revprops
     ((rev . string-to-number))
     (property
      ((name . identity) (encoding . identity))))))

(defconst fsvn-xml-proplist-multiple-nodes
  '((properties target)
    (properties target property)))

(defconst fsvn-xml-info-dtd-alist
  '(info
    nil
    (entry
     ((kind . intern) (revision . string-to-number) (path . identity))
     (url nil . t)
     (commit
      ((revision . string-to-number))
      (date nil . fsvn-svn-parse-date)
      (author nil . t))
     (repository
      nil
      (root nil . t)
      (uuid nil . t))
     (wc-info
      nil
      (schedule nil . intern)
      (depth nil . intern)))))

(defconst fsvn-xml-info-multiple-nodes
  '((info entry)))

(defconst fsvn-xml-ls-dtd-alist
  '(lists
    nil
    (list
     nil
     (entry
      ((kind . intern))
      (name nil . t)
      (size nil . fsvn-string-force-number)
      (commit
       ((revision . string-to-number))
       (date nil . fsvn-svn-parse-date)
       (author nil . t))
      (lock
       nil
       (token nil . t)
       (owner nil . t)
       (created nil . fsvn-svn-parse-date))))))

(defconst fsvn-xml-ls-multiple-nodes
  '((lists list entry)))

(defconst fsvn-xml-status-entry-dtd-alist
  '(entry
    ((path . fsvn-expand-file))
    (wc-status
     ((props . intern) (switched . intern) (item . intern)
      (revision . string-to-number) (wc-locked . intern)
      (copied . fsvn-svn-parse-boolean) (tree-conflicted . fsvn-svn-parse-boolean))
     (commit
      ((revision . string-to-number))
      (author nil . t)
      (date nil . fsvn-svn-parse-date))
     (lock
      nil
      (token nil . t)
      (owner nil . t)
      (created nil . fsvn-svn-parse-date)))
    (repos-status
     ((props . intern) (item . intern))
     (lock
      nil
      (token nil . t)
      (owner nil . t)
      (created nil . fsvn-svn-parse-date)))))

(defconst fsvn-xml-status-dtd-alist
  `(status
    nil
    (target
     ((path . fsvn-expand-file))
     ,fsvn-xml-status-entry-dtd-alist
     (against
      ((revision . string-to-number))))
    (changelist
     ((path . fsvn-expand-file))
     ,fsvn-xml-status-entry-dtd-alist
     (against
      ((revision . string-to-number))))))

(defconst fsvn-xml-status-multiple-nodes
  '((status target)
    (status changelist)))

(defconst fsvn-xml-log-dtd-alist
  '(log
    nil
    (logentry
     ((revision . string-to-number))
     (date nil . fsvn-svn-parse-date)
     (author nil . t)
     (paths
      nil
      (path
       ((action . identity)
	(copyfrom-rev . string-to-number))
       . t))
     (msg nil . t))))

(defconst fsvn-xml-log-multiple-nodes
  '((log logentry)
    (log logentry paths)))

(defconst fsvn-xml-blame-dtd-alist
  '(blame
    nli
    (target
     ((path . fsvn-expand-file))
     (entry
      ((line-number . string-to-number))
      (commit
       ((revision . string-to-number))
       (author nil . t)
       (date nil . fsvn-svn-parse-date))))))

(defconst fsvn-xml-blame-multiple-nodes
  '((blame target)
    (blame target entry)))



;; svn ls --xml

(defun fsvn-safe-xml-lists->list->entry=>size$ (node)
  (if (eq (fsvn-xml-lists->list->entry.kind node) 'dir)
      0
    (fsvn-xml-lists->list->entry=>size$ node)))

(defun fsvn-xml-lists->list.path (node)
  (fsvn-xml-get-attribute node 'path))

(defun fsvn-xml-lists->list->entry.kind (node)
  (fsvn-xml-get-attribute node 'kind))

(defun fsvn-xml-lists->list->entry=>name$ (node)
  (fsvn-xml-get-atom-child node 'name))

(defun fsvn-xml-lists->list->entry=>size$ (node)
  (fsvn-xml-get-atom-child node 'size))

(defun fsvn-xml-lists->list->entry=>commit (node)
  (fsvn-xml-get-child node 'commit))

(defun fsvn-xml-lists->list->entry=>commit.revision (node)
  (fsvn-xml-get-attribute
   (fsvn-xml-lists->list->entry=>commit node) 'revision))

(defun fsvn-xml-lists->list->entry=>commit=>author$ (node)
  (fsvn-xml-get-atom-child
   (fsvn-xml-lists->list->entry=>commit node) 'author))

(defun fsvn-xml-lists->list->entry=>commit=>date$ (node)
  (fsvn-xml-get-atom-child
   (fsvn-xml-lists->list->entry=>commit node) 'date))

(defun fsvn-xml-lists->list->entry=>lock (node)
  (fsvn-xml-get-child node 'lock))

(defun fsvn-xml-lists->list->entry=>lock=>token$ (node)
  (fsvn-xml-get-atom-child-nullable
   (fsvn-xml-lists->list->entry=>lock node) 'token))

(defun fsvn-xml-lists->list->entry=>lock=>owner$ (node)
  (fsvn-xml-get-atom-child-nullable
   (fsvn-xml-lists->list->entry=>lock node) 'owner))

(defun fsvn-xml-lists->list->entry=>lock=>created$ (node)
  (fsvn-xml-get-atom-child-nullable
   (fsvn-xml-lists->list->entry=>lock node) 'created))

;; svn ls --xml end


;; svn info --xml

(defun fsvn-xml-info->entry.kind (node)
  (fsvn-xml-get-attribute node 'kind))

(defun fsvn-xml-info->entry.path (node)
  (fsvn-xml-get-attribute node 'path))

(defun fsvn-xml-info->entry.revision (node)
  (fsvn-xml-get-attribute node 'revision))

(defun fsvn-xml-info->entry=>url$ (node)
  (fsvn-xml-get-atom-child node 'url))

(defun fsvn-xml-info->entry=>repository (node)
  (fsvn-xml-get-child node 'repository))

(defun fsvn-xml-info->entry=>repository=>root$ (node)
  (fsvn-xml-get-atom-child
   (fsvn-xml-info->entry=>repository node) 'root))

(defun fsvn-xml-info->entry=>repository=>uuid$ (node)
  (fsvn-xml-get-atom-child
   (fsvn-xml-info->entry=>repository node) 'uuid))

(defun fsvn-xml-info->entry=>wc-info (node)
  (fsvn-xml-get-child node 'wc-info))

(defun fsvn-xml-info->entry=>wc-info=>schedule$ (node)
  (fsvn-xml-get-atom-child
   (fsvn-xml-info->entry=>wc-info node) 'schedule))

(defun fsvn-xml-info->entry=>wc-info=>depth$ (node)
  (fsvn-xml-get-atom-child
   (fsvn-xml-info->entry=>wc-info node) 'depth))

(defun fsvn-xml-info->entry=>commit (node)
  (fsvn-xml-get-child node 'commit))

(defun fsvn-xml-info->entry=>commit.revision (node)
  (fsvn-xml-get-attribute
   (fsvn-xml-info->entry=>commit node)
   'revision))

(defun fsvn-xml-info->entry=>commit=>author$ (node)
  (fsvn-xml-get-atom-child
   (fsvn-xml-info->entry=>commit node) 'author))

(defun fsvn-xml-info->entry=>commit=>date$ (node)
  (fsvn-xml-get-atom-child
   (fsvn-xml-info->entry=>commit node) 'date))

;; svn info --xml end


;; svn status --xml

(defun fsvn-xml-status->target.path (node)
  (fsvn-xml-get-attribute node 'path))

(defun fsvn-xml-status->entries (node)
  (let (ret tmp)
    (mapc
     (lambda (n)
       (setq tmp (fsvn-xml-status->target&cl->entries n))
       (setq ret (append tmp ret)))
     node)
    (nreverse ret)))

(defun fsvn-xml-status->target&cl->entries (node)
  (fsvn-xml-get-children node 'entry))

(defun fsvn-xml-status->target->entry.path (node)
  (fsvn-xml-get-attribute node 'path))

(defun fsvn-xml-status->target->entry=>wc-status (node)
  (fsvn-xml-get-child node 'wc-status))

(defun fsvn-xml-status->target->entry=>wc-status.props (node)
  (fsvn-xml-get-attribute
   (fsvn-xml-status->target->entry=>wc-status node) 'props))

(defun fsvn-xml-status->target->entry=>wc-status.switched (node)
  (fsvn-xml-get-attribute-or-nil
   (fsvn-xml-status->target->entry=>wc-status node) 'switched))

(defun fsvn-xml-status->target->entry=>wc-status.file-external (node)
  (fsvn-xml-get-attribute-or-nil
   (fsvn-xml-status->target->entry=>wc-status node) 'file-external))

(defun fsvn-xml-status->target->entry=>wc-status.item (node)
  (fsvn-xml-get-attribute
   (fsvn-xml-status->target->entry=>wc-status node) 'item))

(defun fsvn-xml-status->target->entry=>wc-status.revision (node)
  (fsvn-xml-get-attribute
   (fsvn-xml-status->target->entry=>wc-status node) 'revision))

(defun fsvn-xml-status->target->entry=>wc-status.copied (node)
  (fsvn-xml-get-attribute-or-nil
   (fsvn-xml-status->target->entry=>wc-status node) 'copied))

(defun fsvn-xml-status->target->entry=>wc-status.tree-conflicted (node)
  (fsvn-xml-get-attribute-or-nil
   (fsvn-xml-status->target->entry=>wc-status node) 'tree-conflicted))

(defun fsvn-xml-status->target->entry=>wc-status=>lock (node)
  (fsvn-xml-get-child
   (fsvn-xml-status->target->entry=>wc-status node) 'lock))

(defun fsvn-xml-status->target->entry=>wc-status=>lock=>token$ (node)
  (fsvn-xml-get-atom-child-safe
   (fsvn-xml-status->target->entry=>wc-status=>lock node) 'token))

(defun fsvn-xml-status->target->entry=>wc-status=>lock=>owner$ (node)
  (fsvn-xml-get-atom-child-safe
   (fsvn-xml-status->target->entry=>wc-status=>lock node) 'owner))

(defun fsvn-xml-status->target->entry=>wc-status=>lock=>created$ (node)
  (fsvn-xml-get-atom-child-safe
   (fsvn-xml-status->target->entry=>wc-status=>lock node) 'created))

(defun fsvn-xml-status->target->entry=>wc-status=>commit (node)
  (fsvn-xml-get-child
   (fsvn-xml-status->target->entry=>wc-status node) 'commit))

(defun fsvn-xml-status->target->entry=>wc-status=>commit.revision (node)
  (fsvn-xml-get-attribute
   (fsvn-xml-status->target->entry=>wc-status=>commit node) 'revision))

(defun fsvn-xml-status->target->entry=>wc-status=>commit=>author$ (node)
  (fsvn-xml-get-atom-child-safe
   (fsvn-xml-status->target->entry=>wc-status=>commit node) 'author))

(defun fsvn-xml-status->target->entry=>wc-status=>commit=>date$ (node)
  (fsvn-xml-get-atom-child-safe
   (fsvn-xml-status->target->entry=>wc-status=>commit node) 'date))

(defun fsvn-xml-status->target->entry=>repos-status (node)
  (fsvn-xml-get-child node 'repos-status))

(defun fsvn-xml-status->target->entry=>repos-status.props (node)
  (fsvn-xml-get-attribute
   (fsvn-xml-status->target->entry=>repos-status node) 'props))

(defun fsvn-xml-status->target->entry=>repos-status.item (node)
  (fsvn-xml-get-attribute
   (fsvn-xml-status->target->entry=>repos-status node) 'item))

(defun fsvn-xml-status->target->entry=>repos-status=>lock (node)
  (fsvn-xml-get-child
   (fsvn-xml-status->target->entry=>repos-status node) 'lock))

(defun fsvn-xml-status->target=>entry->repos-status=>lock=>token$ (node)
  (fsvn-xml-get-atom-child-safe
   (fsvn-xml-status->target->entry=>repos-status=>lock node) 'token))

(defun fsvn-xml-status->target->entry=>repos-status=>lock=>owner$ (node)
  (fsvn-xml-get-atom-child-safe
   (fsvn-xml-status->target->entry=>repos-status=>lock node) 'owner))

(defun fsvn-xml-status->target->entry=>repos-status=>lock=>comment$ (node)
  (fsvn-xml-get-attribute
   (fsvn-xml-status->target->entry=>repos-status=>lock node) 'comment))

(defun fsvn-xml-status->target->entry=>repos-status=>lock=>created$ (node)
  (fsvn-xml-get-atom-child-safe
   (fsvn-xml-status->target->entry=>repos-status=>lock node) 'created))

(defun fsvn-xml-status-all-entries (status)
  (let (ret)
    (mapc
     (lambda (target&cl)
       (mapc
	(lambda (entry)
	  (setq ret (cons entry ret)))
	(fsvn-xml-status->target&cl->entries target&cl)))
     (copy-sequence status))
    (sort ret
	  (lambda (x y)
	    (string-lessp
	     (fsvn-xml-status->target->entry.path x)
	     (fsvn-xml-status->target->entry.path y))))))

;; svn status --xml end



;; svn proplist --xml start

(defun fsvn-xml-properties->target.path (node)
  (fsvn-xml-get-attribute node 'path))

(defun fsvn-xml-properties->target->properties (node)
  (fsvn-xml-get-children node 'property))

(defun fsvn-xml-properties->target->property.name (node)
  (fsvn-xml-get-attribute node 'name))

(defun fsvn-xml-properties->revprops.rev (node)
  (fsvn-xml-get-attribute node 'rev))

(defun fsvn-xml-properties->revprops->properties (node)
  (fsvn-xml-get-children node 'property))

(defun fsvn-xml-properties->revprops->property.name (node)
  (fsvn-xml-get-attribute node 'name))

(defun fsvn-xml-properties->revprops->property.encoding (node)
  (fsvn-xml-get-attribute node 'encoding))

;; svn proplist --xml end



;; svn log --xml --verbose start

(defun fsvn-xml-log->logentry.revision (node)
  (fsvn-xml-get-attribute node 'revision))

(defun fsvn-xml-log->logentry=>author$ (node)
  (fsvn-xml-get-atom-child node 'author))

(defun fsvn-xml-log->logentry=>date$ (node)
  (fsvn-xml-get-atom-child node 'date))

(defun fsvn-xml-log->logentry->paths (node)
  (fsvn-xml-node-children (fsvn-xml-get-child node 'paths)))

(defun fsvn-xml-log->logentry->paths->path$ (node)
  (fsvn-xml-get-text node 0))

(defun fsvn-xml-log->logentry->paths->path.action (node)
  (fsvn-xml-get-attribute node 'action))

(defun fsvn-xml-log->logentry->paths->path.copyfrom-rev (node)
  (fsvn-xml-get-attribute node 'copyfrom-rev))

(defun fsvn-xml-log->logentry->path.copyfrom-path (node)
  (fsvn-xml-get-attribute node 'copyfrom-path))

(defun fsvn-xml-log->logentry=>msg$ (node)
  (fsvn-xml-get-atom-child-safe node 'msg))

;; svn log --xml --verbose end


;; svn blame --xml start

(defun fsvn-xml-blame->target->entries (node)
  (fsvn-xml-get-children node 'entry))

(defun fsvn-xml-blame->target->entry.line-number (node)
  (fsvn-xml-get-attribute node 'line-number))

(defun fsvn-xml-blame->target->entry=>commit (node)
  (fsvn-xml-get-child node 'commit))

(defun fsvn-xml-blame->target->entry=>commit.revision (node)
  (fsvn-xml-get-attribute
   (fsvn-xml-blame->target->entry=>commit node)
   'revision))

(defun fsvn-xml-blame->target->entry=>commit=>author$ (node)
  (fsvn-xml-get-atom-child
   (fsvn-xml-blame->target->entry=>commit node)
   'author))

(defun fsvn-xml-blame->target->entry=>commit=>date$ (node)
  (fsvn-xml-get-atom-child
   (fsvn-xml-blame->target->entry=>commit node)
   'date))

;; svn blame --xml end

(defun fsvn-xml-processor (node dtd-alist)
  "dtd-alist is a dtd dtd := (name . dtd)
if dtd is t process node's children as text node.
if dtd is symbol call symbol as function that accept a argument.
if dtd is list call this function recursively.
"
  (let ((name-node (car node))
	(attrs (fsvn-xml-node-attributes node))
	(attrd (fsvn-xml-node-attributes dtd-alist))
	(children (fsvn-xml-node-children node))
	(childrend (fsvn-xml-node-children dtd-alist))
	ret child converter dtd name)
    (cond
     ((atom name-node)
      (setq name name-node))
     (t
      (setq name (car name-node))))
    (mapc
     (lambda (attr)
       (when (setq converter (cdr (assq (car attr) attrd)))
	 (setcdr attr (funcall converter (cdr attr)))))
     attrs)
    (while children
      (setq child (car children))
      (setq children (cdr children))
      (unless (stringp child)
	(setq dtd (assq (car child) childrend))
	(setq converter (fsvn-xml-node-children dtd))
	(setq ret
	      (cons
	       (cond
		((eq converter t)
		 child)
		((listp converter)
		 (fsvn-xml-processor child dtd))
		((symbolp converter)
		 (list
		  (car child)
		  (fsvn-xml-node-attributes child)
		  (funcall converter (fsvn-xml-get-node child))))
		(t
		 (error "Unrecognized node")))
	       ret))))
    (cons name
	  (cons
	   attrs
	   (nreverse ret)))))

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
	     (setq max seq))
	   )))
     multi-nodes)
    max))

(defun fsvn-xml-text-matched (node string)
  (catch 'found
    (mapc
     (lambda (attr)
       (when (and (stringp (cdr attr))
		  (string-match string (cdr attr)))
	 (throw 'found node)))
     (fsvn-xml-node-attributes node))
    (mapc
     (lambda (x)
       (cond
	((stringp x)
	 (when (string-match string x)
	   (throw 'found x)))
	((and (listp x)
	      (symbolp (car x)))
	 (let ((matched (fsvn-xml-text-matched x string)))
	   (when matched
	     (throw 'found matched))))))
     (fsvn-xml-node-children node))
    nil))

(defun fsvn-xml-parse-logentry ()
  (let ((xml (xml-parse-region (point-min) (point-max))))
    (fsvn-xml-get-children
     (fsvn-xml-processor (car xml) fsvn-xml-log-dtd-alist)
     'logentry)))

(defun fsvn-xml-parse-proplist ()
  (let ((xml (xml-parse-region (point-min) (point-max))))
    (fsvn-xml-get-children
     (fsvn-xml-processor (car xml) fsvn-xml-proplist-dtd-alist)
     'target)))

(defun fsvn-xml-parse-revprops ()
  (let ((xml (xml-parse-region (point-min) (point-max))))
    (fsvn-xml-get-children
     (fsvn-xml-processor (car xml) fsvn-xml-proplist-dtd-alist)
     'revprops)))

(defun fsvn-xml-parse-info ()
  (let ((xml (xml-parse-region (point-min) (point-max))))
    (fsvn-xml-get-children
     (fsvn-xml-processor (car xml) fsvn-xml-info-dtd-alist)
     'entry)))

(defun fsvn-xml-parse-blame ()
  (let ((xml (xml-parse-region (point-min) (point-max))))
    (fsvn-xml-get-children
     (fsvn-xml-processor (car xml) fsvn-xml-blame-dtd-alist)
     'target)))

(defun fsvn-xml-parse-lists-entries ()
  (let ((xml (fsvn-xml-parse-lists)))
    (fsvn-xml-node-children (car xml))))

(defun fsvn-xml-parse-lists ()
  (let ((xml (xml-parse-region (point-min) (point-max)))
	ret)
    ;; ignore rootnode, attribute, newline
    (fsvn-xml-get-children
     (fsvn-xml-processor (car xml) fsvn-xml-ls-dtd-alist)
     'list)))

(defun fsvn-xml-parse-status ()
  (let ((xml (xml-parse-region (point-min) (point-max))))
    (fsvn-xml-node-children
     (fsvn-xml-processor (car xml) fsvn-xml-status-dtd-alist))))



(provide 'fsvn-xml)

;;; fsvn-xml.el ends here
