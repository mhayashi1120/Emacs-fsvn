;;; -*- Coding: utf-8 -*-
;;; mw32cmp-test.el --- Test mw32cmp.el tools

(defvar mw32cmp-test-checked-type nil)

(defun mw32cmp-test-equality ()
  (setq mw32cmp-test-checked-type nil)
  (ad-add-advice 'mw32cmp-registry-type-converter 
		 (list 'mw32cmp-registry-type-converter-check nil t 
		       (list 'advice 'lambda nil 
			     `(unless (memq ad-return-value mw32cmp-test-checked-type)
				(setq mw32cmp-test-checked-type
				      (cons ad-return-value mw32cmp-test-checked-type)))))
		 'after nil)
  (ad-activate 'mw32cmp-registry-type-converter)
  (mw32cmp-test-registry-equality "HKEY_CLASSES_ROOT")
  (mw32cmp-test-registry-equality "HKEY_CURRENT_USER")
  (mw32cmp-test-registry-equality "HKEY_LOCAL_MACHINE")
  (mw32cmp-test-registry-equality "HKEY_USERS")
  (mw32cmp-test-registry-equality "HKEY_CURRENT_CONFIG")
  ;;todo non existent key and name
  )

(defun mw32cmp-test-registry-equality (target-key)
  (condition-case err
      (let (pseudo original)
	;; compare default value
	(setq original (mw32-registry-get target-key))
	(setq pseudo (mw32cmp-pseudo-registry-get target-key))
	(unless (equal pseudo original)
	  (error "different value %s: %s -> %s" target-key original pseudo))
	(setq original (mw32-registry-list-values target-key t))
	(setq pseudo (mw32cmp-pseudo-registry-list-values target-key t))
	(unless (equal pseudo original)
	  (error "different values data %s: %s -> %s" target-key original pseudo))
	(setq original (mw32-registry-list-values target-key))
	(setq pseudo (mw32cmp-pseudo-registry-list-values target-key))
	(unless (equal pseudo original)
	  (error "different values %s: %s -> %s" target-key original pseudo))
	(setq original (mw32-registry-list-keys target-key t))
	(setq pseudo (mw32cmp-pseudo-registry-list-keys target-key t))
	(unless (equal pseudo original)
	  (error "different key data %s: %s -> %s" target-key original pseudo))
	(setq original (mw32-registry-list-keys target-key))
	(setq pseudo (mw32cmp-pseudo-registry-list-keys target-key))
	(unless (equal pseudo original)
	  (error "different key %s: %s -> %s" target-key original pseudo))
	(mapc
	 (lambda (key-name)
	   (mw32cmp-test-registry-equality (mw32cmp-pseudo-registry-concat-key target-key key-name)))
	 pseudo)
	t)
    (error 
     (let (buffer-read-only)
       (insert (format "%s: %s\n" target-key err))
       (signal 'error err)))))


(defun mw32cmp-pseudo-registry-concat-key (parent name)
  (if (string-match "\\\\$" parent)
      (concat parent name)
    (concat parent "\\\\" name)))

;;; mw32cmp-test.el ends here
