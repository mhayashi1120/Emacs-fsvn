;; Path to svn command exec-path
(setq exec-path (cons "/path/to/subversion/bin" exec-path))

;; Set full path if PATH problem (ex: cygwin, win32)
;; (setq fsvn-svn-command "c:/path/to/subversion/bin/svn.exe"
;;       fsvn-svnadmin-command "c:/path/to/subversion/bin/svnadmin.exe")

(require 'fsvn)

;; Set help locale
(setq fsvn-help-locale "en")

;; blame/prase/annotate minor-mode
(global-set-key "\C-xvG" 'fsvn-blame-minor-mode)
;; Import current directory to selected repository.
(global-set-key "\C-xvI" 'fsvn-import)
;; Checkout from selected repository.
(global-set-key "\C-xvO" 'fsvn-checkout)
;; Remove result buffers
(global-set-key "\C-xv\ec" 'fsvn-global-cleanup-buffer)
