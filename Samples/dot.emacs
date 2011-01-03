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

;; Execute svn command.
(global-set-key "\C-xv!" 'fsvn-command)

;; Cycle result buffers.
(global-set-key "\C-xv," 'fsvn-backward-popup-result-buffer)
(global-set-key "\C-xv." 'fsvn-forward-popup-result-buffer)

;; Commit current buffer.
(global-set-key "\C-xvV" 'fsvn-vc-commit)

;; Commit current buffer with no query.
(global-set-key "\C-xvN" 'fsvn-vc-commit-non-query)

;; Show log current buffer file.
(global-set-key "\C-xvL" 'fsvn-vc-print-log)

;; Show process list.
(global-set-key "\C-xvP" 'fsvn-process-list)

;; Toggle debugging.
(global-set-key "\C-xvZ" 'fsvn-debug-toggle)

;; Inhibit displaying prompt when `svn update' (C-c C-u).
(setq fsvn-no-confirm
      '(fsvn-browse-update-path))
