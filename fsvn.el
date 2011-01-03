;;; fsvn.el --- Functional Subversion interface for emacs
;; Copyright (C) 2008-2010 by Masahiro Hayashi

;; Author: Hayashi Masahiro <mhayashi1120@gmail.com>
;; URL: http://fsvn.sourceforge.jp/
;; Keywords: Emacs, Subversion, Frontend
;; Version: 0.9.8

;; fsvn.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; fsvn.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; fsvn supports
;;  * GNU Emacs 23.x or later.
;;  * Subversion 1.5.x or later. (1.4.x works but some restriction)

;; fsvn has TortoiseSVN like user interface by using `svn' command.
;;  Have following advantages of other Emacs svn client.
;;  * tsvn:*, bugtraq:* like property supported. (or will be supported)
;;  * Using `svn help' output for completing read.
;;  * Fast in huge working copy by background process.
;;  * Has repository browser.
;;  * Has visualize blame/annotate/praise minor-mode.
;;  * Has svk support

;;  But following **disadvantage** has.
;;  * Key bindings is not friendly for legacy user.
;;  * Dired like interface but not exactly equals dired functions.
;;  * A little user help.

;; This package is tested on following environment.
;;      NTEmacs (based Emacs 23.1) on Windows.  svn 1.5.x - 1.7.x
;;      Emacs (23.2) on GNU/Linux (Debian).  svn svn 1.5.x - 1.7.x
;;      Emacs current (24.0.50) on GNU/Linux (Debian).  svn 1.5.x - 1.7.x

;; major-mode and brief description
;; * fsvn-browse-mode (dired like interface)
;; * fsvn-select-file-mode (Select multiple files)
;; * fsvn-message-edit-mode (Edit log message and commit it)
;; * fsvn-popup-result-mode (Show svn command output)
;; * fsvn-proplist-mode (Property list view)
;; * fsvn-propedit-mode (`fsvn-proplist-mode' subwindow property selected)
;; * fsvn-log-list-mode (Log list)
;; * fsvn-log-sibling-mode (`fsvn-log-list-mode' subwindow revision changed file list)
;; * fsvn-log-message-mode (`fsvn-log-list-mode' subwindow revision log message)
;; * fsvn-process-list-mode (Process list view)

;; see following repositories
;; * http://svn.apache.org/repos/asf/subversion/trunk/
;; * http://tortoisesvn.tigris.org/svn/tortoisesvn/trunk
;; * http://svn.meadowy.org/Meadow/trunk/

;; NOTE:

;; Q. TODO
;; A. TODO
;; (setenv "SVN_SSH" "ssh -q")

;; Q.  How to use in NTEmacs
;; A.  TODO
;;    fiber.exe patch

;;; Coding

;; * name definition
;;   url -+- filename of local directory (e.g. c:/hoge /hoge )
;;        +- real url (e.g.  svn://* http:// like string)
;;
;;   path -+- filename of local directory (e.g. c:/hoge /hoge )
;;         +- path to repository object.  only directory.  contains first `/'
;;
;;   * path is unique in a fsvn-browse-mode buffer.
;;
;;

;; * fsvn-xml-* name definition todo not completed
;;      => : access to children's first node (unique one)
;;      -> : access to children
;;      $  : text node.
;;      .  : attribute node.

;; * Explicit argument MUST be long option.

;; * do not use fsvn-test namespace.

;;; Configuration in dot-emacs

;; 1. Add svn command path to `exec-path' correctly.
;;    Otherwise set `fsvn-svn-command' `fsvn-svnadmin-command' value by full-path before loading/requiring.
;; 
;; (setq fsvn-svn-command "/path/to/svn.exe"
;;       fsvn-svnadmin-command "/path/to/svnadmin.exe")

;; 2. Sample settings.
;;  (require 'fsvn)
;;  (setq fsvn-help-locale "ja")
;;  (setq fsvn-repository-alist
;;       '(
;;      ("svn://localhost"
;;       )
;;      ))

;; 3. global key bindings (option)
;; (global-set-key "\C-xv!" 'fsvn-command)
;; (global-set-key "\C-xv," 'fsvn-backward-popup-result-buffer)
;; (global-set-key "\C-xv." 'fsvn-forward-popup-result-buffer)
;; (global-set-key "\C-xvV" 'fsvn-vc-commit)
;; (global-set-key "\C-xvG" 'fsvn-blame-minor-mode)
;; (global-set-key "\C-xvI" 'fsvn-import)
;; (global-set-key "\C-xvL" 'fsvn-vc-print-log)
;; (global-set-key "\C-xvO" 'fsvn-checkout)
;; (global-set-key "\C-xvP" 'fsvn-process-list)
;; (global-set-key "\C-xvZ" 'fsvn-debug-toggle)
;; (global-set-key "\C-xv\ec" 'fsvn-global-cleanup-buffer)
;; (global-set-key "\C-xvN" 'fsvn-vc-commit-non-query)

;;  4. Module dependency
;;     Independent modules
;;      * fsvn-env
;;      * fsvn-debug

;;      browse -> proplist/propedit
;;      browse -> log
;;      browse -> file-select

;;      blame-minor

;;; History:
;;

;;; Code:



(defgroup fsvn nil
  "Emacs subversion interface."
  :group 'tools
  :prefix "fsvn-")

(defvar fsvn-version "0.9.8"
  "Version of fsvn.")



(require 'dired)
(require 'menu-bar)

(require 'fsvn-deps)
(require 'fsvn-proc)
(require 'fsvn-xml)
(require 'fsvn-data)
(require 'fsvn-env)
(require 'fsvn-debug)
(require 'fsvn-fs)
(require 'fsvn-config)
(require 'fsvn-pub)

(require 'fsvn-browse)
(require 'fsvn-dired)

(require 'fsvn-proclist)

(require 'fsvn-minibuf)
(require 'fsvn-tortoise)
(require 'fsvn-blame)
(require 'fsvn-admin)
(require 'fsvn-magic)



(defvar system-type)
(when (memq system-type '(windows-nt))
  (require 'fsvn-win))



(fsvn-initialize-loading)

(provide 'fsvn)

;;; fsvn.el ends here
