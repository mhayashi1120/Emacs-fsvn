;;; fsvn.el --- Fast and Functional Subversion interface for emacs
;; Copyright (C) 2008-2010 by Masahiro Hayashi

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; $Id: fsvn.el 1317 2008-11-01 14:36:27Z masa $

;; fsvn.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

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
;;  * GNU Emacs 22.x or later.
;;  * Subversion 1.4.x or later.

;; fsvn has TortoiseSVN like user interface by using `svn' command.
;;  Have following advantages of other Emacs svn client.
;;  * tsvn:*, bugtraq:* like property supported. (or will be supported)
;;  * Using `svn help' output for completing read.
;;  * Fast in huge working copy by asynchronous process.
;;  * Has repository browser.
;;  * Has visualize blame/annotate/praise minor-mode.
;;  * Has svk support

;;  But following **disadvantage** has.
;;  * Key bindings is not friendly for legacy user.
;;  * Dired like interface but not equals exactly dired functions.
;;  * A little user help.

;; This package tested following environment.
;;      Meadow (based Emacs 22.1) on Windows.  svn 1.6.x
;;      Emacs current (24.0.50) on GNU/Linux (Debian).  svn 1.4.2

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

;; see repositories below
;; * http://svn.apache.org/repos/asf/subversion/trunk/
;; * http://tortoisesvn.tigris.org/svn/tortoisesvn/trunk
;; * http://svn.meadowy.org/Meadow/trunk/

;; NOTE:
;; Q.  How to execute `propset' recursively?
;; A.  M-x fsvn-propedit-toggle-recursive (or type C-c C-r)

;; Q.  How to authorize repository.  Password prompt stop everything.
;; A.  M-x fsvn-authenticate-repository
;;    This makes svn to cache password.

;; Q.  How to use svk in windows.
;; A.  Sample of settings.
;;    (setq fsvn-svk-perl-command "c:/usr/local/svk/bin/perl")
;;    (setq fsvn-svk-script "c:/usr/local/svk/bin/svk")
;;    
;;   Windows native perl.exe displays propmt but not works in Emacs sub process.
;;

;; Q.  How to create repository
;; A.  M-x fsvn-admin-create-repository
;;    to create repository if default-directory have no files.

;; Q.  How to use in NTEmacs
;; A.  TODO
;;    fiber.exe patch

;;;;;;;;;; DO following sometimes ;;;;;;;;;;;;;;;;;;;
;; elint with Emacs command line option -q
;; fsvn-test Emacs command line option -q
;; checkdoc (?)
;; checkdoc-message-text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Coding

;; * name definition
;;   url -+- filename of local directory (ex c:/hoge /hoge )
;;        +- real url (ex.  svn://* http:// like string)
;;
;;   path -+- filename of local directory (ex c:/hoge /hoge )
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
;; 	("svn://localhost"
;; 	 )
;; 	))

;; 3. global key bindings (option)
;; (global-set-key "\C-xv!" 'fsvn-command)
;; (global-set-key "\C-xv," 'fsvn-backward-popup-result-buffer)
;; (global-set-key "\C-xv." 'fsvn-forward-popup-result-buffer)
;; (global-set-key "\C-xvV" 'fsvn-vc-commit)
;; (global-set-key "\C-xvG" 'fsvn-blame-minor-mode)
;; (global-set-key "\C-xvI" 'fsvn-import)
;; (global-set-key "\C-xvL" 'fsvn-vc-print-log)
;; (global-set-key "\C-xvO" 'fsvn-checkout)
;; (global-set-key "\C-xvZ" 'fsvn-debug-toggle)
;; (global-set-key "\C-xv\ec" 'fsvn-global-cleanup-buffer)

;; 4. TODO this is old
;;    Emacs 22 unicode conversion is wrong.
;;    Put following lines into your dot-emacs
;;
;;    thanks.
;;   http://www.pqrs.org/tekezo/emacs/doc/wide-character/index.html
;;
;;   (utf-translate-cjk-set-unicode-range
;;    '((#x00a2 . #x00a3)			; Cent Sign, Pound Sign
;;      (#x00a7 . #x00a8)			; Section Sign, Diaeresis
;;      (#x00ac . #x00ac)			; Not Sign
;;      (#x00b0 . #x00b1)			; Degree Sign, Plus-Minus Sign
;;      (#x00b4 . #x00b4)			; Acute Accent
;;      (#x00b6 . #x00b6)			; Pilcrow Sign
;;      (#x00d7 . #x00d7)			; Multiplication Sign
;;      (#X00f7 . #x00f7)			; Division Sign
;;      (#x0370 . #x03ff)			; Greek And Coptic
;;      (#x0400 . #x04FF)			; Cyrillic
;;      (#x2000 . #x206F)			; General Punctuation
;;      (#x2100 . #x214F)			; Letterlike Symbols
;;      (#x2190 . #x21FF)			; Arrows
;;      (#x2200 . #x22FF)			; Mathematical Operators
;;      (#x2300 . #x23FF)			; Miscellaneous Technical
;;      (#x2500 . #x257F)			; Box Drawing
;;      (#x25A0 . #x25FF)			; Geometric Shapes
;;      (#x2600 . #x26FF)			; Miscellaneous Symbols
;;      (#x2e80 . #xd7a3) (#xff00 . #xffef))))
;;    
;;    TODO: Circle number, Fullwidth Tilde

;;  5. Module dependency
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
;;



(defgroup fsvn nil
  "Emacs subversion interface."
  :group 'tools
  :prefix "fsvn-")

(defvar fsvn-version "0.9.2"
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

(require 'fsvn-minibuf)
(require 'fsvn-tortoise)
(require 'fsvn-blame)
(require 'fsvn-admin)
(require 'fsvn-magic)
(require 'fsvn-svk)



(defvar system-type)
(when (memq system-type '(windows-nt))
  (require 'fsvn-win))



(fsvn-initialize-loading)

(provide 'fsvn)

;;; fsvn.el ends here
