;;; fsvn-ui.el --- Global User Interface definitions for fsvn.el


;;; History:
;; 

;;; Commentary:
;; 

(require 'dired)
(require 'fsvn-deps)



(defcustom fsvn-generic-datetime-format "%Y-%m-%d %H:%M"
  "*Date and time format in any."
  :group 'fsvn
  :type 'string)

(defcustom fsvn-help-locale nil
  "*Locale of your favorite."
  :group 'fsvn
  :type 'string)

(defconst fsvn-mark-mark-char dired-marker-char
  "In `fsvn-browse-mode', the current mark character.
This is what the do-commands look for, and what the mark-commands store.")

(defconst fsvn-mark-delete-char dired-del-marker
  "Character used to flag files for deletion.")

(defgroup fsvn-faces nil
  "*"
  :group 'fsvn)

(defface fsvn-header-face
  '((t (:inherit dired-header)))
  "Face used for directory headers."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-header-face 'fsvn-header-face
  "Face name used for directory headers.")

(defface fsvn-header-key-face
  '((t (:inherit dired-header :bold t)))
  "Face used for directory headers."
  :group 'fsvn-faces
  :version "22.1")

(defconst fsvn-header-key-face 'fsvn-header-key-face
  "Face name used for directory headers.")

(defface fsvn-mark-face
  '((t (:inherit dired-mark)))
  "Face used for fsvn marks."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-mark-face 'fsvn-mark-face
  "Face name used for fsvn marks.")

(defface fsvn-marked-face
  '((t (:inherit dired-marked)))
  "Face used for marked files."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-marked-face 'fsvn-marked-face
  "Face name used for marked files.")

(defface fsvn-flagged-face
  '((t (:inherit dired-flagged)))
  "Face used for flagged files."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-flagged-face 'fsvn-flagged-face
  "Face name used for flagged files.")

(defface fsvn-warning-face
  '((t (:inherit dired-warning)))
  "Face used to highlight a part of a buffer that needs user attention."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-warning-face 'fsvn-warning-face
  "Face name used for a part of a buffer that needs user attention.")

(defface fsvn-directory-face
  '((t (:inherit dired-directory)))
  "Face used for subdirectories."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-directory-face 'fsvn-directory-face
  "Face name used for subdirectories.")

(defface fsvn-symlink-face
  '((t (:inherit dired-symlink)))
  "Face used for subdirectories."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-symlink-face 'fsvn-symlink-face
  "Face name used for subdirectories.")

(defface fsvn-ignored-face
  '((t (:inherit shadow)))
  "Face used for files suffixed with `completion-ignored-extensions'."
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-ignored-face 'fsvn-ignored-face
  "Face name used for files suffixed with `completion-ignored-extensions'.")

(defface fsvn-keyname-face
  '((t (:inherit dired-directory)))
  "Face used for revision"
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-keyname-face 'fsvn-keyname-face
  "Face name used for revision.")

(defface fsvn-link-face
  '((t (:foreground "blue" :underline "blue")))
  "Face used for any link"
  :group 'fsvn-faces
  :version "22.1")
(defconst fsvn-link-face 'fsvn-link-face
  "Face used for any link")



;; playing
;;  http://ja.wiktionary.org/wiki/Category:%E8%8B%B1%E8%AA%9E_%E4%B8%8D%E8%A6%8F%E5%89%87%E8%A4%87%E6%95%B0%E5%BD%A2%E3%81%AE%E5%90%8D%E8%A9%9E

(defconst fsvn-word-irregular-plural-alist
  '(
    ("child" . "children" )
    ;;    ("crux" . "cruxes" )
    ("foot" . "feet" )
    ("knife" . "knives" )
    ("leaf" . "leaves" )
    ("louse" . "lice" )
    ("man" . "men" )
    ("medium" . "media" )
    ("mouse" . "mice" )
    ("oasis" . "oases" )
    ("person" . "people" )
    ("phenomenon" . "phenomena" )
    ("seaman" . "seamen" )
    ("snowman" . "snowmen" )
    ("tooth" . "teeth" )
    ("woman" . "women" )
    ))

(defun fsvn-word-plural (word)
  (cond
   ((fsvn-string-assoc word fsvn-word-irregular-plural-alist)
    (cdr (fsvn-string-assoc word fsvn-word-irregular-plural-alist)))
   ((string-match "\\(sh\\|ch\\|o\\|s\\|x\\)$" word)
    (concat word "es"))
   ((string-match "\\(y\\)$" word)
    (replace-match "ies" nil nil word 1))
   (t
    (concat word "s"))))



;; face utility

;; FIXME want to well contrast value
(defun fsvn-get-background-color (foreground)
  (let ((count (length (defined-colors)))
	(rest (member foreground (defined-colors))))
    ;;FIXME
    (nth (% (+ (length rest) 100) count) (defined-colors))))



(defun fsvn-buffer-popup-as-information (buffer)
  (delete-other-windows)
  (let ((win (split-window)))
    (set-window-buffer win buffer)
    (fsvn-save-window-only win
      (goto-char (point-min)))))



(provide 'fsvn-ui)

;;; fsvn-ui.el ends here
