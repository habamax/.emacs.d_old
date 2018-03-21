;;;; hl-lisp-globals-mode.el --- A minor mode to highlight LISP global and constant variables.

;; Copyright (C) 2018-03-21 Maxim Kim

;; Author: Maxim Kim <habamax@gmail.com>
;; Version: 0.1
;; URL: https://github.com/habamax/hl-lisp-global-mode.el

;;;; Commentary:

;; TODO: When this mode is off highlighting in still there...

;;;; Code:

(defface lisp-global-var-face
  '((t (:inherit default :weight bold)))
  "Face for LISP global variable.")

(defvar lisp-global-var-face 'lisp-global-var-face
  "Face name to use for LISP global variables.")

(defface lisp-constant-var-face
  '((t (:inherit defalut :weight bold)))
  "Face for LISP constant variable.")

(defvar lisp-constant-var-face 'lisp-constant-var-face
  "Face name to use for LISP constant variables.")

(define-minor-mode hl-lisp-globals-mode
  "Minor mode for highlighting LISP *GLOBAL-VARIABLES* and +CONSTANT-VARIABLES+."
  nil "GL-HL" nil

  (if hl-lisp-globals-mode
      (hl-lisp-globals/add-keywords)
    (hl-lisp-globals/remove-keywords))
  


  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))


(defvar hl-lisp-globals-font-lock--installed-keywords nil)

(defvar hl-lisp-globals-keywords
  '(("\\B\\*\\(?:[[:word:]]+?[-_/]*\\)+?\\*\\B" . lisp-global-var-face)
    ("\\B\\+\\(?:[[:word:]]+?[-_/]*\\)+?\\+\\B" . lisp-constant-var-face)))


(defun hl-lisp-globals/add-keywords ()
  "Add extra font-lock keywords to lisp."
  (set (make-local-variable 'font-lock-multiline) t)
  (when (local-variable-p 'hl-lisp-globals-font-lock--installed-keywords)
    (font-lock-remove-keywords nil hl-lisp-globals-font-lock--installed-keywords))
  (set (make-local-variable 'hl-lisp-globals-font-lock--installed-keywords)
       hl-lisp-globals-keywords)
  (font-lock-add-keywords nil
                          hl-lisp-globals-keywords))


(defun hl-lisp-globals/remove-keywords ()
  "Remove font-lock keywords for extra lisp highlithing."
  (font-lock-remove-keywords nil hl-lisp-globals-font-lock--installed-keywords))

(provide 'hl-lisp-globals-mode)
;;; asciidoctor-mode.el ends here

