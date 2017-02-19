;;; asciidoctor-mode.el --- A major mode for AsciiDoc files.

;; Copyright (C) 2017 Maxim Kim

;; Author: Maxim Kim <habamax@gmail.com>
;; Version: 0.1
;; URL: https://github.com/habamax/asciidoctor-mode.el

;;; Commentary:

;; None
;; Yet

;;; Code:

;; (defvar asciidoctor-mode-hook nil
  ;; "Hook run when entering asciidoctor mode.")

;;; Customize

(defgroup asciidoctor nil
  "Major mode for editing text files in AsciiDoc format."
  :prefix "asciidoctor-")


(defcustom asciidoctor-pdf-stylesdir
  ""
  "Directory with asciidoctor-pdf themes."
  :group 'asciidoctor-pdf
  :type 'string)

(defcustom asciidoctor-pdf-fontsdir
  ""
  "Directory with asciidoctor-pdf fonts."
  :group 'asciidoctor-pdf
  :type 'string)

(defcustom asciidoctor-pdf-extensions
  ""
  "Extensions such as asciidoctor-diagram."
  :group 'asciidoctor-pdf
  :type 'string)



;;; Regular expressions
(defconst asciidoctor-regex-comment
  "//"
  "Regular expression matches AsciiDoc comment.")

(defconst asciidoctor-regex-header-atx
  "^\\(=+\\)[ \t]+\\(.*?\\)[ \t]*\\(=*\\)$"
  "Regular expression for generic atx-style (equal signs) headers.")

(defconst asciidoctor-regex-bold
  "\\(^\\|[^\\]\\)\\(\\([*_]\\{2\\}\\)\\([^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(\\3\\)\\)"
  "Regular expression for matching bold text.")

;;; Font Lock

(require 'font-lock)

;; (defvar asciidoctor-italic-face 'asciidoctor-italic-face
;;   "Face name to use for italic text.")

;; (defvar asciidoctor-bold-face 'asciidoctor-bold-face
;;   "Face name to use for bold text.")

;; (defvar asciidoctor-header-face 'asciidoctor-header-face
;;   "Face name to use as a base for headers.")

;; (defvar asciidoctor-header-face-1 'asciidoctor-header-face-1
;;   "Face name to use for level-1 headers.")

;; (defvar asciidoctor-header-face-2 'asciidoctor-header-face-2
;;   "Face name to use for level-2 headers.")

;; (defvar asciidoctor-header-face-3 'asciidoctor-header-face-3
;;   "Face name to use for level-3 headers.")

;; (defvar asciidoctor-header-face-4 'asciidoctor-header-face-4
;;   "Face name to use for level-4 headers.")

;; (defvar asciidoctor-header-face-5 'asciidoctor-header-face-5
;;   "Face name to use for level-5 headers.")

;; (defvar asciidoctor-header-face-6 'asciidoctor-header-face-6
;;   "Face name to use for level-6 headers.")

;; (defgroup asciidoctor-faces nil
;;   "Faces used in AsciiDoc Mode"
;;   :group 'asciidoctor
;;   :group 'faces)






(defun asciidoctor-compile-html ()
  "Compile AsciiDoc file to HTML, using asciidoctor ruby implementation."
  (interactive)
  (when buffer-file-name
    (message "AsciiDoctor HTML compilation...")
    (shell-command
     (concat "asciidoctor"
             " "
             ;; what about multiple extensions?
             (when (not (string= "" asciidoctor-pdf-extensions))
               (concat "-r " asciidoctor-pdf-extensions))
             " "
             "-a docdate=" (format-time-string "%Y-%m-%d")
             " "
             "-a doctime=" (format-time-string "%T")
             " "
             buffer-file-name))))


(defun asciidoctor-compile-pdf ()
  "Compile AsciiDoc file to PDF, using asciidoctor-pdf ruby implementation."
  (interactive)

  (let ((pdf-stylesdir (expand-file-name asciidoctor-pdf-stylesdir))
        (pdf-fontsdir (expand-file-name asciidoctor-pdf-fontsdir)))
    (when buffer-file-name
      (message "AsciiDoctor PDF compilation...")
      (shell-command
       (concat "asciidoctor-pdf"
               " "
               ;; what about multiple extensions?
               (when (not (string= "" asciidoctor-pdf-extensions))
                 (concat "-r " asciidoctor-pdf-extensions))
               " "
               (when (not (string= "" pdf-stylesdir))
                 (concat "-a pdf-stylesdir=" pdf-stylesdir)
                 )
               " "
               (when (not (string= "" pdf-fontsdir))
                 (concat "-a pdf-fontsdir=" pdf-fontsdir)
                 )
               " "
               buffer-file-name)))))

(defun asciidoctor-open-pdf ()
  "Open compiled PDF file."
  (interactive)
  (browse-url (concat
               (file-name-sans-extension buffer-file-name)
               ".pdf")))

(defun asciidoctor-open-html ()
  "Open compiled HTML file."
  (interactive)
  (browse-url (concat
               (file-name-sans-extension buffer-file-name)
               ".html")))



(defvar asciidoctor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c\C-h" 'asciidoctor-compile-html)
    (define-key map "\C-c\C-c\C-p" 'asciidoctor-compile-pdf)
    (define-key map "\C-c\C-o\C-o" 'browse-url-of-buffer)
    (define-key map "\C-c\C-o\C-p" 'asciidoctor-open-pdf)
    (define-key map "\C-c\C-o\C-h" 'asciidoctor-open-html)
    map)
  "Keymap used in AsciiDoc mode.")


(defconst asciidoctor-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; comments
    (modify-syntax-entry ?/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    
    table)
  "Syntax table for `asciidoctor-mode'")

(defvar asciidoctor-font-lock-keywords nil
  "Keyword highlighting specification for `sample-mode'.")

(setq asciidoctor-font-lock-keywords 
  '(
    ("^\\(=+\\)[ \t]+\\(.*?\\)[ \t]*\\(=*\\)$" . font-lock-comment-face)
    ("\\(NOTE\\|TIP\\|CAUTION\\|WARNING\\|IMPORTANT\\):\\(\\s+\\S+\\)" 0 font-lock-keyword-face)
    ))


;; (defconst markdown-regex-header
;;   "^\\(?:\\([^\r\n\t -].*\\)\n\\(?:\\(=+\\)\\|\\(-+\\)\\)\\|\\(#+\\)[ \t]+\\(.*?\\)[ \t]*\\(#*\\)\\)$"
;;   "Regexp identifying Markdown headings.
;; Group 1 matches the text of a setext heading.
;; Group 2 matches the underline of a level-1 setext heading.
;; Group 3 matches the underline of a level-2 setext heading.
;; Group 4 matches the opening hash marks of an atx heading.
;; Group 5 matches the text, without surrounding whitespace, of an atx heading.
;; Group 6 matches the closing hash marks of an atx heading.")


;;;###autoload
(define-derived-mode asciidoctor-mode text-mode "AsciiDoctor"
  "Major mode for editing AsciiDoc(or) files."

  :syntax-table asciidoctor-mode-syntax-table
  
  (setq-local tab-width 4)

  (setq font-lock-defaults
        '(asciidoctor-font-lock-keywords))
  
  ;; Comments
  (setq-local comment-start "// ")
  ;; (setq-local comment-end "")
  (setq-local comment-use-syntax t)
  (setq-local comment-start-skip "//+\\s *")
  
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . asciidoctor-mode) t)

(provide 'asciidoctor-mode)
;;; asciidoctor-mode.el ends here


