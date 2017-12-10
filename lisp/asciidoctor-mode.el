;;; asciidoctor-mode.el --- A major mode for AsciiDoc(tor) files that is not fancy.

;; Copyright (C) 2017-05-01 Maxim Kim

;; Author: Maxim Kim <habamax@gmail.com>
;; Version: 0.1
;; URL: https://github.com/habamax/asciidoctor-mode.el

;;; Commentary:

;; DONE:
;; * Headers
;; * Comments
;; * Options
;; * Definition list
;; * Bold and italic (partly)

;; TODO:
;; * Single line notes
;; * Source block (and other as-is blocks)
;; * Rest of the blocks
;; * Block names and options
;; * List bullets
;; * Tables? or Table separators?
;; * Bold/Italic/Mono (not yet)


;;; Code:

;; (defvar asciidoctor-mode-hook nil
  ;; "Hook run when entering asciidoctor mode.")


(require 'outline)
(require 'thingatpt)
(require 'cl-lib)
;; (require 'url-parse)



;;; Customize

(defgroup asciidoctor nil
  "Major mode for editing text files in AsciiDoc format."
  :prefix "asciidoctor-")


(defcustom asciidoctor-pdf-executable
  "asciidoctor-pdf"
  "asciidoctor-pdf executable."
  :group 'asciidoctor-pdf
  :type 'string)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Regexes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst asciidoctor-regex-header
  "^\\(=+\\)[ \t]+\\(.*?\\)[ \t]*\\(=*\\)$"
  "Regexp identifying Asciidoctor headings.
Group 1 matches the opening equal marks of an atx heading.
Group 2 matches the text, without surrounding whitespace, of an atx heading.")

(defconst asciidoctor-regex-blank-line
  "^[[:blank:]]*$"
  "Regular expression that matches a blank line.")

(defconst asciidoctor-regex-list-item
  "^[[:blank:]]*\\([*-.]+\\|[0-9]\\.\\)[[:blank:]]+"
  "Regular expression that matches a list item element.")


(defconst asciidoctor-regex-comment-start
  "^\\(////[ \t]*\\)\\|\\(//[ \t]+.*\\)$"
  "Regular expression matches asciidoctor comment opening.")

(defconst asciidoctor-regex-comment-end
  "^////[ \t]*$"
  "Regular expression matches asciidoctor comment closing.")

(defconst asciidoctor-regex-option
  "^\\(:\\)\\(.*?\\)\\(:\\)\\([ \t]\\|$\\)"
  "Regular expression matches asciidoctor comment closing.")

(defconst asciidoctor-regex-def-list
  "^\\(.*?::\\)\\([[:blank:]]*$\\|[[:blank:]].*\\)"
  "Regular expression matches asciidoctor definition list.")

;; XXX: should be redone for multi row strings
(defconst asciidoctor-regex-double-bold
  ".\\(\\*\\*.*?\\*\\*\\)"
  "Regular expression matches bold text in double stars.")

;; XXX: should be redone for multi row strings
(defconst asciidoctor-regex-double-italic
  "__.*?__"
  "Regular expression matches bold text in double underscores.")


(defun asciidoctor-text-property-at-point (prop)
  (get-text-property (point) prop))


;; (defun asciidoctor-code-block-at-pos (pos)
;;   "Return match data list if there is a code block at POS.
;; This includes pre blocks, tilde-fenced code blocks, and GFM
;; quoted code blocks.  Return nil otherwise."
;;   (or (get-text-property pos 'asciidoctor-pre)
;;       (asciidoctor-get-enclosing-fenced-block-construct pos)))

;; (defun asciidoctor-code-block-at-point ()
;;   "Return match data if the point is inside a code block.
;; This includes pre blocks, tilde-fenced code blocks, and
;; GFM quoted code blocks.  Calls `asciidoctor-code-block-at-pos'."
;;   (asciidoctor-code-block-at-pos (point)))

(defun asciidoctor-outline-level ()
  "Return the depth to which a statement is nested in the outline."
  (cond
   ;; ((asciidoctor-code-block-at-point) 7)
   ((match-end 2) 1)
   ((match-end 3) 2)
   ((- (match-end 4) (match-beginning 4)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compilation and visiting
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
       (concat asciidoctor-pdf-executable
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
    ;; do not add any mode specific bindings
    ;; let the end user define it
    map)
  "Keymap used in AsciiDoctor mode.")


;; (defconst asciidoctor-mode-syntax-table
  ;; (let ((table (make-syntax-table)))
    ;; (modify-syntax-entry ?$ "." table)
    ;; (modify-syntax-entry ?% "." table)
    ;; (modify-syntax-entry ?& "." table)
    ;; (modify-syntax-entry ?' "." table)
    ;; (modify-syntax-entry ?` "." table)
    ;; (modify-syntax-entry ?\" "." table)
    ;; (modify-syntax-entry ?* "." table )
    ;; (modify-syntax-entry ?+ "." table )
    ;; (modify-syntax-entry ?. "." table )
    ;; (modify-syntax-entry ?/ "." table )
    ;; (modify-syntax-entry ?< "." table )
    ;; (modify-syntax-entry ?= "." table )
    ;; (modify-syntax-entry ?> "." table )
    ;; (modify-syntax-entry ?\\ "." table)
    ;; (modify-syntax-entry ?| "." table)
    ;; (modify-syntax-entry ?_ "." table)

    ;; table)
  ;; "Syntax table for `asciidoctor-mode'")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Faces
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup asciidoctor-faces nil
  "Faces used in Asciidoctor Mode"
  :group 'asciidoctor
  :group 'faces)


(defface asciidoctor-markup-face
  '((t (:inherit shadow :slant normal :weight normal)))
  "Face for markup elements."
  :group 'asciidoctor-faces)

(defface asciidoctor-header-delimiter-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Base face for headers hash delimiter."
  :group 'asciidoctor-faces)

(defface asciidoctor-header-face
  '((t (:inherit default
                 :weight bold)))
  "Base face for headers."
  :group 'asciidoctor-faces)

(defface asciidoctor-header-face-1
  '((t (:inherit asciidoctor-header-face :height 1.4)))
  "Header 1 face."
  :group 'asciidoctor-faces)

(defface asciidoctor-header-face-2
  '((t (:inherit asciidoctor-header-face :height 1.4)))
  "Header 2 face."
  :group 'asciidoctor-faces)

(defface asciidoctor-header-face-3
  '((t (:inherit asciidoctor-header-face :height 1.2)))
  "Header 3 face."
  :group 'asciidoctor-faces)

(defface asciidoctor-header-face-4
  '((t (:inherit asciidoctor-header-face :height 1.1)))
  "Header 4 face."
  :group 'asciidoctor-faces)

(defface asciidoctor-header-face-5
  '((t (:inherit asciidoctor-header-face :height 1.1)))
  "Header 5 face."
  :group 'asciidoctor-faces)

(defface asciidoctor-header-face-6
  '((t (:inherit asciidoctor-header-face :height 1.1)))
  "Header 6 face."
  :group 'asciidoctor-faces)

(defface asciidoctor-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for asciidoctor comments."
  :group 'asciidoctor-faces)

(defface asciidoctor-option-face
  '((t (:inherit shadow)))
  "Face for asciidoctor options."
  :group 'asciidoctor-faces)

(defface asciidoctor-option-markup-face
  '((t (:inherit shadow)))
  "Face for asciidoctor options."
  :group 'asciidoctor-faces)

(defface asciidoctor-def-list-face
  '((t (:inherit bold)))
  "Face for asciidoctor definition lists."
  :group 'asciidoctor-faces)

(defface asciidoctor-bold-face
  '((t (:inherit bold)))
  "Face for asciidoctor bold text."
  :group 'asciidoctor-faces)

(defface asciidoctor-italic-face
  '((t (:inherit italic)))
  "Face for asciidoctor italic text."
  :group 'asciidoctor-faces)

(defvar asciidoctor-markup-face 'asciidoctor-markup-face
  "Face name to use for markup elements.")

(defvar asciidoctor-header-delimiter-face 'asciidoctor-header-delimiter-face
  "Face name to use as a base for header delimiters.")

(defvar asciidoctor-header-face 'asciidoctor-header-face
  "Face name to use as a base for headers.")

(defvar asciidoctor-header-face-1 'asciidoctor-header-face-1
  "Face name to use for level-1 headers.")

(defvar asciidoctor-header-face-2 'asciidoctor-header-face-2
  "Face name to use for level-2 headers.")

(defvar asciidoctor-header-face-3 'asciidoctor-header-face-3
  "Face name to use for level-3 headers.")

(defvar asciidoctor-header-face-4 'asciidoctor-header-face-4
  "Face name to use for level-4 headers.")

(defvar asciidoctor-header-face-5 'asciidoctor-header-face-5
  "Face name to use for level-5 headers.")

(defvar asciidoctor-header-face-6 'asciidoctor-header-face-6
  "Face name to use for level-6 headers.")

(defvar asciidoctor-option-face 'asciidoctor-option-face
  "Face name to use for option elements.")

(defvar asciidoctor-option-markup-face 'asciidoctor-option-markup-face
  "Face name to use for option's markup elements.")

(defvar asciidoctor-def-list-face 'asciidoctor-def-list-face
  "Face name to use for definition lists.")

(defvar asciidoctor-bold-face 'asciidoctor-bold-face
  "Face name to use for bold text.")

(defvar asciidoctor-italic-face 'asciidoctor-italic-face
  "Face name to use for italic text.")

(defun asciidoctor-syntactic-face (state)
  "Return font-lock face for characters with given STATE.
See `font-lock-syntactic-face-function' for details."
  (let ((in-comment (nth 4 state)))
    (cond
     (in-comment 'asciidoctor-comment-face)
     (t nil))))


(defun asciidoctor-syntax-propertize-extend-region (start end)
  "Extend START to END region to include an entire block of text.
This helps improve syntax analysis for block constructs.
Returns a cons (NEW-START . NEW-END) or nil if no adjustment should be made.
Function is called repeatedly until it returns nil. For details, see
`syntax-propertize-extend-region-functions'."
  (save-match-data
    (save-excursion
      (let* ((new-start (progn (goto-char start)
                               (skip-chars-forward "\n")
                               (if (re-search-backward "\n\n" nil t)
                                   (min start (match-end 0))
                                 (point-min))))
             (new-end (progn (goto-char end)
                             (skip-chars-backward "\n")
                             (if (re-search-forward "\n\n" nil t)
                                 (max end (match-beginning 0))
                               (point-max))))
             ;; (code-match (asciidoctor-code-block-at-pos new-start))
             ;; (new-start (or (and code-match (cl-first code-match)) new-start))
             ;; (code-match (asciidoctor-code-block-at-pos end))
             ;; (new-end (or (and code-match (cl-second code-match)) new-end)))
             )
        (unless (and (eq new-start start) (eq new-end end))
          (cons new-start (min new-end (point-max))))))))



(defun asciidoctor-syntax-propertize-comments (start end)
  "Match asciidoctor comments from the START to END."
  (let* ((state (syntax-ppss)) (in-comment (nth 4 state)))
    (goto-char start)
    (cond

     ;; Block Comment start
     ((and (not in-comment)
           (re-search-forward asciidoctor-regex-comment-start end t)
           ;; TODO: implement code and code blocks at point
           ;; (save-match-data (not (asciidoctor-code-at-point-p)))
           ;; (save-match-data (not (asciidoctor-code-block-at-point)))
           )
      (let ((open-beg (match-beginning 0))
            (close-beg (match-end 2))) ; second group is the single line comment -- non nil if matched
        (put-text-property open-beg (1+ open-beg)
                           'syntax-table (string-to-syntax "<"))
        (when close-beg ; close single line comment
          (put-text-property (1- close-beg) close-beg
                             'syntax-table (string-to-syntax ">")))
        
        (asciidoctor-syntax-propertize-comments
         (min (1+ (match-end 0)) end (point-max)) end)))

     ;; Block Comment end
     ((and in-comment
           (re-search-forward asciidoctor-regex-comment-end end t))
      (put-text-property (1- (match-end 0)) (match-end 0)
                         'syntax-table (string-to-syntax ">"))
      (asciidoctor-syntax-propertize-comments
       (min (1+ (match-end 0)) end (point-max)) end))

      ;; Nothing found
     (t nil))))

(defun asciidoctor-syntax-propertize-headings (start end)
  "Match headings of type SYMBOL with REGEX from START to END."
  (goto-char start)
  (while (re-search-forward asciidoctor-regex-header end t)
    ;; TODO: implement code block
    ;; (unless (asciidoctor-code-block-at-pos (match-beginning 0))
      (put-text-property
       (match-beginning 0) (match-end 0) 'asciidoctor-heading t)
      (put-text-property
       (match-beginning 0) (match-end 0)
       (let ((atx-level (length (match-string-no-properties 1))))
         (intern (format "asciidoctor-heading-%d-atx" atx-level)))
       (match-data t))))

(defun asciidoctor-syntax-propertize-options (start end)
  "Match options of type SYMBOL with REGEX from START to END."
  (goto-char start)
  (while (re-search-forward asciidoctor-regex-option end t)
    ;; TODO: implement code block
    ;; (unless (asciidoctor-code-block-at-pos (match-beginning 0))
      (put-text-property
       (match-beginning 0) (match-end 0) 'asciidoctor-option (match-data t))
      ))

(defun asciidoctor-syntax-propertize-def-lists (start end)
  "Match options of type SYMBOL with REGEX from START to END."
  (goto-char start)
  (while (re-search-forward asciidoctor-regex-def-list end t)
    ;; TODO: implement code block
    ;; (unless (asciidoctor-code-block-at-pos (match-beginning 0))
      (put-text-property
       (match-beginning 0) (match-end 0) 'asciidoctor-def-list (match-data t))
      ))

(defun asciidoctor-syntax-propertize-double-bold (start end)
  "Match options of type SYMBOL with REGEX from START to END."
  (goto-char start)
  (while (re-search-forward asciidoctor-regex-double-bold end t)
    ;; TODO: implement code block
    ;; (unless (asciidoctor-code-block-at-pos (match-beginning 0))
      (put-text-property
       (match-beginning 0) (match-end 0) 'asciidoctor-double-bold (match-data t))
      ))

(defun asciidoctor-syntax-propertize-double-italic (start end)
  "Match options of type SYMBOL with REGEX from START to END."
  (goto-char start)
  (while (re-search-forward asciidoctor-regex-double-italic end t)
    ;; TODO: implement code block
    ;; (unless (asciidoctor-code-block-at-pos (match-beginning 0))
      (put-text-property
       (match-beginning 0) (match-end 0) 'asciidoctor-double-italic (match-data t))
      ))

(defvar asciidoctor--syntax-properties
  (list 'asciidoctor-heading nil
        'asciidoctor-heading-1-atx nil
        'asciidoctor-heading-2-atx nil
        'asciidoctor-heading-3-atx nil
        'asciidoctor-heading-4-atx nil
        'asciidoctor-heading-5-atx nil
        'asciidoctor-heading-6-atx nil
        'asciidoctor-option nil
        'asciidoctor-def-list nil
        'asciidoctor-double-bold nil
        'asciidoctor-double-italic nil)
  "Property list of all known asciidoctor syntactic properties.")

(defun asciidoctor-syntax-propertize (start end)
  "Function used as `syntax-propertize-function'.
START and END delimit region to propertize."
  (remove-text-properties start end asciidoctor--syntax-properties)
  ;; TODO: imlemenent additional propertizing
  ;; (markdown-syntax-propertize-fenced-block-constructs start end)
  ;; (markdown-syntax-propertize-yaml-metadata start end)
  ;; (markdown-syntax-propertize-pre-blocks start end)
  ;; (markdown-syntax-propertize-blockquotes start end)
  (asciidoctor-syntax-propertize-def-lists start end)
  (asciidoctor-syntax-propertize-double-bold start end)
  (asciidoctor-syntax-propertize-double-italic start end)
  (asciidoctor-syntax-propertize-options start end)
  (asciidoctor-syntax-propertize-headings start end)
  (asciidoctor-syntax-propertize-comments start end))


(defun asciidoctor-match-propertized-text (property last)
  "Match text with PROPERTY from point to LAST.
Restore match data previously stored in PROPERTY."
  (let ((saved (get-text-property (point) property))
        pos)
    (unless saved
      (setq pos (next-single-char-property-change (point) property nil last))
      (setq saved (get-text-property pos property)))
    (when saved
      (set-match-data saved)
      ;; Step at least one character beyond point. Otherwise
      ;; `font-lock-fontify-keywords-region' infloops.
      (goto-char (min (1+ (max (match-end 0) (point)))
                      (point-max)))
      saved)))


(defun asciidoctor-match-heading-1-atx (last)
  "Match level 1 ATX headings from point to LAST."
  (asciidoctor-match-propertized-text 'asciidoctor-heading-1-atx last))

(defun asciidoctor-match-heading-2-atx (last)
  "Match level 2 ATX headings from point to LAST."
  (asciidoctor-match-propertized-text 'asciidoctor-heading-2-atx last))

(defun asciidoctor-match-heading-3-atx (last)
  "Match level 3 ATX headings from point to LAST."
  (asciidoctor-match-propertized-text 'asciidoctor-heading-3-atx last))

(defun asciidoctor-match-heading-4-atx (last)
  "Match level 4 ATX headings from point to LAST."
  (asciidoctor-match-propertized-text 'asciidoctor-heading-4-atx last))

(defun asciidoctor-match-heading-5-atx (last)
  "Match level 5 ATX headings from point to LAST."
  (asciidoctor-match-propertized-text 'asciidoctor-heading-5-atx last))

(defun asciidoctor-match-heading-6-atx (last)
  "Match level 6 ATX headings from point to LAST."
  (asciidoctor-match-propertized-text 'asciidoctor-heading-6-atx last))

(defun asciidoctor-match-option (last)
  "Match option from point to LAST."
  (asciidoctor-match-propertized-text 'asciidoctor-option last))

(defun asciidoctor-match-def-list (last)
  "Match option from point to LAST."
  (asciidoctor-match-propertized-text 'asciidoctor-def-list last))

(defun asciidoctor-match-double-bold (last)
  "Match option from point to LAST."
  (asciidoctor-match-propertized-text 'asciidoctor-double-bold last))

(defun asciidoctor-match-double-italic (last)
  "Match option from point to LAST."
  (asciidoctor-match-propertized-text 'asciidoctor-double-italic last))

;; TODO: check with markdown what is this???
;; (defun asciidoctor-match-comments (last)
;;   "Match asciidoctor comments from the point to LAST."
;;   (when (and (skip-syntax-forward "^<" last))
;;     (let ((beg (point)))
;;       (when (and (skip-syntax-forward "^>" last) (< (point) last))
;;         (forward-char)
;;         (set-match-data (list beg (point)))
;;         t))))



;; (defun asciidoctor-match-bold (last)
;;   "Match inline bold from the point to LAST."
;;   (when (asciidoctor-match-inline-generic asciidoctor-regex-bold last)
;;     (let ((begin (match-beginning 2)) (end (match-end 2)))
;;       (cond
;;        ((asciidoctor-range-property-any
;;          begin end 'face (list asciidoctor-inline-code-face
;;                                asciidoctor-math-face))
;;         (goto-char (1+ (match-end 0)))
;;         (asciidoctor-match-bold last))
;;        (t
;;         (set-match-data (list (match-beginning 2) (match-end 2)
;;                           (match-beginning 3) (match-end 3)
;;                           (match-beginning 4) (match-end 4)
;;                           (match-beginning 5) (match-end 5)))
;;         (goto-char (1+ (match-end 0))))))))

;; (defun asciidoctor-match-italic (last)
;;   "Match inline italics from the point to LAST."
;;   (let ((regex (if (eq major-mode 'gfm-mode)
;;                    markdown-regex-gfm-italic markdown-regex-italic)))
;;     (when (markdown-match-inline-generic regex last)
;;       (let ((begin (match-beginning 1)) (end (match-end 1)))
;;         (cond
;;          ((markdown-range-property-any
;;            begin begin 'face (list markdown-url-face))
;;           ;; Italics shouldn't begin inside a URL due to an underscore
;;           (goto-char (min (1+ (match-end 0)) last))
;;           (markdown-match-italic last))
;;          ((markdown-range-property-any
;;            begin end 'face (list markdown-inline-code-face
;;                                  markdown-bold-face
;;                                  markdown-list-face
;;                                  markdown-math-face))
;;           (goto-char (1+ (match-end 0)))
;;           (markdown-match-italic last))
;;          (t
;;           (set-match-data (list (match-beginning 1) (match-end 1)
;;                                 (match-beginning 2) (match-end 2)
;;                                 (match-beginning 3) (match-end 3)
;;                                 (match-beginning 4) (match-end 4)))
;;           (goto-char (1+ (match-end 0)))))))))


(defvar asciidoctor-font-lock-keywords
  '((asciidoctor-match-heading-6-atx . ((1 asciidoctor-header-delimiter-face)
                                        (2 asciidoctor-header-face-6)
                                        (3 asciidoctor-header-delimiter-face)))
    (asciidoctor-match-heading-5-atx . ((1 asciidoctor-header-delimiter-face)
                                        (2 asciidoctor-header-face-5)
                                        (3 asciidoctor-header-delimiter-face)))
    (asciidoctor-match-heading-4-atx . ((1 asciidoctor-header-delimiter-face)
                                        (2 asciidoctor-header-face-4)
                                        (3 asciidoctor-header-delimiter-face)))
    (asciidoctor-match-heading-3-atx . ((1 asciidoctor-header-delimiter-face)
                                        (2 asciidoctor-header-face-3)
                                        (3 asciidoctor-header-delimiter-face)))
    (asciidoctor-match-heading-2-atx . ((1 asciidoctor-header-delimiter-face)
                                        (2 asciidoctor-header-face-2)
                                        (3 asciidoctor-header-delimiter-face)))
    (asciidoctor-match-heading-1-atx . ((1 asciidoctor-header-delimiter-face)
                                        (2 asciidoctor-header-face-1)
                                        (3 asciidoctor-header-delimiter-face)))
    (asciidoctor-match-option . ((1 asciidoctor-option-markup-face)
                                 (2 asciidoctor-option-face)
                                 (3 asciidoctor-option-markup-face)))
    (asciidoctor-match-def-list . ((1 asciidoctor-def-list-face)))
    (asciidoctor-match-double-bold . ((1 asciidoctor-bold-face)))
    (asciidoctor-match-double-italic . ((0 asciidoctor-italic-face)))

    ;; (asciidoctor-match-bold . ((1 asciidoctor-bold-markup-face prepend)
    ;;                            (2 asciidoctor-bold-face append)
    ;;                            (3 asciidoctor-bold-markup-face prepend)))
    ;; (asciidoctor-match-italic . ((1 asciidoctor-italic-markup-face prepend)
    ;;                              (2 asciidoctor-italic-face append)
    ;;                              (3 asciidoctor-italic-markup-face prepend)))
    )
  "Keyword highlighting specification for `asciidoctor-mode'.")

;; (setq asciidoctor-font-lock-keywords (asciidoctor-get-font-lock-keywords))


(defun asciidoctor-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line.
Return 0 if the current line is the first line in the buffer."
  (save-excursion
    (if (= (line-beginning-position) (point-min))
        0
      (forward-line -1)
      (current-indentation))))

(defun asciidoctor-cur-line-blank-p ()
  "Return true if current line is blank"
  (interactive)
      (save-excursion
        (beginning-of-line)
        (looking-at asciidoctor-regex-blank-line)))

(defun asciidoctor-prev-line-list-p ()
  "Return true if previous line is the beginning of the list item"
  (interactive)
      (save-excursion
        (forward-line -1)
        (looking-at asciidoctor-regex-list-item)))

(defun asciidoctor-cur-line-list-p ()
  "Return true if current line is the list item"
  (interactive)
      (save-excursion
        (beginning-of-line)
        (looking-at asciidoctor-regex-list-item)))

;; XXX: INEFFICIENT
(defun asciidoctor-prev-line-list-indent ()
  "Return indent level of previous line which is a list item"
  (interactive)
  (save-excursion
    (forward-line -1)
    (looking-at asciidoctor-regex-list-item)
    (- (match-end 0) (line-beginning-position))))

(defun asciidoctor-indent-list-item-below-p ()
  (and (asciidoctor-prev-line-list-p)
       (save-excursion
         (beginning-of-line)
         (not (looking-at asciidoctor-regex-list-item)))))

(defun asciidoctor-calc-indent ()
  (interactive)
  (cond
   ;; hanging list item like
   ;; * List item
   ;;   hanging part (cursor is on that line)
   ;; XXX: REFACTOR IT!!! Double time looking at list item
   ((asciidoctor-cur-line-list-p) 0)
   ((asciidoctor-indent-list-item-below-p) (asciidoctor-prev-line-list-indent))
   ;; blank line -- no indent
   ;; ((asciidoctor-cur-line-blank-p) 0)
   ;; use previous indent as a fallback
   (t (asciidoctor-prev-line-indent))))

(defun asciidoctor-indent-line ()
  (interactive)
  (let ((cur-col (current-column))
        (_ (back-to-indentation))
        (cur-indented-col (current-column))
        (new-indent (asciidoctor-calc-indent)))
    (indent-line-to new-indent)
    (move-to-column (+ cur-col (- new-indent cur-indented-col)))))




(defun asciidoctor-adaptive-fill-function ()
  "Return prefix for filling paragraph or nil if not determined."
  (cond
   ;;;; List item inside blockquote
   ;; ((looking-at "^[ \t]*>[ \t]*\\(\\(?:[0-9]+\\|#\\)\\.\\|[*+:-]\\)[ \t]+")
   ;;  (markdown-replace-regexp-in-string
   ;;   "[0-9\\.*+-]" " " (match-string-no-properties 0)))
   ;;;; Blockquote
   ;; ((looking-at markdown-regex-blockquote)
   ;;  (buffer-substring-no-properties (match-beginning 0) (match-end 2)))
   ;; List items
   ((looking-at asciidoctor-regex-list-item)
    (match-string-no-properties 0))
   ;; No match
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Electric
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun asciidoctor-electric-list-should-indent-p ()
  (interactive)
    (and
     ;; nothing till the end
     (looking-at "[[:blank:]]*$")
     ;; list item in the beginning
     (save-excursion
       (beginning-of-line)
       (looking-at asciidoctor-regex-list-item))))

(defun asciidoctor-electric-indent (char)
  (cond 
   ((eq char ?\n) t)
   ((eq char ?\s) (asciidoctor-electric-list-should-indent-p))
   (t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clipboard image -- save image and paste link
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun asciidoctor-imagesdir ()
  "Return name of the image directory. It is either current buffer directory or 
combined current buffer directory / :imagesdir: (stated at the top of the buffer)."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^[[:blank:]]*$")
    (let ((end-pos (point)))
      (goto-char (point-min))
      (search-forward-regexp "^:imagesdir:[[:blank:]]+\\([[:alnum:]-._]+\\)" end-pos t)
      (match-string-no-properties 1))))


(defun asciidoctor-extract-index-from-file-name (filename)
  "Return index of the generated image file name.
`img_document_23.png` --> 23"
  (string-to-number
   (replace-regexp-in-string "^img_.*_\\([[:digit:]]+\\).*" "\\1" filename)))


(defun asciidoctor-image-full-path ()
  "Return full path of the image directory."
  (concat (file-name-directory (buffer-file-name))
          (asciidoctor-imagesdir)
          "/"))

(defun asciidoctor-list-generated-images (path)
  "Return list of all generated images for current buffer."
  (directory-files path nil
                   (concat "^img_" (file-name-base) "_[[:digit:]]+\\.png")))

(defun asciidoctor-generate-new-image-index (list-of-images)
  "Return the next non-existent index of the current buffer image.
'(\"img_document_1.png\" \"img_document_2.png\") --> 3"
  (let ((indices (mapcar 'asciidoctor-extract-index-from-file-name
                         list-of-images)))
    (if indices
        (1+ (seq-max indices)))
    1))

(defun asciidoctor-image-generate-name (path)
  "Return new name for the generated image in the given path."
  (let ((base-name (concat "img_"(ignore-errors (file-name-base))))
        (new-index (asciidoctor-generate-new-image-index (asciidoctor-list-generated-images path))))
    (concat base-name "_" (number-to-string new-index) ".png")))

(defun asciidoctor-save-image ()
  "Save image from the clipboard to the document image path using generated image filename.
Use GraphicsMagick for that.
Create directory if needed."
  (let ((path (asciidoctor-image-full-path)))
    (unless (file-exists-p path)
      (make-directory path t))
    (let ((filename (asciidoctor-image-generate-name path)))
      (shell-command
       (concat "gm convert clipboard: " path filename))
      filename)))

(defun asciidoctor-save-image-insert-link ()
  "Save image to the document image path and insert link to the document."
  (interactive)
  (let ((filename (asciidoctor-save-image)))
    (insert "image::" filename "[]")))



;;;###autoload
(define-derived-mode asciidoctor-mode text-mode "AsciiDoctor"
  "Major mode for editing AsciiDoc(or) files."

  ;; :syntax-table asciidoctor-mode-syntax-table
  
  (setq-local tab-width 4)

  ;; Comments
  (setq-local comment-column 0)
  (setq-local comment-auto-fill-only-comments nil)
  (setq-local comment-start "// ")
  (setq-local comment-end "") 
  (setq-local comment-use-syntax t)
  (setq-local comment-start-skip "^//[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\(?:\n\\|\\'\\)")

  
  ;; Font lock.
  (setq-local asciidoctor-mode-font-lock-keywords asciidoctor-font-lock-keywords)
  (setq-local font-lock-defaults '(asciidoctor-mode-font-lock-keywords
                                   nil nil nil nil
                                   (font-lock-syntactic-face-function . asciidoctor-syntactic-face)))
  
  (setq-local font-lock-multiline t)

  ;; Syntax
  (add-hook 'syntax-propertize-extend-region-functions
            'asciidoctor-syntax-propertize-extend-region)
  (setq-local syntax-propertize-function 'asciidoctor-syntax-propertize)

                    ;; "^[ \t]*\\([0-9]+\\.|\\.\\)[ \t]+" ; ordered list item

  ;; Paragraph filling
  (set
   ;; Should match start of lines that start or separate paragraphs
   (make-local-variable 'paragraph-start)
       (mapconcat #'identity
                  '(
                    "\f" ; starts with a literal line-feed
                    "^[ \t\f]*$" ; space-only line
                    "^[ \t]*[*+-]+[ \t]+" ; unordered list item
                    "^[ \t]*[0-9]*\\.[ \t]+" ; ordered list item
                    "[ \t]*:[ \t]+" ; definition
                    )
                  "\\|"))
  (set
   ;; Should match lines that separate paragraphs without being
   ;; part of any paragraph:
   (make-local-variable 'paragraph-separate)
   (mapconcat #'identity
              '("[ \t\f]*$" ; space-only line
                "^=+[[:blank:]]" ; Headings
                "^\\[.+\\][[:blank:]]*$" ; Blocks (source, quotes, etc)
                "^====[[:blank:]]*$" ; block separator 
                "^____[[:blank:]]*$" ; block separator
                "^\\*\\{4\\}[[:blank:]]*$" ; block separator
                "^\\+\\{4\\}[[:blank:]]*$" ; block separator
                "^\\.\\{4\\}[[:blank:]]*$" ; block separator
                "^-\\{2,4\\}[[:blank:]]*$" ; block separator
                "[ \t]*\\[\\^\\S-*\\]:[ \t]*$") ; just the start of a footnote def
              "\\|"))
  (setq-local adaptive-fill-first-line-regexp "\\`[ \t]*[A-Z]?>[ \t]*?\\'")
  (setq-local adaptive-fill-regexp "\\s-*")
  (setq-local adaptive-fill-function #'asciidoctor-adaptive-fill-function)
  ;; (setq-local fill-forward-paragraph-function #'markdown-fill-forward-paragraph)


  ;; Outline mode
  (make-local-variable 'outline-regexp)
  (setq outline-regexp asciidoctor-regex-header)
  (make-local-variable 'outline-level)
  (setq outline-level 'asciidoctor-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))


  ;; Indentation
  (setq indent-line-function 'asciidoctor-indent-line)
  ;; XXX: How to make it local?
  (add-hook 'electric-indent-functions 'asciidoctor-electric-indent)
  
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . asciidoctor-mode) t)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . asciidoctor-mode) t)

(provide 'asciidoctor-mode)
;;; asciidoctor-mode.el ends here


