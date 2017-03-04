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

;; (defgroup asciidoctor nil
;;   "Major mode for editing text files in AsciiDoc format."
;;   :prefix "asciidoctor-")


;; (defcustom asciidoctor-pdf-stylesdir
;;   ""
;;   "Directory with asciidoctor-pdf themes."
;;   :group 'asciidoctor-pdf
;;   :type 'string)

;; (defcustom asciidoctor-pdf-fontsdir
;;   ""
;;   "Directory with asciidoctor-pdf fonts."
;;   :group 'asciidoctor-pdf
;;   :type 'string)

;; (defcustom asciidoctor-pdf-extensions
;;   ""
;;   "Extensions such as asciidoctor-diagram."
;;   :group 'asciidoctor-pdf
;;   :type 'string)



;;; Font Lock

;; (require 'font-lock)


;; (defface asciidoctor-title-face
;;   '((t (:inherit default :height 2.0 :weight bold)))
;;   "Document's title"
;;   :group 'asciidoctor-faces)

;; (defface asciidoctor-header1-face
;;   '((t (:inherit default :height 1.6 :weight bold)))
;;   "Header level 1"
;;   :group 'asciidoctor-faces)

;; (defface asciidoctor-header2-face
;;   '((t (:inherit default :height 1.3 :weight bold)))
;;   "Header level 2"
;;   :group 'asciidoctor-faces)

;; (defface asciidoctor-header3-face
;;   '((t (:inherit default :height 1.1 :weight bold)))
;;   "Header level 3"
;;   :group 'asciidoctor-faces)

;; (defface asciidoctor-header4-face
;;   '((t (:inherit default :height 1.0 :weight bold)))
;;   "Header level 4"
;;   :group 'asciidoctor-faces)

;; (defvar asciidoctor-title-face 'asciidoctor-title-face)
;; (defvar asciidoctor-header1-face 'asciidoctor-header1-face)
;; (defvar asciidoctor-header2-face 'asciidoctor-header2-face)
;; (defvar asciidoctor-header3-face 'asciidoctor-header3-face)
;; (defvar asciidoctor-header4-face 'asciidoctor-header4-face)




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


;; (defconst asciidoctor-mode-syntax-table
;;   (let ((table (make-syntax-table)))
;;     (modify-syntax-entry ?$ "." table)
;;     (modify-syntax-entry ?% "." table)
;;     (modify-syntax-entry ?& "." table)
;;     (modify-syntax-entry ?' "." table)
;;     (modify-syntax-entry ?` "." table)
;;     (modify-syntax-entry ?\" "." table)
;;     (modify-syntax-entry ?* "." table )
;;     (modify-syntax-entry ?+ "." table )
;;     (modify-syntax-entry ?. "." table )
;;     (modify-syntax-entry ?/ "." table )
;;     (modify-syntax-entry ?< "." table )
;;     (modify-syntax-entry ?= "." table )
;;     (modify-syntax-entry ?> "." table )
;;     (modify-syntax-entry ?\\ "." table)
;;     (modify-syntax-entry ?| "." table)
;;     (modify-syntax-entry ?_ "." table)
    
;;     table)
;;   "Syntax table for `asciidoctor-mode'")




;; (defun asciidoctor-make-one-line-title (sub-type level text)
;;   "Returns a one line title of LEVEL and SUB-TYPE containing the given text."
;;   (let ((del (make-string (+ level 1) ?=)))
;;     (concat del " " text (when (eq sub-type 2) (concat " " del)))))


;; (defun asciidoctor-kwf-std (end regexp &optional must-free-groups no-block-del-groups)
;;   "Standart function for keywords

;; Intendent to be called from font lock keyword functions. END is
;; the limit of the search. REXEXP the regexp to be searched.
;; MUST-FREE-GROUPS a list of regexp group numbers which may not
;; match text that has an asciidoctor-reserved text-property with a non-nil
;; value. Likewise, groups in NO-BLOCK-DEL-GROUPS may not contain
;; text having asciidoctor-reserved set to 'block-del."
;;   (let ((found t) (prevented t) saved-point)
;;     (while (and found prevented (<= (point) end) (not (eobp)))
;;       (setq saved-point (point))
;;       (setq found (re-search-forward regexp end t))
;;       (setq prevented 
;; 	    (and found
;; 		 (or
;; 		  (some (lambda(x)
;; 			  (and (match-beginning x)
;; 			       (text-property-not-all (match-beginning x)
;; 						      (match-end x)
;; 						      'asciidoctor-reserved nil)))
;; 			must-free-groups)
;; 		  (some (lambda(x)
;; 			  (and (match-beginning x))
;; 			  (text-property-any (match-beginning x)
;; 			         	     (match-end x)
;; 					     'asciidoctor-reserved 'block-del))
;; 			no-block-del-groups))))
;;       (when (and found prevented (<= (point) end))
;; 	(goto-char (1+ saved-point))))
;;     (and found (not prevented))))


;; (defconst asciidoctor-title-max-level 4
;;   "Max title level, counting starts at 0.")

;; (defun asciidoctor-re-one-line-title (level)
;;   "Returns a regex matching a one line title of the given LEVEL.
;; When LEVEL is nil, a one line title of any level is matched.

;; match-data has these sub groups:
;; 1 leading delimiter inclusive whites between delimiter and title text
;; 2 title's text exclusive leading/trailing whites
;; 3 trailing delimiter with all whites 
;; 4 trailing delimiter only inclusive whites between title text and delimiter
;; 0 only chars that belong to the title block element

;; ==  my title  ==  n
;; ---12------23------
;;             4--4"
;;   (let* ((del (if level
;;                  (make-string (+ level 1) ?=)
;;                (concat "=\\{1," (+ asciidoctor-title-max-level 1) "\\}"))))
;;     (concat
;;      "^\\(" del "[ \t]+\\)"		      ; 1
;;      "\\([^ \t\n].*?\\)"                          ; 2
;;      ;; using \n instad $ is important so group 3 is guaranteed to be at least 1
;;      ;; char long (except when at the end of the buffer()). That is important to
;;      ;; to have a place to put the text property asciidoctor-reserved on.
;;      "\\(\\([ \t]+" del "\\)?[ \t]*\\(?:\n\\|\\'\\)\\)" )))

;; (defun asciidoctor-kw-one-line-title (level text-face)
;;   "Creates a keyword for font-lock which highlights one line titles"
;;   (list
;;    `(lambda (end) (asciidoctor-kwf-std end ,(asciidoctor-re-one-line-title level) '(0)))
;;     '(1 '(face nil asciidoctor-reserved block-del) t)
;;     `(2 ,text-face t) 
;;     '(3  '(face nil asciidoctor-reserved block-del) t)
;;     '(4 '(face nil) t t)))


;; (defun asciidoctor-font-lock-mark-block-function ()
;;   (mark-paragraph 2)
;;   (forward-paragraph -1))

;; (defun asciidoctor-get-font-lock-keywords ()
;;   (list
   
;;    ;; Asciidoc BUG: Lex.next has a different order than the following extract
;;    ;; from the documentation states.
   
;;    ;; When a block element is encountered asciidoc(1) determines the type of
;;    ;; block by checking in the following order (first to last): (section)
;;    ;; Titles, BlockMacros, Lists, DelimitedBlocks, Tables, AttributeEntrys,
;;    ;; AttributeLists, BlockTitles, Paragraphs.

;;    ;; sections / document structure
;;    ;; ------------------------------
;;    (asciidoctor-kw-one-line-title 0 asciidoctor-title-face)
;;    (asciidoctor-kw-one-line-title 1 asciidoctor-header1-face)
;;    (asciidoctor-kw-one-line-title 2 asciidoctor-header2-face)
;;    (asciidoctor-kw-one-line-title 3 asciidoctor-header3-face)
;;    (asciidoctor-kw-one-line-title 4 asciidoctor-header4-face)

;;    ;; ;; block macros 
;;    ;; ;; ------------------------------
;;    ;; ;; todo: respect asciidoc.conf order

;;    ;; ;; -- system block macros
;;    ;; ;;     # Default system macro syntax.
;;    ;; ;; SYS_RE = r'(?u)^(?P<name>[\\]?\w(\w|-)*?)::(?P<target>\S*?)' + \
;;    ;; ;;          r'(\[(?P<attrlist>.*?)\])$'
;;    ;; ;; conditional inclusion
;;    ;; (list "^\\(\\(?:ifn?def\\|endif\\)::\\)\\([^ \t\n]*?\\)\\(\\[\\).+?\\(\\]\\)[ \t]*$"
;;    ;;       '(1 '(face asciidoctor-preprocessor asciidoctor-reserved block-del))    ; macro name
;;    ;;       '(2 '(face asciidoctor-delimiter asciidoctor-reserved block-del))       ; condition
;;    ;;       '(3 '(face asciidoctor-hide-delimiter asciidoctor-reserved block-del))  ; [
;;    ;;       ; ... attribute list content = the conditionaly included text
;;    ;;       '(4 '(face asciidoctor-hide-delimiter asciidoctor-reserved block-del))) ; ]
;;    ;; ;; include
;;    ;; (list "^\\(\\(include1?::\\)\\([^ \t\n]*?\\)\\(\\[\\)\\(.*?\\)\\(\\]\\)\\)[ \t]*$"
;;    ;;       '(1 '(face nil asciidoctor-reserved block-del)) ; the whole match
;;    ;;       '(2 asciidoctor-preprocessor)           ; macro name
;;    ;;       '(3 asciidoctor-delimiter)              ; file name
;;    ;;       '(4 asciidoctor-hide-delimiter)         ; [
;;    ;;       '(5 asciidoctor-delimiter)              ;   attribute list content
;;    ;;       '(6 asciidoctor-hide-delimiter))        ; ]


;;    ;; ;; -- special block macros
;;    ;; ;; ruler line.
;;    ;; ;; Is a block marcro in asciidoc.conf, altough manual has it in the "text formatting" section 
;;    ;; ;; ^'{3,}$=#ruler
;;    ;; (list "^\\('\\{3,\\}+\\)[ \t]*$"
;;    ;;       '(1 '(face asciidoctor-complex-replacement asciidoctor-reserved block-del))) 
;;    ;; ;; forced pagebreak
;;    ;; ;; Is a block marcro in asciidoc.conf, altough manual has it in the "text formatting" section 
;;    ;; ;; ^<{3,}$=#pagebreak
;;    ;; (list "^\\(<\\{3,\\}+\\)[ \t]*$"
;;    ;;       '(1 '(face asciidoctor-delimiter asciidoctor-reserved block-del))) 
;;    ;; ;; comment
;;    ;; ;; (?mu)^[\\]?//(?P<passtext>[^/].*|)$
;;    ;; ;; I don't know what the [\\]? should mean
;;    ;; (list "^\\(//\\(?:[^/].*\\|\\)\\(?:\n\\|\\'\\)\\)"
;;    ;;       '(1 '(face markup-comment-face asciidoctor-reserved block-del)))    
;;    ;; ;; image. The first positional attribute is per definition 'alt', see
;;    ;; ;; asciidoc manual, sub chapter 'Image macro attributes'.
;;    ;; (list `(lambda (end) (asciidoctor-kwf-std end ,(asciidoctor-re-block-macro "image") '(0)))
;;    ;;       '(0 '(face markup-meta-face asciidoctor-reserved block-del) t) ; whole match
;;    ;;       '(1 markup-complex-replacement-face t)	; 'image'  
;;    ;;       '(2 markup-internal-reference-face t)  ; file name
;;    ;;       '(3 '(face markup-meta-face asciidoctor-reserved nil asciidoctor-attribute-list ("alt")) t)) ; attribute list
			  
;;    ;; ;; passthrough: (?u)^(?P<name>pass)::(?P<subslist>\S*?)(\[(?P<passtext>.*?)\])$
;;    ;; ;; todo

;;    ;; ;; -- general block macro
;;    ;; (list `(lambda (end) (asciidoctor-kwf-std end ,(asciidoctor-re-block-macro) '(0)))
;;    ;;       '(0 '(face markup-meta-face asciidoctor-reserved block-del)) ; whole match
;;    ;;       '(1 markup-command-face t)			       ; command name
;;    ;;       '(3 '(face markup-meta-face asciidoctor-reserved nil asciidoctor-attribute-list t) t)) ; attribute list

;;    ;; ;; Delimited blocks
;;    ;; ;; ------------------------------
;;    ;; (asciidoctor-kw-delimited-block 0 markup-comment-face)   ; comment
;;    ;; (asciidoctor-kw-delimited-block 1 markup-passthrough-face) ; passthrough
;;    ;; (asciidoctor-kw-delimited-block 2 markup-code-face) ; listing
;;    ;; (asciidoctor-kw-delimited-block 3 markup-verbatim-face) ; literal
;;    ;; (asciidoctor-kw-delimited-block 4 nil t) ; quote    
;;    ;; (asciidoctor-kw-delimited-block 5 nil t) ; example  
;;    ;; (asciidoctor-kw-delimited-block 6 asciidoctor-secondary-text t) ; sidebar
;;    ;; (asciidoctor-kw-delimited-block 7 nil t) ; open block
;;    ;; (asciidoctor-kw-delimiter-line-fallback)  


;;    ;; ;; tables
;;    ;; ;; ------------------------------
;;    ;; ;; must come BEFORE block title, else rows starting like .2+| ... | ... are taken as 
;;    ;; (cons "^|=\\{3,\\}[ \t]*$" 'asciidoctor-table-del ) ; ^\|={3,}$
;;    ;; (list (concat "^"                  "\\(" (asciidoctor-re-cell-specifier) "\\)" "\\(|\\)"
;;    ;;               "\\(?:[^|\n]*?[ \t]" "\\(" (asciidoctor-re-cell-specifier) "\\)" "\\(|\\)"
;;    ;;               "\\(?:[^|\n]*?[ \t]" "\\(" (asciidoctor-re-cell-specifier) "\\)" "\\(|\\)"
;;    ;;               "\\(?:[^|\n]*?[ \t]" "\\(" (asciidoctor-re-cell-specifier) "\\)" "\\(|\\)" "\\)?\\)?\\)?")
;;    ;;       '(1 '(face asciidoctor-delimiter asciidoctor-reserved block-del) nil t) '(2 '(face asciidoctor-table-del asciidoctor-reserved block-del) nil t)
;;    ;;       '(3 '(face asciidoctor-delimiter asciidoctor-reserved block-del) nil t) '(4 '(face asciidoctor-table-del asciidoctor-reserved block-del) nil t)
;;    ;;       '(5 '(face asciidoctor-delimiter asciidoctor-reserved block-del) nil t) '(6 '(face asciidoctor-table-del asciidoctor-reserved block-del) nil t)
;;    ;;       '(7 '(face asciidoctor-delimiter asciidoctor-reserved block-del) nil t) '(8 '(face asciidoctor-table-del asciidoctor-reserved block-del) nil t))
   

;;    ;; ;; attribute entry
;;    ;; ;; ------------------------------
;;    ;; (list (asciidoctor-re-attribute-entry) '(1 asciidoctor-delimiter) '(2 asciidoctor-secondary-text nil t))


;;    ;; ;; attribute list
;;    ;; ;; ----------------------------------

;;    ;; ;; --- special attribute lists
;;    ;; ;; quote/verse
;;    ;; (list (concat
;;    ;;        "^\\("
;;    ;;          "\\(\\[\\)"
;;    ;;          "\\(quote\\|verse\\)"
;;    ;;          "\\(?:\\(,\\)\\(.*?\\)\\(?:\\(,\\)\\(.*?\\)\\)?\\)?"
;;    ;;          "\\(\\]\\)"
;;    ;;        "\\)[ \t]*$")
;;    ;;       '(1 '(face nil asciidoctor-reserved block-del)) ; whole match
;;    ;;       '(2 asciidoctor-hide-delimiter)         ; [
;;    ;;       '(3 asciidoctor-delimiter)              ;   quote|verse
;;    ;;       '(4 asciidoctor-hide-delimiter nil t)   ;   ,
;;    ;;       '(5 asciidoctor-secondary-text nil t)   ;   attribution(author)
;;    ;;       '(6 asciidoctor-delimiter nil t)        ;   ,
;;    ;;       '(7 asciidoctor-secondary-text nil t)   ;   cite title
;;    ;;       '(8 asciidoctor-hide-delimiter))        ; ]
;;    ;; ;; admonition block
;;    ;; (list "^\\(\\[\\(?:CAUTION\\|WARNING\\|IMPORTANT\\|TIP\\|NOTE\\)\\]\\)[ \t]*$"
;;    ;;       '(1 '(face asciidoctor-complex-replacement asciidoctor-reserved block-del)))
;;    ;; ;; block id
;;    ;; (list `(lambda (end) (asciidoctor-kwf-std end ,(asciidoctor-re-anchor 'block-id) '(0)))
;;    ;; 	 '(0 '(face markup-meta-face asciidoctor-reserved block-del))
;;    ;; 	 '(1 markup-anchor-face t)
;;    ;; 	 '(2 markup-secondary-text-face t t))

;;    ;; ;; --- general attribute list block element
;;    ;; ;; ^\[(?P<attrlist>.*)\]$
;;    ;; (list '(lambda (end) (asciidoctor-kwf-std end "^\\(\\[\\(.*\\)\\]\\)[ \t]*$" '(0)))
;;    ;;       '(1 '(face markup-meta-face asciidoctor-reserved block-del))
;;    ;; 	 '(2 '(face markup-meta-face asciidoctor-attribute-list t)))


;;    ;; ;; block title
;;    ;; ;; -----------------------------------
;;    ;; (asciidoctor-kw-block-title)


;;    ;; ;; paragraphs
;;    ;; ;; --------------------------
;;    ;; (asciidoctor-kw-verbatim-paragraph-sequence)
;;    ;; (asciidoctor-kw-admonition-paragraph)
;;    ;; (list "^[ \t]+$" '(0 '(face nil asciidoctor-reserved block-del) t))

;;    ;; ;; Inline substitutions
;;    ;; ;; ==========================================
;;    ;; ;; Inline substitutions within block elements are performed in the
;;    ;; ;; following default order:
;;    ;; ;; -. Passtrough stuff removal (seen in asciidoc source)
;;    ;; ;; 1. Special characters
;;    ;; ;; 2. Quotes
;;    ;; ;; 3. Special words
;;    ;; ;; 4. Replacements
;;    ;; ;; 5. Attributes
;;    ;; ;; 6. Inline Macros
;;    ;; ;; 7. Replacements2


;;    ;; ;; (passthrough stuff removal)
;;    ;; ;; ------------------------
;;    ;; ;; todo. look in asciidoc source how exactly asciidoc does it
;;    ;; ;; 1) BUG: actually only ifdef::no-inline-literal[]
;;    ;; ;; 2) TODO: in asciidod.conf (but not yet here) also in inline macro section

;;    ;; ;; AsciiDoc Manual: constitutes an inline literal passthrough. The enclosed
;;    ;; ;; text is rendered in a monospaced font and is only subject to special
;;    ;; ;; character substitution.
;;    ;; (asciidoctor-kw-quote 'asciidoctor-constrained "`" markup-typewriter-face nil nil t)     ;1)
;;    ;; ;; AsciiDoc Manual: The triple-plus passthrough is functionally identical to
;;    ;; ;; the pass macro but you don’t have to escape ] characters and you can
;;    ;; ;; prefix with quoted attributes in the inline version
;;    ;; (asciidoctor-kw-quote 'asciidoctor-unconstrained "+++" markup-typewriter-face nil nil t) ;2)
;;    ;; ;;The double-dollar passthrough is functionally identical to the triple-plus
;;    ;; ;;passthrough with one exception: special characters are escaped.
;;    ;; (asciidoctor-kw-quote 'asciidoctor-unconstrained "$$" markup-typewriter-face nil nil t)  ;2)
;;    ;; ;; todo: add pass:[...], latexmath:[...], asciimath[...]

;;    ;; ;; special characters
;;    ;; ;; ------------------
;;    ;; ;; no highlighting for them, since they are a property of the backend markup,
;;    ;; ;; not of AsciiDoc syntax


;;    ;; ;; quotes: unconstrained and constrained
;;    ;; ;; order given by asciidoc.conf
;;    ;; ;; ------------------------------
;;    ;; (asciidoctor-kw-quote 'asciidoctor-unconstrained "**" markup-strong-face)
;;    ;; (asciidoctor-kw-quote 'asciidoctor-constrained "*" markup-strong-face)
;;    ;; (asciidoctor-kw-quote 'asciidoctor-constrained "``" nil asciidoctor-replacement "''") ; double quoted text
;;    ;; (asciidoctor-kw-quote 'asciidoctor-constrained "'" markup-emphasis-face)	   ; single quoted text
;;    ;; (asciidoctor-kw-quote 'asciidoctor-constrained "`" nil asciidoctor-replacement "'")
;;    ;; ;; `...` , +++...+++, $$...$$ are within passthrough stuff above
;;    ;; (asciidoctor-kw-quote 'asciidoctor-unconstrained "++" markup-typewriter-face) ; AsciiDoc manual: really onl '..are rendered in a monospaced font.'
;;    ;; (asciidoctor-kw-quote 'asciidoctor-constrained "+" markup-typewriter-face) 
;;    ;; (asciidoctor-kw-quote 'asciidoctor-unconstrained  "__" markup-emphasis-face)
;;    ;; (asciidoctor-kw-quote 'asciidoctor-constrained "_" markup-emphasis-face)
;;    ;; (asciidoctor-kw-quote 'asciidoctor-unconstrained "##" markup-gen-face) ; unquoted text
;;    ;; (asciidoctor-kw-quote 'asciidoctor-constrained "#" markup-gen-face)    ; unquoted text
;;    ;; (asciidoctor-kw-quote 'asciidoctor-unconstrained "~" (asciidoctor-facespec-subscript)) ; subscript
;;    ;; (asciidoctor-kw-quote 'asciidoctor-unconstrained "^" (asciidoctor-facespec-superscript)) ; superscript
    

;;    ;; ;; special words
;;    ;; ;; --------------------
;;    ;; ;; there are no default special words to highlight


;;    ;; ;; attributes
;;    ;; ;; ---------------------------------
;;    ;; ;; attribute refrence
;;    ;; (cons "{\\(\\w+\\(?:\\w*\\|-\\)*\\)\\([=?!#%@$][^}\n]*\\)?}" 'asciidoctor-replacement) 


;;    ;; ;; inline macros (that includes anchors, links, footnotes,....)
;;    ;; ;; ------------------------------
;;    ;; ;; todo: make asciidoctor-kw-... macros to have less redundancy
;;    ;; ;; Note: Some regexp/kewyords are within the macro section 
;;    ;; ;; TODO:
;;    ;; ;; - allow multiline
;;    ;; ;; - currently escpapes are not looked at
;;    ;; ;; - adapt to the asciidoctor-reserved scheme
;;    ;; ;; - same order as in asciidoc.conf (is that in 'reverse'? cause 'default syntax' comes first)

;;    ;; ;; Macros using default syntax, but having special highlighting in asciidoctor-mode
;;    ;; (asciidoctor-kw-inline-macro-urls-no-attribute-list)
;;    ;; (asciidoctor-kw-inline-macro-urls-attribute-list)
;;    ;; (asciidoctor-kw-standalone-urls)

;;    ;; ;; Macros using default syntax and having default highlighting in asciidoctor-mod
;;    ;; (asciidoctor-kw-inline-macro)

;;    ;; ;; -- misc 
;;    ;; (asciidoctor-kw-first-whites-fixed-width)

;;    ;; ;; content of attribute lists
;;    ;; (list 'asciidoctor-kwf-attribute-list)

;;    ))

(defvar asciidoctor-font-lock-keywords nil
  "Keyword highlighting specification for `asciidoctor-mode'.")

;; (setq asciidoctor-font-lock-keywords (asciidoctor-get-font-lock-keywords))






;;;###autoload
(define-derived-mode asciidoctor-mode text-mode "AsciiDoctor"
  "Major mode for editing AsciiDoc(or) files."

  ;; :syntax-table asciidoctor-mode-syntax-table
  
  (setq-local tab-width 4)

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "") 
  (setq-local comment-use-syntax t)
  (setq-local comment-start-skip "^//[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\(?:\n\\|\\'\\)")

  
  ;; font lock
  ;; (set (make-local-variable 'font-lock-defaults)
  ;;      '(asciidoctor-font-lock-keywords
  ;;        nil nil nil nil
  ;;        (font-lock-multiline . t)
  ;;        (font-lock-mark-block-function . asciidoctor-font-lock-mark-block-function)))
  
  ;; (setq-local font-lock-defaults
  ;;             '(asciidoctor-font-lock-keywords
  ;;               nil nil nil nil
  ;;               (font-lock-multiline .t)
  ;;               (font-lock-mark-block-function . asciidoctor-font-lock-mark-block-function)))

  ;; (make-local-variable 'font-lock-extra-managed-props)
  ;; (setq font-lock-extra-managed-props '(asciidoctor-reserved asciidoctor-attribute-list))

  ;; (make-local-variable 'font-lock-unfontify-region-function)
  ;; (setq font-lock-unfontify-region-function 'font-lock-default-unfontify-region-function)

  ;; (set (make-local-variable 'parse-sexp-lookup-properties) t)

  
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . asciidoctor-mode) t)

(provide 'asciidoctor-mode)
;;; asciidoctor-mode.el ends here


