(provide 'haba-latex)


;; (setq org-latex-default-table-environment "tabulary")
;; Pandoc
(use-package ox-pandoc)

;; Latex
(require 'ox-latex)


;; (setq org-latex-image-default-width nil)
(setq org-latex-pdf-process
      '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-latex-hyperref-template "\\hypersetup{\n pdfencoding=auto,\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L}}\n"
)

(setq org-latex-default-packages-alist
      '(("" "indentfirst" t) ; первая строка параграфа сдвинута
        ("" "float" nil) ; расположение таблиц и рисунков "точно здесь!" [H]
        ("" "misccorr" t) ; точка в номерах заголовков
        ("onehalfspacing" "setspace" t) ; межстрочный интервал
        ;; ("" "tabulary" t) ; Таблицы с возможностью "спана"
        ("" "array" t) ; Таблицы
        ("" "fixltx2e" nil)
        ("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil) ; Таблицы на несколько страниц
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("hidelinks" "hyperref" nil)))

(defun haba/org-latex-class-common (lang-main lang-other)
  (concat "\\usepackage{fontspec}
         \\defaultfontfeatures{Ligatures=TeX}
         \\setmainfont{Roboto}
         \\setsansfont{Roboto Condensed}
         \\newfontfamily{\\cyrillicfonttt}{Roboto Mono}
         \\setmonofont{Roboto Mono}
         \\newcommand\\quotefont{\\fontspec[Colour=55555500]{Roboto}}
         \\usepackage{polyglossia}
         \\setdefaultlanguage{" lang-main "}
         \\setotherlanguages{" lang-other "}
         \\usepackage[top=25mm, left=20mm, right=20mm, bottom=25mm]{geometry}
         \\renewcommand{\\arraystretch}{1.5}
         \\newcommand{\\theadfst}[1]{\\multicolumn{1}{|c|}{\\textbf{#1}}}
         \\newcommand{\\thead}[1]{\\multicolumn{1}{c|}{\\textbf{#1}}}
         "))
         ;; \\usepackage{enumitem}
         ;; \\setlist{nosep}"))

(defun haba/org-latex-class-hf-logo-2(logo-left vmove-left logo-right vmove-right)
  (interactive "P")
  (format "\\usepackage{fancyhdr}
             \\pagestyle{fancy}
             \\fancypagestyle{plain}{\\pagestyle{fancy}}
             %s
             \\chead{}
             %s
             \\lfoot{} \\cfoot{} \\rfoot{\\thepage}
             \\renewcommand{\\headrulewidth}{0.0pt}
             \\renewcommand{\\footrulewidth}{0.0pt}"
          (if (string= logo-left "")
              "\\lhead{}"
            (format
             "\\lhead{\\includegraphics[scale=1%s]{%s/logo/%s}}"
             (if (string= vmove-left "") "" (format ",trim=0 %s 0 0" vmove-left))
             org-directory logo-left))
          (if (string= logo-right "")
              "\\rhead{}"
            (format
             "\\rhead{\\includegraphics[scale=1%s]{%s/logo/%s}}"
             (if (string= vmove-right "") "" (format ",trim=0 %s 0 0" vmove-right))
             org-directory logo-right))))



(setq haba/org-latex-class-titling
      "\\usepackage{titling}
         \\pretitle{\\begin{center}\\LARGE\\bfseries\\sffamily}
         \\posttitle{\\par\\end{center}\\vspace{24bp}}
         \\preauthor{\\begin{center}\\normalsize\\sffamily}
         \\postauthor{\\par\\end{center}}
         \\predate{\\begin{center}\\normalsize\\sffamily}
         \\date{}
         \\postdate{\\par\\end{center}}")
(setq haba/org-latex-class-hf-std
      "\\usepackage{fancyhdr}
         \\pagestyle{fancy}
         \\fancypagestyle{plain}{\\pagestyle{fancy}}
         \\lhead{} \\chead{} \\rhead{\\today}
         \\lfoot{} \\cfoot{} \\rfoot{\\thepage}")
(setq haba/org-latex-class-dot-in-chapters
      "\\usepackage{misccorr} % Точка в номерах заголовков
         \\usepackage{titlesec}
         \\titleformat*{\\section}{\\Large\\bfseries\\sffamily}
         \\titleformat*{\\subsection}{\\large\\bfseries\\sffamily}
         \\titleformat*{\\subsubsection}{\\normalsize\\bfseries\\sffamily}")
(setq haba/org-latex-class-fancy-quoteblock
      "\\newcommand*\\openquote{\\makebox(25,7){\\scalebox{2}{<<}}}
         \\newcommand*\\closequote{\\makebox(25,7){\\scalebox{2}{>>}}}
         \\renewenvironment{quote}
         {\\list{}{\\rightmargin\\leftmargin}%
         \\item\\quotefont\\openquote\\relax\\ignorespaces}
         {\\unskip\\unskip\\closequote\\endlist}")
(setq haba/org-latex-class-no-chapters
      "% Главы без глав
         \\usepackage{titlesec}
         \\titleformat{\\chapter}{\\normalfont\\LARGE\\bfseries\\sffamily}{\\thechapter.}{1em}{}
         \\titlespacing*{\\chapter}{0pt}{3.5ex plus 1ex minus .2ex}{2.3ex plus .2ex} ")

(defun haba/org-latex-class-titling-logo-2 (logo-left vmove-left logo-right vmove-right)
  (format "\\usepackage{titling}
             \\pretitle{
             \\vspace{-35mm}\\hspace{-5mm}
             %s
             \\hspace{\\stretch{1}}
             %s
             \\vspace{35mm}\\vspace{\\stretch{1}}
             \\begin{center}\\LARGE\\bfseries\\sffamily}
             \\posttitle{\\par\\end{center}\\vspace{\\stretch{1.5}}}
             \\preauthor{\\begin{center}\\normalsize\\sffamily}
             \\postauthor{\\par\\end{center}}
             \\predate{\\begin{center}\\normalsize\\sffamily}
             \\date{}
             \\postdate{\\par\\end{center}}"
          (if (string= logo-left "")
              ""
            (format
             "\\includegraphics[scale=1%s]{%s/logo/%s}"
             (if (string= vmove-left "") "" (format ",trim=0 %s 0 0" vmove-left))
             org-directory logo-left))
          (if (string= logo-right "")
              ""
            (format
             "\\includegraphics[scale=1%s]{%s/logo/%s}"
             (if (string= vmove-right "") "" (format ",trim=0 %s 0 0" vmove-right))
             org-directory logo-right))))

(add-to-list 'org-latex-classes
             `("article"
               ,(concat "\\documentclass[a4paper,10pt]{article}"
                        (haba/org-latex-class-common "russian" "english")
                        haba/org-latex-class-hf-std
                        haba/org-latex-class-titling
                        haba/org-latex-class-dot-in-chapters
                        haba/org-latex-class-fancy-quoteblock)
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             `("article-en"
               ,(concat "\\documentclass[a4paper,10pt]{article}"
                        (haba/org-latex-class-common "english" "russian")
                        haba/org-latex-class-hf-std
                        haba/org-latex-class-titling
                        haba/org-latex-class-dot-in-chapters
                        haba/org-latex-class-fancy-quoteblock)
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
             `("article-adastra"
               ,(concat "\\documentclass[a4paper,10pt]{article}"
                        (haba/org-latex-class-common "russian" "english")
                        (haba/org-latex-class-hf-logo-2 "logo_adastra_50_opacity.png" "" "" "")
                        haba/org-latex-class-titling
                        haba/org-latex-class-dot-in-chapters
                        haba/org-latex-class-fancy-quoteblock)
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             `("article-adastra-en"
               ,(concat "\\documentclass[a4paper,10pt]{article}"
                        (haba/org-latex-class-common "english" "russian")
                        (haba/org-latex-class-hf-logo-2 "logo_adastra_50_opacity.png" "" "" "")
                        haba/org-latex-class-titling
                        haba/org-latex-class-dot-in-chapters
                        haba/org-latex-class-fancy-quoteblock)
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
             `("article-sberbank"
               ,(concat "\\documentclass[a4paper,10pt]{article}"
                        (haba/org-latex-class-common "russian" "english")
                        (haba/org-latex-class-hf-logo-2 "logo_sberbank_simple_50_opacity.png" "" "logo_adastra_50_opacity.png" "-3mm")
                        haba/org-latex-class-titling
                        haba/org-latex-class-dot-in-chapters
                        haba/org-latex-class-fancy-quoteblock)
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             `("report"
               ,(concat "\\documentclass[a4paper,10pt]{report}"
                        (haba/org-latex-class-common "russian" "english")
                        haba/org-latex-class-hf-std
                        haba/org-latex-class-titling
                        haba/org-latex-class-dot-in-chapters
                        haba/org-latex-class-fancy-quoteblock
                        haba/org-latex-class-no-chapters)

               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
             `("report-sberbank-adastra"
               ,(concat "\\documentclass[a4paper,10pt]{report}"
                        (haba/org-latex-class-common "russian" "english")
                        (haba/org-latex-class-hf-logo-2 "logo_sberbank_simple_50_opacity.png" "" "logo_adastra_50_opacity.png" "-3mm")
                        (haba/org-latex-class-titling-logo-2 "logo_sberbank_simple_50_opacity.png" "" "logo_adastra_50_opacity.png" "-3mm")
                        haba/org-latex-class-dot-in-chapters
                        haba/org-latex-class-fancy-quoteblock
                        haba/org-latex-class-no-chapters)

               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             `("report-sberbank"
               ,(concat "\\documentclass[a4paper,10pt]{report}"
                        (haba/org-latex-class-common "russian" "english")
                        (haba/org-latex-class-hf-logo-2 "logo_sberbank_simple_50_opacity.png" "" "" "")
                        (haba/org-latex-class-titling-logo-2 "logo_sberbank_simple_50_opacity.png" "" "" "")
                        haba/org-latex-class-dot-in-chapters
                        haba/org-latex-class-fancy-quoteblock
                        haba/org-latex-class-no-chapters)

               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             `("report-adastra-ru"
               ,(concat "\\documentclass[a4paper,10pt]{report}"
                        (haba/org-latex-class-common "russian" "english")
                        (haba/org-latex-class-hf-logo-2 "logo_adastra_50_opacity.png" "" "" "")
                        (haba/org-latex-class-titling-logo-2 "logo_adastra_50_opacity.png" "" "" "")
                        haba/org-latex-class-dot-in-chapters
                        haba/org-latex-class-fancy-quoteblock
                        haba/org-latex-class-no-chapters)

               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; (add-to-list 'org-latex-classes
             ;; `("beamer"
               ;; ,(concat "\\documentclass[a4paper,10pt]{report}"
                        ;; (haba/org-latex-class-common "russian" "english")
                        ;; "\\usepackage[top=30mm, left=30mm, right=25mm, bottom=35mm]{geometry}"
                        ;; haba/org-latex-class-no-chapters
                ;; "\\mode<{beamermode}>
                 ;; \\usetheme{beamertheme}
                 ;; \\usecolortheme{{{{beamercolortheme}}}}
                 ;; \\beamertemplateballitem
                 ;; \\setbeameroption{show note
                 ;; \\institute{{{{beamerinstitute}}}}
                 ;; \\subject{{{{beamersubject}}}}")

             ;; ("\\section{%s}" . "\\section*{%s}")

             ;; ("\\begin{frame}[fragile]\\frametitle{%s}"
               ;; "\\end{frame}"
               ;; "\\begin{frame}[fragile]\\frametitle{%s}"
               ;; "\\end{frame}")))
