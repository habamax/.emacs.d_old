;;; init.el -- Emacs initialization file
;;; Maxim Kim <habamax@gmail.com>

;;; Commentary:
;; Carefully crafted settings for alien's editor I become used to.

;;; Code:

;; Non-Package setup

;; No new frames for files that are opened from OSX
(when (eq system-type 'darwin)
  (setq ns-pop-up-frames nil))

(when window-system
  (setq default-frame-alist '((fullscreen . maximized)))
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1))

(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message "")

(setq user-full-name "Maxim Kim"
      user-mail-address "habamax@gmail.com")


;; RU stuff
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq default-input-method 'russian-computer)

;; scroll to the top or bottom with C-v and M-v
(setq scroll-error-top-bottom t)

;; M-a and M-e use punct and single space as sentence delimiter
(setq sentence-end-double-space nil)

(delete-selection-mode 1)
(setq ring-bell-function #'ignore)
(setq disabled-command-function nil)
(setq suggest-key-bindings t)

(electric-indent-mode t)
(show-paren-mode t)
(column-number-mode t)
(recentf-mode 1)


;; winner mode is a must
(winner-mode 1)


;; tabs are evil but...
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Backups & Autosave
;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;; autosave position in file
(setq save-place-file (concat user-emacs-directory "saveplace"))
(setq-default save-place t)
(require 'saveplace)

;; C-u C-SPC C-SPC to pop mark twice...
(setq set-mark-command-repeat-pop t)

;; Tab to indent or complete
;; (setq tab-always-indent 'complete)



;; Keep 'Customize' stuff separated
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)


;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)



;; Set up packaging system
(let ((package-protocol (if (eq system-type 'windows-nt) "http://" "https://")))
  (setq package-archives `(("elpa" .  ,(concat package-protocol "elpa.gnu.org/packages/"))
                           ("melpa" . ,(concat package-protocol "melpa.org/packages/")))))

(package-initialize)
                                   
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)


;; Local packages

(use-package haba-stuff
  :ensure nil
  :demand
  :commands (haba/next-buffer haba/previous-buffer haba/toggle-window-split)
  :load-path "lisp/"
  :bind (("M-;" . haba/toggle-comment)
	 ("C-a" . haba/move-beginning-of-line)
	 ("M-j" . haba/join-line)
         ("s-d" . haba/duplicate-line)
	 ("C-c o i" . haba/open-init-file))
  :config

  (let ((font-size (if (string-equal (system-name) "MKIM") 110 130)))
    (haba/set-font '("Iosevka" "Menlo" "Roboto Mono" "Dejavu Sans Mono" "Consolas")
                   font-size))
  
  )


(use-package haba-appearance
  :ensure nil
  :demand
  :load-path "lisp/"
  :bind (("s-t" . haba/toggle-theme)
         ("C-c t" . haba/toggle-theme))
  :config
  (use-package zenburn-theme :defer)
  (use-package eclipse-theme :defer)
  )




;; Melpa packages

;; PATH for OSX
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  :if (eq system-type 'darwin))

(use-package which-key
  :defer 3
  :diminish which-key-mode
  :config (which-key-mode))

(use-package expand-region
  :bind (("M-m" . er/expand-region))
  :config
  (progn
    (setq expand-region-contract-fast-key "M")))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package smex :defer)

(use-package ivy
  :diminish ivy-mode
  :init
  ;; clear default ^ for counsel-M-x and friends
  (setq ivy-initial-inputs-alist '())
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
	;; '((t . ivy--regex-fuzzy)))
	'((t . ivy--regex-ignore-order))))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c k" . counsel-ag)
	 ("C-c g" . counsel-git)
         ("C-s" . counsel-grep-or-swiper)
	 ("C-x b" . ivy-switch-buffer)
         ("s-b" . ivy-switch-buffer))
  :config
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
	(concat
	 ;; file names beginning with # or .
	 "\\(?:\\`[#.]\\)"
	 ;; file names ending with # or ~
	 "\\|\\(?:\\`.+?[#~]\\'\\)")))

(use-package hydra
  :bind ("C-c w" . hydra-windows/body)
  :bind ("C-c t" . hydra-toggle-theme/body)
  :bind ("C-x o" . hydra-other-window/body)

  :config

  (defhydra hydra-windows ()
    "Windows"
    ("w" winner-undo "Winner undo")
    ("W" winner-redo "Winner redo")
    ("t" haba/toggle-window-split "Toggle split")
    ("]" enlarge-window-horizontally "Enlarge horizontal")
    ("[" shrink-window-horizontally "Shrink horizontal")
    ("=" enlarge-window "Enlarge vertival")
    ("-" shrink-window "Shrink vertical")
    ("b" balance-windows "Balance windows")
    ("m" delete-other-windows "Maximize window")
    ("c" delete-window "Close window")
    ("SPC" nil "quit")
    ("q" nil "quit"))

  (defhydra hydra-toggle-theme
    (:body-pre (haba/toggle-theme))
    "Themes"
    ("t" (haba/toggle-theme) "Toggle next theme")
    ("SPC" nil "quit")
    ("q" nil "quit"))

  (defhydra hydra-other-window
    (:body-pre (other-window 1))
    "Other window"
    ("o" (other-window 1) "Next")
    ("O" (other-window -1) "Previous")
    ("SPC" nil "quit")
    ("q" nil "quit"))
  )

;; Complete Anything
(use-package company
  :defer 1
  :diminish company-mode
  :bind ("s-/" . company-complete)
  :config
  (progn
    (use-package company-flx :config (company-flx-mode +1))
    
    (add-to-list 'company-backends 'company-omnisharp)
    (setq company-minimum-prefix-length 2)
    
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    
    (global-company-mode)))



(use-package multiple-cursors
  :bind* (("M-n" . mc/mark-next-like-this)
	  ("M-p" . mc/mark-previous-like-this)
	  ("M-N" . mc/unmark-next-like-this)
	  ("M-P" . mc/unmark-previous-like-this)))




(use-package rainbow-delimiters
  :defer
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package smartparens
  :diminish smartparens-mode
  :defer 1
  :config
  (progn
    (require 'smartparens-config)
    (setq sp-base-key-bindings 'sp)
    (sp-use-smartparens-bindings)
    (smartparens-global-mode)
    (show-smartparens-global-mode)))

(use-package magit
  :commands (magit-status)
  :bind ("C-c m" . magit-status)
  :config
  (setq magit-log-arguments '("--graph" "--show-signature")
	magit-push-always-verify nil
	magit-popup-use-prefix-argument 'default
	magit-revert-buffers t))


(use-package emmet-mode
  :defer
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'html-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))
  

(use-package markdown-mode
  :mode ("\\.\\(markdown|md\\)$" . markdown-mode))

(use-package csharp-mode
  :mode ("\\.\\(cs|sln\\)$" . csharp-mode)
  :config
  (progn
    (use-package omnisharp
      :diminish omnisharp-mode
      :config
      (setq omnisharp-company-match-type 'company-match-flx))
    (add-hook 'csharp-mode-hook 'omnisharp-mode)))

(use-package go-mode
  :mode ("\\.\\(go\\)$" . go-mode)
  )


;; flycheck
(use-package flycheck
  :diminish 'flycheck-mode
  :config (global-flycheck-mode))

;; flyspell - use aspell instead of ispell
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config (setq ispell-program-name (executable-find "aspell")
		ispell-extra-args '("--sug-mode=ultra")))


;; music FTW
(use-package emms
  :bind (("C-c p m" . haba/emms-play-main)
         ("C-c p c" . emms-playlist-mode-go)
         ("C-c p p" . emms-pause)
         ("C-c p n" . emms-next)
         ("C-c p r" . emms-random)
         ("C-c p s" . emms-stop))
  :init
  ;; Well on OSX I get weird tramp error...
  ;; (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

  :config
  (setq emms-mode-line-icon-color "yellow")

  (emms-all)

  (require 'emms-history)
  (emms-history-load)
  
  (setq emms-repeat-playlist t)

  ;; OSX has simple afplay utility to play music
  (when (eq system-type 'darwin)
    (define-emms-simple-player afplay '(file)
      (regexp-opt '(".mp3" ".m4a" ".aac")) "afplay")
    (setq emms-player-list `(,emms-player-afplay))
    (add-hook 'kill-emacs-hook 'emms-stop))

  ;; Not sure if this is needed
  (setq emms-source-file-default-directory "~/Music")

  (defun haba/emms-play-main ()
    (interactive)
    (emms-play-directory "~/Music/smusic/main"))
  )


;; (use-package evil)

(use-package nyan-mode
  :defer 3
  :config
  (nyan-mode))


;; Built-in packages

(use-package erc
  :defer
  :ensure nil
  :init
  (setq ;erc-fill-column (- (window-width) 2)
	erc-nick '("habamax" "mxmkm")
	erc-track-minor-mode t
	erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
  (defun erc-freenode ()
    (interactive)
    (erc :server "irc.freenode.net" :port 6667 :nick "habamax"))
  
  :config
  ;; (make-variable-buffer-local 'erc-fill-column)
  ;; (add-hook 'window-configuration-change-hook
	    ;; '(lambda ()
	       ;; (save-excursion
		 ;; (walk-windows
		  ;; (lambda (w)
		    ;; (let ((buffer (window-buffer w)))
		      ;; (set-buffer buffer)
		      ;; (when (eq major-mode 'erc-mode)
			;; (setq erc-fill-column (- (window-width w) 2)))))))))
  )


(use-package calendar
  :ensure nil
  :init
  ;; Calendar -- говорим и показываем по русски.
  (setq calendar-date-style 'iso
	calendar-week-start-day 1
	calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
	calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель"
				   "Май" "Июнь" "Июль" "Август"
				   "Сентябрь" "Октябрь" "Ноябрь" "Декабрь"]))


(use-package org
  :mode ("\\.\\(org|txt\\)$" . org-mode)
  :bind (("C-c o a" . org-agenda)
	 ("C-c o l" . org-store-link)
	 ("C-c o c" . org-capture))
  :config
    
  (setq org-src-fontify-natively t
	org-fontify-whole-heading-line t
	org-return-follows-link t
	org-special-ctrl-a/e t
	org-special-ctrl-k t)

  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/refile.org")

  (setq org-refile-targets '((nil :maxlevel . 3)
			     (org-agenda-files :maxlevel . 3)))
  ;; Refile in a single go
  (setq org-outline-path-complete-in-steps nil)
  ;; Show full paths for refiling
  (setq org-refile-use-outline-path 'file)

  ;; Doesn't work
  (setq org-image-actual-width 500)
  
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "|" "DONE(d)")
		(sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

  (setq org-tag-alist '(("work" . ?w) ("tax" . ?t)("adastra" . ?a) ("tax" . ?t)))
  ;; C-u C-c C-c to realign all tags
  (setq org-tags-column 50)
  ;; Place tags close to the right-hand side of the window
  (add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
  (defun place-agenda-tags ()
    "Put the agenda tags by the right border of the agenda window."
    (setq org-agenda-tags-column (- 4 (window-width)))
    (org-agenda-align-tags))

  (setq org-capture-templates
	(quote (("t" "Todo" entry (file org-default-notes-file)
		 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		("n" "Note" entry (file org-default-notes-file)
		 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		("j" "Journal" entry (file+datetree "~/org/diary.org")
		 "* %?\n%U\n" :clock-in t :clock-resume t)
		("w" "Org-protocol" entry (file org-default-notes-file)
		 "* TODO Review %c\n%U\n" :immediate-finish t))))


  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (sh . t)
     (plantuml . t)))


  ;; this has to be set up for different machines
  (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/8037/plantuml.8037.jar")


  ;; Pandoc
  (use-package ox-pandoc)

  ;; Latex
  (require 'ox-latex)
  
  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[a4paper,12pt]{scrartcl}
                 \\usepackage{cmap}
                 \\usepackage[utf8]{inputenc}
                 \\usepackage[english,russian]{babel}
                 \\usepackage[top=25mm, left=20mm, right=20mm, bottom=25mm]{geometry}
                 \\usepackage{indentfirst}   % русский стиль: отступ первого абзаца раздела
                 \\usepackage{misccorr}      % точка в номерах заголовков
                 \\usepackage[onehalfspacing]{setspace}
                 \\usepackage[T2A]{fontenc}
                 \\usepackage[scaled]{beraserif}
                 \\usepackage[scaled]{berasans}
                 \\usepackage[scaled]{beramono}

                 \\usepackage{graphicx}
                 \\usepackage{grffile}
                 \\usepackage{longtable}
                 \\usepackage{wrapfig}
                 \\usepackage{rotating}
                 \\usepackage[normalem]{ulem}
                 \\usepackage{amsmath}
                 \\usepackage{textcomp}
                 \\usepackage{amssymb}
                 \\usepackage{capt-of}
                 \\usepackage{hyperref}
                 [NO-DEFAULT-PACKAGES]
                 [NO-PACKAGES]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("report"
                 "\\documentclass[a4paper,12pt]{report}
                 \\usepackage{cmap}
                 \\usepackage[utf8]{inputenc}
                 \\usepackage[english,russian]{babel}
                 \\usepackage[top=30mm, left=30mm, right=20mm, bottom=20mm]{geometry}
                 \\usepackage{indentfirst}   % русский стиль: отступ первого абзаца раздела
                 \\usepackage{misccorr}      % точка в номерах заголовков
                 \\usepackage[onehalfspacing]{setspace}
                 \\usepackage[T2A]{fontenc}
                 \\usepackage[scaled]{beraserif}
                 \\usepackage[scaled]{berasans}
                 \\usepackage[scaled]{beramono}

                 \\usepackage{fancyhdr}
                 \\pagestyle{fancy}
                 \\fancypagestyle{plain}{\\pagestyle{fancy}}
                 \\lhead{Максим Ким}
                 \\chead{}
                 \\rhead{\\today}
                 \\lfoot{}
                 \\cfoot{}
                 \\rfoot{\\thepage}
                 \\renewcommand{\\headrulewidth}{0.4pt}
                 \\renewcommand{\\footrulewidth}{0.4pt}

                 % Главы без глав
                 \\usepackage{titlesec}
                 \\titleformat{\\chapter}
                   {\\normalfont\\LARGE\\bfseries}{\\thechapter.}{1em}{}
                 \\titlespacing*{\\chapter}{0pt}{3.5ex plus 1ex minus .2ex}{2.3ex plus .2ex}

                 \\usepackage{graphicx}
                 \\usepackage{grffile}
                 \\usepackage{longtable}
                 \\usepackage{wrapfig}
                 \\usepackage{rotating}
                 \\usepackage[normalem]{ulem}
                 \\usepackage{amsmath}
                 \\usepackage{textcomp}
                 \\usepackage{amssymb}
                 \\usepackage{capt-of}
                 \\usepackage{hyperref}
                 [NO-DEFAULT-PACKAGES]
                 [NO-PACKAGES]"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  

  (add-to-list 'org-latex-classes
               '("report-sberbank"
                 "\\documentclass[a4paper,12pt]{report}
                 \\usepackage{cmap}
                 \\usepackage[utf8]{inputenc}
                 \\usepackage[english,russian]{babel}
                 \\usepackage[top=30mm, left=30mm, right=20mm, bottom=20mm]{geometry}
                 \\usepackage{indentfirst}   % русский стиль: отступ первого абзаца раздела
                 \\usepackage{misccorr}      % точка в номерах заголовков
                 \\usepackage[onehalfspacing]{setspace}
                 \\usepackage[T2A]{fontenc}
                 \\usepackage[scaled]{beraserif}
                 \\usepackage[scaled]{berasans}
                 \\usepackage[scaled]{beramono}

                 \\usepackage{fancyhdr}
                 \\pagestyle{fancy}
                 \\fancypagestyle{plain}{\\pagestyle{fancy}}
                 \\lhead{\\raisebox{-1\\height}{\\includegraphics[scale=1]{logo/logo_sberbank.png}}}
                 \\chead{}
                 \\rhead{\\raisebox{-1\\height}{\\includegraphics[scale=1]{logo/logo_adastra.png}}}
                 \\lfoot{}
                 \\cfoot{}
                 \\rfoot{\\thepage}
                 \\renewcommand{\\headrulewidth}{0.0pt}
                 \\renewcommand{\\footrulewidth}{0.0pt}

                 % Главы без глав
                 \\usepackage{titlesec}
                 \\titleformat{\\chapter}
                   {\\normalfont\\LARGE\\bfseries}{\\thechapter.}{1em}{}
                 \\titlespacing*{\\chapter}{0pt}{3.5ex plus 1ex minus .2ex}{2.3ex plus .2ex}

                 \\usepackage{graphicx}
                 \\usepackage{grffile}
                 \\usepackage{longtable}
                 \\usepackage{wrapfig}
                 \\usepackage{rotating}
                 \\usepackage[normalem]{ulem}
                 \\usepackage{amsmath}
                 \\usepackage{textcomp}
                 \\usepackage{amssymb}
                 \\usepackage{capt-of}
                 \\usepackage{hyperref}
                 [NO-DEFAULT-PACKAGES]
                 [NO-PACKAGES]"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )





;;; init.el ends here
