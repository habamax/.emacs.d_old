;;; init.el -- Personal init file for Emacs
;;; Maxim Kim <habamax@gmail.com>

;;; Commentary:
;; No comments.

;;; Code:


;; TODO: This should be tweaked to select my projects
;; (progn
;;   (cd (ivy-read "Select project:" '("~/work/unity/Dominion Master/" "~/.emacs.d" "~/org")))
;;   (ffip))
;; (progn
;;   (setq ffip-project-root (ivy-read "Select project:" '("~/Projects/Test ground" "~/.emacs.d" "~/org")))
;;   (ffip))


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
(winner-mode 1)

;; tabs are evil but...
(setq-default indent-tabs-mode t)
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

;; Set up packaging system
(setq package-enable-at-startup nil)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
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
	 ("C-c i" . haba/open-init-file))
  :config
  (haba/set-font '("Source Code Pro" "Roboto Mono" "Menlo" "Dejavu Sans Mono") 140))



;; Themes
;; Nighttime
;; (use-package base16-theme :init (load-theme 'base16-eighties-dark t))
;; (use-package base16-theme :init (load-theme 'base16-default-dark t))
;; Daytime
(use-package leuven-theme :init (load-theme 'leuven t))
;; (use-package gandalf-theme :init (load-theme 'gandalf t))





;; Melpa packages

;; PATH for OSX
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  :if (eq system-type 'darwin))

(use-package which-key
  :defer 3
  :diminish which-key-mode
  :config (which-key-mode))

(use-package expand-region
  :bind (("M-m" . er/expand-region))
  ;; :bind (:map html-mode-map
  ;; 	      ("M-h" . er/mark-inner-tag)
  ;; 	      ("M-H" . er/mark-outer-tag))
  :config
  (progn
    (setq expand-region-contract-fast-key "M")))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package swiper
  :demand
  :diminish ivy-mode
  :bind (("C-s" . swiper)
	 ("C-c C-r" . ivy-recentf)
	 ("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
	;; '((t . ivy--regex-fuzzy)))
	'((t . ivy--regex-ignore-order)))
  )

(use-package smex :defer)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v" . counsel-describe-variable)
	 ("C-c k" . counsel-ag)
	 ("C-c g" . counsel-git))
  :init
  ;; clear default ^ for counsel-M-x and friends
  (setq ivy-initial-inputs-alist '())
  
  :config
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
	(concat
	 ;; file names beginning with # or .
	 "\\(?:\\`[#.]\\)"
	 ;; file names ending with # or ~
	 "\\|\\(?:\\`.+?[#~]\\'\\)")))



;; Complete Anything
(use-package company
  :defer 1
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


(use-package golden-ratio                 ; Auto resize windows
  :diminish golden-ratio-mode
  :init
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale nil))
  
(use-package hydra
  :bind ("C-c n" . hydra-cycle-next/body)
  :bind ("C-x o" . hydra-cycle-windows/body)
  :bind ("C-c l" . hydra-line/body)
  :bind ("s-l" . hydra-line/body)

  :config
  (defhydra hydra-line ()
    "Line operations"
    ("d" haba/duplicate-line "Duplicate line")
    ("n" haba/move-line-down "Move down")
    ("p" haba/move-line-up "Move up")
    ("SPC" nil "quit")
    ("q" nil "quit"))

  (defhydra hydra-cycle-windows
    (:body-pre (other-window 1))
    "Windows"
    ("o" (other-window 1) "Next")
    ("O" (other-window -1) "Previous")
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
  
  (defhydra hydra-cycle-next ()
    "Frames, Buffers"
    ("f" other-frame "Next frame")
    ("F" (other-frame -1) "Previous frame")
    ("b" haba/next-buffer "Next buffer")
    ("B" haba/previous-buffer "Previous buffer")
    ("k" kill-this-buffer "Kill buffer")
    ("SPC" nil "quit")
    ("q" nil "quit")))

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
      :config
      (setq omnisharp-company-match-type 'company-match-flx))
    (add-hook 'csharp-mode-hook 'omnisharp-mode)))

(use-package geiser
  ;; :ensure nil
  :init
  (setq geiser-active-implementations '(racket)))



;; flycheck
(use-package flycheck
  :config (global-flycheck-mode))

;; flyspell - use aspell instead of ispell
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config (setq ispell-program-name (executable-find "aspell")
		ispell-extra-args '("--sug-mode=ultra")))






;; Built-in packages

(use-package erc
  :defer
  :ensure nil
  :init
  (setq erc-fill-column (- (window-width) 2)
	erc-nick '("habamax" "mxmkm")
	erc-track-minor-mode t
	erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#racket")))
  (defun erc-freenode ()
    (interactive)
    (erc :server "irc.freenode.net" :port 6667 :nick "habamax"))
  
  :config
  (make-variable-buffer-local 'erc-fill-column)
  (add-hook 'window-configuration-change-hook 
	    '(lambda ()
	       (save-excursion
		 (walk-windows
		  (lambda (w)
		    (let ((buffer (window-buffer w)))
		      (set-buffer buffer)
		      (when (eq major-mode 'erc-mode)
			(setq erc-fill-column (- (window-width w) 2)))))))))
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
  :ensure nil
  :mode ("\\.\\(org|txt\\)$" . org-mode)
  :bind (("C-c o a" . org-agenda)
	 ("C-c o l" . org-store-link)
	 ("C-c o c" . org-capture))
  :config
  (use-package org-bullets
    :init
    (setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "●" "○"))
    ;; (setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇"))
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
    
  (setq org-src-fontify-natively t
	org-fontify-whole-heading-line t
	org-return-follows-link t
	org-special-ctrl-a/e t
	org-special-ctrl-k t)

  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/refile.org")
  ;; (setq org-agenda-files '("~/org"))

  (setq org-refile-targets '((nil :maxlevel . 9)
			     (org-agenda-files :maxlevel . 9)))
  ;; Refile in a single go
  (setq org-outline-path-complete-in-steps nil)
  ;; Show full paths for refiling
  (setq org-refile-use-outline-path t)

  
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "|" "DONE(d)")
		(sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  ;; (setq org-todo-state-tags-triggers
	;; (quote (("CANCELLED" ("CANCELLED" . t))
		;; ("WAITING" ("WAITING" . t))
		;; ("HOLD" ("WAITING" . t) ("HOLD" . t))
		;; (done ("WAITING") ("HOLD"))
		;; ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
		;; ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

  (setq org-capture-templates
	(quote (("t" "Todo" entry (file org-default-notes-file)
		 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		("n" "Note" entry (file org-default-notes-file)
		 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		("j" "Journal" entry (file+datetree "~/org/diary.org")
		 "* %?\n%U\n" :clock-in t :clock-resume t)
		("w" "Org-protocol" entry (file org-default-notes-file)
		 "* TODO Review %c\n%U\n" :immediate-finish t))))
  )




;;; init.el ends here
