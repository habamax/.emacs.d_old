;;; init.el -- Personal init file for Emacs
;;; Maxim Kim <habamax@gmail.com>

;;; Commentary:
;; Things to do:
;; - TABs or Spaces for CSharp?
;; - Yasnippets
;; - Multiple cursors (mark-multiple)
;; - Smartparens hydra with C-9
;; - Projectile setup

;;; Code:

;; Non-Package setup
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  (setq default-frame-alist '((fullscreen . maximized))))

(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message "")

(setq user-full-name "Maxim Kim"
      user-mail-address "habamax@gmail.com")

;
;; RU stuff
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq default-input-method 'russian-computer)

;; scroll to the top or bottom with C-v and M-v
(setq scroll-error-top-bottom t)

;; M-a and M-e use punct and single space as sentence delimiter
(setq sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(delete-selection-mode 1)
(setq ring-bell-function #'ignore)
(setq disabled-command-function nil)
(setq suggest-key-bindings t)

(electric-indent-mode t)
(show-paren-mode t)
(column-number-mode t)
(recentf-mode 1)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Backups
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))

(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t)


;; Keep 'Customize' stuff separated
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Convenience bindings
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)


;; Set up packaging system
(setq package-enable-at-startup nil)
(setq package-archives '(("elpa" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
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
         ("C-c f i" . haba/open-init-file))
  :config
  (haba/set-font '("Source Code Pro" "Roboto Mono" "Menlo" "Dejavu Sans Mono") 140))



;; Themes
(use-package gruvbox-theme
  :disabled t
  :config
  (load-theme 'gruvbox t))

;; use-package doen't work for base16 themes
;; (use-package base16-theme
  ;; :config
  ;; (load-theme 'base16-tomorrow-dark t))

(ignore-errors
  (load-theme 'base16-eighties-dark t)
;;  (load-theme 'base16-tomorrow-dark t)
  )



;; Melpa packages

;; PATH for OSX
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  :if (eq system-type 'darwin))

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config (which-key-mode))

(use-package expand-region
  :bind ("M-m" . er/expand-region)
  :config
  (progn
    (setq expand-region-contract-fast-key "M")))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package helm
  :diminish helm-mode
  :defer 2
  :bind* (("M-x"     . helm-M-x)
          ("C-c M-x" . execute-extended-command)
          ("C-c h"   . helm-command-prefix)
          ("M-s o"   . helm-occur)
          ("C-x C-b" . helm-buffers-list)
          ("C-x b"   . helm-mini)
          ("C-x C-f" . helm-find-files))
  :config
  (progn
    (use-package helm-fuzzier :config (helm-fuzzier-mode 1))
    (use-package helm-flx :config (helm-flx-mode +1))
    (require 'helm-config)
    (bind-key "C-c !" 'helm-toggle-suspend-update helm-map)
    (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
    (bind-key "C-i" 'helm-execute-persistent-action helm-map)
    (bind-key "C-z" 'helm-select-action helm-map)
    (setq helm-M-x-fuzzy-match t
          helm-ff-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-move-to-line-cycle-in-source t
          helm-ff-file-name-history-use-recentf t
          helm-ff-auto-update-initial-value nil
          helm-split-window-in-side-p t
          helm-tramp-verbose 9)
    (setq helm-ag-base-command "pt -e --nogroup --nocolor")
    (helm-mode)
    (add-hook 'helm-minibuffer-set-up-hook 'deactivate-input-method)
    (helm-autoresize-mode t)))

;; Complete Anything
(use-package company
  :defer 1
  :config
  (progn
    (use-package company-flx :config (company-flx-mode +1))
    
    (add-to-list 'company-backends 'company-omnisharp)
    (setq company-minimum-prefix-length 2)

    ;; (define-key company-active-map (kbd "<tab>") 'company-select-next)
    ;; (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
    ;; (define-key company-active-map (kbd "M-p") nil)
    ;; (define-key company-active-map (kbd "M-n") nil)
    ;; (define-key company-active-map (kbd "TAB") 'company-yasnippet-or-completion)

    
    (global-company-mode)))




(use-package hydra
  :bind ("C-c n" . hydra-cycle-next/body)
  :bind ("C-x o" . hydra-cycle-windows/body)
  :config
  (defhydra hydra-page-break (global-map "C-x")
    "Page breaks"
    ("[" backward-page "Back")
    ("]" forward-page "Forward")
    ("RET" nil "quit")
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
    ("RET" nil "quit")
    ("q" nil "quit"))
  
  (defhydra hydra-cycle-next ()
    "Frames, Buffers"
    ("f" other-frame "Next frame")
    ("F" (other-frame -1) "Previous frame")
    ("b" haba/next-buffer "Next buffer")
    ("B" haba/previous-buffer "Previous buffer")
    ("k" kill-this-buffer "Kill buffer")
    ("RET" nil "quit")
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
  :commands (magit-status projectile-vc)
  :config
  (setq magit-log-arguments '("--graph" "--show-signature")
        magit-push-always-verify nil
        magit-popup-use-prefix-argument 'default
        magit-revert-buffers t))

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-mode-map)
  :init
      (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile
          projectile-enable-caching t
          projectile-file-exists-remote-cache-expire (* 10 60))
  :config
  (progn
    (use-package helm-projectile
      :config (helm-projectile-on))
    (projectile-global-mode)))


(use-package markdown-mode
  :mode ("\\.\\(markdown|md\\)$" . markdown-mode))

(use-package csharp-mode
  :mode ("\\.\\(cs|sln\\)$" . csharp-mode)
  :config
  (progn
    (use-package omnisharp)
    (add-hook 'csharp-mode-hook 'omnisharp-mode)))

;; (use-package geiser
;;   :ensure nil
;;   :init
;;   (setq geiser-active-implementations '(racket)))



;; flycheck
(use-package flycheck
  :config (global-flycheck-mode))

(use-package helm-flycheck
  :bind ("C-c ! h" . helm-flycheck)
  :config (global-flycheck-mode))

;; ;; flyspell - use aspell instead of ispell
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
  :bind (("C-c o a" . org-agenda)
         ("C-c o l" . org-store-link)
         ("C-c o c" . org-capture))
  :init
  (setq org-src-fontify-natively t
        org-fontify-whole-heading-line t
        org-return-follows-link t
        org-special-ctrl-a/e t
        org-special-ctrl-k t))
