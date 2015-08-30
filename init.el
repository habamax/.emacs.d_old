;; Init file for Emacs
;; Maxim Kim <habamax@gmail.com>



;;;; Set up packaging system
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)



;;;; Local packages
(use-package haba-osx
  :ensure nil
  :load-path "lisp/"
  :if (eq system-type 'darwin))

(use-package haba-comment
  :ensure nil
  :load-path "lisp/"
  :bind ("M-;" . haba-comment-dwim))

(use-package haba-misc
  :ensure nil
  :load-path "lisp/"
  :bind (("C-c W W" . haba-toggle-window-split)
         ("C-c f i" . find-user-init-file)))


;;;; Melpa packages


;; PATH for OSX
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  :if (eq system-type 'darwin))


(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package helm
  :diminish helm-mode
  :bind* (("M-x" . helm-M-x)
          ("C-c M-x" . execute-extended-command)
          ("M-]" . helm-command-prefix)
          ("M-y" . helm-show-kill-ring)
          ("C-x C-b" . helm-buffers-list)
          ("C-x b" . helm-mini)
          ("C-x C-f" . helm-find-files))
  :config
  (progn
    (require 'helm-config)
    (bind-key "C-c !" 'helm-toggle-suspend-update helm-map)
    (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
    (bind-key "C-i" 'helm-execute-persistent-action helm-map)
    (bind-key "C-z" 'helm-select-action helm-map)
    (setq helm-M-x-fuzzy-match t
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
    (helm-mode)
    (helm-autoresize-mode t)))

 ;; company "complete anything"
(use-package company
  :defer 2
  :config
  (progn
    (use-package company-c-headers)
    (push '(company-clang
            :with company-semantic
            :with company-yasnippet
            :with company-c-headers)
          company-backends)
    (setq company-minimum-prefix-length 2)
    (global-company-mode)))
  

(use-package hydra
  :bind ("C-." . hydra-cycle-windows/body)
  :config
  (defhydra hydra-cycle-windows ()
    "Windows, Buffers, Frames"
    ("o" other-window "Next window")
    ("O" (other-window -1) "Previous window")
    ("f" other-frame "Next frame")
    ("F" (other-frame -1) "Previous frame")
    ("b" next-buffer "Next buffer")
    ("B" previous-buffer "Previous buffer")
    ("k" kill-this-buffer "Kill buffer")
    ("m" delete-other-windows "Maximize window")
    ("q" nil "quit")))

(use-package golden-ratio
  :diminish golden-ratio-mode
  :config (golden-ratio-mode 1))

(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'text-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode)
    (show-smartparens-global-mode)))


(use-package leuven-theme
  :disabled t
  :config
  (load-theme 'leuven t))

(use-package cyberpunk-theme
  :disabled t
  :config
  (load-theme 'cyberpunk t))

(use-package solarized-theme
  :ensure nil
  ;; :disabled t
  :config
  (setq solarized-high-contrast-mode-line t
        solarized-use-more-italic nil
        solarized-use-less-bold nil)
  (load-theme 'solarized-dark t))


;; magit
(use-package magit
  :commands (magit-status projectile-vc)
  :config
  (setq magit-log-arguments '("--graph" "--show-signature")
        magit-push-always-verify nil
        magit-popup-use-prefix-argument 'default
        magit-revert-buffers t))



;; projectile
(use-package projectile
  :diminish projectile-mode
  :bind* ("M-[" . projectile-command-map)
  :demand
  :config
  (progn
    (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile
          projectile-enable-caching t
          projectile-file-exists-remote-cache-expire (* 10 60))
    (use-package helm-projectile
      :commands helm-projectile
      :config (helm-projectile-on))
    (projectile-global-mode)))

(use-package markdown-mode
  :mode ("\\.\\(markdown|md\\)$" . markdown-mode))



;; Doesn't work as there is no let-alist blablabla.
;; I should have find some time to resolve it.
;;
;; flycheck
;; (use-package flycheck
;; :init (global-flycheck-mode))
;;
;; (use-package helm-flycheck
;;   :bind ("C-c ! h" . helm-flycheck)
;;   :config (global-flycheck-mode))

;; ;; flyspell - use aspell instead of ispell
;; (use-package flyspell
;;   :commands (flyspell-mode flyspell-prog-mode)
;;   :config (setq ispell-program-name (executable-find "aspell")
;;                 ispell-extra-args '("--sug-mode=ultra")))





;;;; Built-in packages

;; (use-package erc
;;   :ensure nil
;;   :init
;;   (setq erc-nick "habamax"
;;         erc-user-full-name "Maxim Kim"
;;         erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#lor")))

;;   (add-hook 'erc-mode-hook
;;             (lambda ()
;;               (erc-track-minor-mode 1))))

(use-package rcirc
  :ensure nil
  :init
  (setq rcirc-default-nick "habamax"
        rcirc-default-user-name "mxmkm"
        rcirc-default-full-name "Maxim Kim"
        rcirc-fill-column 'frame-width
        rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE")
        rcirc-time-format "[%Y-%m-%d %H:%M] "
        rcirc-server-alist '(("irc.freenode.net" :channels ("#emacs" "#lor"))))
  (add-hook 'rcirc-mode-hook
            (lambda ()
              (rcirc-track-minor-mode 1)
              (set (make-local-variable 'scroll-conservatively)
                 8192)))
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Non-Package setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Maxim Kim"
      user-mail-address "habamax@gmail.com")

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1))

;; RU stuff
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq default-input-method 'russian-computer)

;; auto indent on Enter (should be default since Emacs 24.4)
(electric-indent-mode t)
(show-paren-mode t)
(column-number-mode t)

;; scroll to the top or bottom with C-v and M-v
(setq scroll-error-top-bottom t)

;; M-a and M-e use punct and single space as sentence delimiter
(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)
(delete-selection-mode 1)
(recentf-mode 1)
(setq ring-bell-function #'ignore)
(setq disabled-command-function nil)
(setq suggest-key-bindings t)

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


(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'text-mode-hook-identify)


;; Global rebinds
(global-set-key (kbd "M-/") 'hippie-expand)


;; Customize stuff
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
