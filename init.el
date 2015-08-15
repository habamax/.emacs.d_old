;; Init file for Emacs
;; Maxim Kim <habamax@gmail.com>

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
;;;;



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
	  helm-tramp-verbose 9)
    (helm-mode)
    (helm-autoresize-mode t)))

;; company "complete anything"
(use-package company
  :commands (company-mode)
  :config
  (progn
    (use-package company-c-headers)
    (push '(company-clang
	    :with company-semantic
	    :with company-yasnippet
	    :with company-c-headers)
          company-backends)
    (setq company-minimum-prefix-length 2
          company-idle-delay nil
	  company-global-modes '(not gud-mode))))


(use-package helm-company
  :bind ("<backtab>" . helm-company)
  :commands (helm-company)
  :config
  (progn
    (company-mode)
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company)))


;; ace-window
(use-package ace-window
  :bind* ("M-o" . ace-window)
  :config
  (progn
    (setq aw-keys '(?l ?k ?j ?h ?o ?i ?u ?m ?n))
    (add-to-list 'golden-ratio-extra-commands 'ace-window)))

(use-package golden-ratio
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


(use-package zenburn-theme		
  :config (load-theme 'zenburn t))

;; rust
;; (use-package rust-mode
;;   :mode "\\.rs\\'"
;;   :config (use-package flycheck-rust))

;; magit
(use-package magit
  :commands (magit-status projectile-vc)
  :config
  (setq magit-log-arguments '("--graph" "--show-signature")
	magit-push-always-verify nil
	magit-popup-use-prefix-argument 'default
	magit-revert-buffers t))


;; ;; flycheck
;; (use-package flycheck
;;   :init (global-flycheck-mode))

;; (use-package helm-flycheck
;;   :bind ("C-c ! h" . helm-flycheck)
;;   :config (global-flycheck-mode))

;; ;; flyspell - use aspell instead of ispell
;; (use-package flyspell
;;   :commands (flyspell-mode flyspell-prog-mode)
;;   :config (setq ispell-program-name (executable-find "aspell")
;;                 ispell-extra-args '("--sug-mode=ultra")))


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


;; RU stuff
(set-language-environment 'Russian)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)



(electric-indent-mode t)		; auto indent on Enter (should be default since Emacs 24.4)
(show-paren-mode t)			; show matching parens
(tool-bar-mode -1)			; no toolbar
(scroll-bar-mode -1)			; no scrollbar
(column-number-mode t)			; column/line numbers on modeline

(setq scroll-error-top-bottom t)	; scroll to the top or bottom with C-v and M-v
(setq sentence-end-double-space nil)	; M-a and M-e use punct and single space as sentence delimiter

(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)

(setq ring-bell-function #'ignore)

(recentf-mode 1)			; enable recent files

(put 'upcase-region 'disabled nil)	; C-x C-u
(put 'downcase-region 'disabled nil)	; C-x C-l

(setq default-major-mode 'text-mode)
(setq initial-major-mode 'text-mode)

(setq make-backup-files nil)
(setq suggest-key-bindings t)

;; y/n for yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; quit prompt
(setq confirm-kill-emacs 'y-or-n-p)

;; Calendar
(setq calendar-date-style 'iso)
(setq calendar-week-start-day 1)
(setq calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"])
(setq calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май" "Июнь" "Июль" "Август" "Сентябрь" "Октябрь" "Ноябрь" "Декабрь"])

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'text-mode-hook-identify)

;; org mode
(setq org-src-fontify-natively t)
(setq org-fontify-whole-heading-line t)



