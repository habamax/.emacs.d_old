;; Init file for Emacs
;; Maxim Kim <habamax@gmail.com>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Non-Package setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Maxim Kim"
      user-mail-address "habamax@gmail.com")

(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message "")

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control))

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1)


  ;; choose font
  ;; TODO: make a function with a loop
  (cond 
   ;; ((find-font (font-spec :name "Source Code Pro"))
   ;;  (set-face-attribute 'default nil
   ;;                      :family "Source Code Pro"
   ;;                      :height 140))
   ((find-font (font-spec :name "Menlo"))
    (set-face-attribute 'default nil
                        :family "Menlo"
                        :height 140))
   ((find-font (font-spec :name "DejaVu Sans Mono"))
    (set-face-attribute 'default nil
                        :family "DejaVu Sans Mono"
                        :height 160)))

  (setq default-frame-alist '((fullscreen . maximized))))

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

;; (add-hook 'prog-mode-hook '(lambda () (linum-mode 1)))
;; (add-hook 'prog-mode-hook 'linum-mode)

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

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook 'text-mode-hook-identify)


;; Global rebinds
(global-set-key (kbd "M-/") 'hippie-expand)


;; 'Customize' stuff
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages for the rescue
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package haba-comment
  :ensure nil
  :load-path "lisp/"
  :bind ("M-;" . haba-comment-dwim))

(use-package haba-misc
  :ensure nil
  :commands (haba-next-buffer haba-previous-buffer haba-toggle-window-split)
  :load-path "lisp/"
  :bind (("C-c f i" . find-user-init-file)))


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
  :defer 2
  :bind* (("M-x"     . helm-M-x)
          ("C-c M-x" . execute-extended-command)
          ("C-c h"   . helm-command-prefix)
          ("M-s o"   . helm-occur)
          ("M-y"     . helm-show-kill-ring)
          ("C-x C-b" . helm-buffers-list)
          ("C-x b"   . helm-mini)
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
    (setq ;helm-grep-ag-command "pt -e --nogroup --nocolor"
          helm-ag-base-command "pt -e --nogroup --nocolor")
    (helm-mode)
    (add-hook 'helm-minibuffer-set-up-hook 'deactivate-input-method)
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
  :bind ("C-c n" . hydra-cycle-next/body)
  :bind ("C-x o" . hydra-cycle-windows/body)
  :bind ("C-c i" . hydra-insert/body)
  :config
  (defhydra hydra-insert ()
    "Insert"
    ("E" (insert-string "EUR") "EUR")
    ("U" (insert-string "USD") "USD")
    ("R" (insert-string "RUR") "RUR"))
  
  (defhydra hydra-cycle-windows
    (:body-pre (other-window 1))
    "Windows"
    ("o" (other-window 1) "Next")
    ("O" (other-window -1) "Previous")
    ("t" haba-toggle-window-split "Toggle split")
    ("]" enlarge-window-horizontally "Enlarge horizontal")
    ("[" shrink-window-horizontally "Shrink horizontal")
    ("=" enlarge-window "Enlarge vertival")
    ("-" shrink-window "Shrink vertical")
    ("b" balance-windows "Balance windows")
    ("m" delete-other-windows "Maximize window")
    ("c" delete-window "Close window")
    ("q" nil "quit"))
  
  (defhydra hydra-cycle-next ()
    "Frames, Buffers"
    ("f" other-frame "Next frame")
    ("F" (other-frame -1) "Previous frame")
    ("b" haba-next-buffer "Next buffer")
    ("B" haba-previous-buffer "Previous buffer")
    ("k" kill-this-buffer "Kill buffer")
    ("q" nil "quit")))

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
  ;; :disabled t
  :config
  (load-theme 'leuven t))

(use-package gotham-theme
  :disabled t
  :config
  (load-theme 'gotham t))

(use-package solarized-theme
  :ensure nil
  :disabled t
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
  :bind-keymap ("C-c p" . projectile-mode-map)
  :init
      (setq projectile-completion-system 'helm
          projectile-switch-project-action 'helm-projectile
          projectile-enable-caching t
          projectile-file-exists-remote-cache-expire (* 10 60))
  :config
  (progn
    ;; (bind-key "C-c p" 'projectile-command-map projectile-mode-map)
    (use-package helm-projectile
      :config (helm-projectile-on))
    (projectile-global-mode)))


(use-package markdown-mode
  :mode ("\\.\\(markdown|md\\)$" . markdown-mode))


(use-package ledger
  :ensure nil
  :mode ("\\.ledger$" . ledger-mode)
  :init
  (setq ledger-default-date-format "%Y-%m-%d"
        ledger-use-iso-dates t
        ledger-reconcile-default-commodity "RUR"))

(use-package geiser
  :ensure nil
  ;; :mode ("\\.ledger$" . ledger-mode)
  :init
  (setq geiser-active-implementations '(racket)))



;; (use-package wanderlust
;;   :ensure nil
;;   :init
;;   ;; imap
;;   (setq elmo-imap4-default-server "imap.gmail.com"
;;         elmo-imap4-default-user "habamax@gmail.com"
;;         elmo-imap4-default-authenticate-type 'clear
;;         elmo-imap4-default-port '993
;;         elmo-imap4-default-stream-type 'ssl
;;         elmo-imap4-use-modified-utf7 t)
;;   ;; smtp
;;   (setq wl-smtp-connection-type 'starttls
;;         wl-smtp-posting-port 587
;;         wl-smtp-authenticate-type "plain"
;;         wl-smtp-posting-user "habamax"
;;         wl-smtp-posting-server "smtp.gmail.com"
;;         wl-local-domain "gmail.com"
;;         wl-message-id-domain "smtp.gmail.com")

;;   (setq wl-from "Maxim Kim <habamax@gmail.com>"
;;         wl-default-folder "%inbox"
;;         wl-draft-folder   "%[Gmail]/Черновики"
;;         wl-trash-folder   "%[Gmail]/Корзина"
;;         wl-fcc            "%[Gmail]/Отправленные"
;;         wl-fcc-force-as-read t
;;         wl-default-spec "%")

;;   )



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

(use-package erc
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


;; (use-package rcirc
;;   :ensure nil
;;   :init
;;   (setq rcirc-default-nick "habamax"
;;         rcirc-default-user-name "mxmkm"
;;         rcirc-default-full-name "Maxim Kim"
;;         rcirc-fill-column 'frame-width
;;         ;; rcirc-time-format "[%Y-%m-%d %H:%M] "
;;         rcirc-time-format "[%H:%M] "
;;         rcirc-server-alist '(("irc.freenode.net" :channels ("#emacs" "#lor"))))
;;   :config
;;   (rcirc-track-minor-mode 1)
  
;;   (defun-rcirc-command reconnect (arg)
;;      "Reconnect the server process."
;;      (interactive "i")
;;      (unless process
;;        (error "There's no process for this target"))
;;      (let* ((server (car (process-contact process)))
;;             (port (process-contact process :service))
;;             (nick (rcirc-nick process))
;;             channels query-buffers)
;;        (dolist (buf (buffer-list))
;;          (with-current-buffer buf
;;            (when (eq process (rcirc-buffer-process))
;;              (remove-hook 'change-major-mode-hook
;;                           'rcirc-change-major-mode-hook)
;;              (if (rcirc-channel-p rcirc-target)
;;                  (setq channels (cons rcirc-target channels))
;;                (setq query-buffers (cons buf query-buffers))))))
;;        (delete-process process)
;;        (rcirc-connect server port nick
;;                       rcirc-default-user-name
;;                       rcirc-default-full-name
;;                       channels)))
;;   )


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
