;;; init.el -- Emacs initialization file
;;; Maxim Kim <habamax@gmail.com>

;;; Commentary:
;; Carefully crafted settings for alien's editor I become used to.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Non-Package setup

;; General OSX setup
(when (eq system-type 'darwin)
  ;; No new frames for files that are opened from OSX
  (setq ns-pop-up-frames nil)
  ;; command to meta, option to control
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'control))

(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message "")

;; The user with tentacles
(setq user-full-name "Maxim Kim"
      user-mail-address "habamax@gmail.com")

;; RU stuff
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq default-input-method 'russian-computer)

;; make unix lineendings default
(setq default-buffer-file-coding-system 'utf-8-unix)

;; scroll to the top or bottom with C-v and M-v
(setq scroll-error-top-bottom t)

;; not so jumpy mouse scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

;; M-a and M-e use punct and single space as sentence delimiter
(setq sentence-end-double-space nil)

(delete-selection-mode 1)
(setq ring-bell-function #'ignore)
(setq disabled-command-function nil)
(setq suggest-key-bindings t)

(electric-indent-mode t)
(global-subword-mode t)
(show-paren-mode t)
(column-number-mode t)
(recentf-mode 1)

;; fill text with M-q or auto-fill-mode
(setq-default fill-column 80)

;; winner mode is a must
(winner-mode 1)

;; tabs are evil...
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

;; abbrev
(setq abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
;; (setq save-abbrevs t)
(setq-default abbrev-mode t)


;; dired user another dired buffer as destination for copy/move
(setq dired-dwim-target t)


;; use hippie-expand
;; (global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Keep 'Customize' stuff separated
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Set up packaging system
(let ((package-protocol (if (eq system-type 'windows-nt) "http://" "https://")))
  (setq package-archives `(("elpa" . ,(concat package-protocol "elpa.gnu.org/packages/"))
                           ("melpa" . ,(concat package-protocol "melpa.org/packages/")))))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure nil)

(use-package haba-appearance
  :load-path "lisp/"
  :config
  (use-package leuven-theme :defer)
  (use-package base16-theme :defer)
  (haba/set-current-theme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Local packages
(use-package haba-stuff
  :load-path "lisp/"
  :commands (haba/next-buffer
             haba/previous-buffer
             haba/toggle-window-split
             haba/fill-or-unfill)
  :bind (("M-;" . haba/toggle-comment)
         ("C-a" . haba/move-beginning-of-line)
         ("C-M-j" . haba/join-line)
         ("C-c o i" . haba/open-init-file)
         ("C-c o s" . haba/open-scratch-buffer)
         ("C-c o t" . haba/open-todo-file)
         ([remap fill-paragraph] . haba/fill-or-unfill)
         ("C-c i d" . haba/insert-current-date)
         ("<C-wheel-up>" . text-scale-increase)
         ("<C-wheel-down>" . text-scale-decrease)
         ("M-0" . haba/restore-windows-layout-from-register)
         ("C-M-0" . haba/save-windows-layout-to-register))
  :config
  (defun haba/open-scratch-buffer ()
    "Open scratch buffer"
    (interactive)
    (switch-to-buffer "*scratch*"))
  (defun haba/open-todo-file ()
    "Open todo.adoc file"
    (interactive)
    (find-file "~/docs/todo.adoc"))
  )

;; STARTUP: 0.7

(use-package asciidoctor-mode
  :load-path "lisp/"
  :mode ("\\.\\(adoc\\|asciidoc\\)$" . asciidoctor-mode)
  :config
  ;; (setq asciidoctor-pdf-executable (concat "ruby " (expand-file-name "~/projects/asciidoctor-pdf/bin/asciidoctor-pdf")))
  (setq asciidoctor-pdf-stylesdir "~/docs/AsciiDocThemes")
  (setq asciidoctor-pdf-fontsdir "~/docs/AsciiDocThemes/fonts")
  (setq asciidoctor-pdf-extensions "asciidoctor-diagram")
  )

;; STARTUP: 0.8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Melpa packages

;; PATH for OSX
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  :if (eq system-type 'darwin))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(use-package which-key
  :defer 3
  :diminish which-key-mode
  :config (which-key-mode))

(use-package expand-region
  :bind (("M-m" . er/expand-region))
  :config
  (setq expand-region-contract-fast-key "M")

  (defun er/add-text-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
                              er/try-expand-list
                              '(mark-paragraph))))

  (add-hook 'text-mode-hook 'er/add-text-mode-expansions))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))


(use-package avy
  :diminish avy-mode
  :bind (("C-l" . avy-goto-word-1)))


(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

;; (use-package ido
;;   :config
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-everywhere t)
;;   (setq ido-use-virtual-buffers t)
;;   (setq ido-use-filename-at-point 'guess)
;;   (setq ido-create-new-buffer 'always)
;;   (ido-mode 1)
;;   (use-package smex :bind (("M-x" . smex)))
;;   (use-package ido-ubiquitous :config (ido-ubiquitous-mode 1))
;;   (use-package flx-ido
;;     :config
;;     (setq ido-enable-flex-matching t)
;;     (setq ido-use-faces nil)
;;     (flx-ido-mode 1))
;;   (use-package ido-vertical-mode
;;     :config
;;     (ido-vertical-mode 1)
;;     (setq ido-vertical-show-count t)
;;     (setq ido-vertical-define-keys 'C-n-and-C-p-only)))


(use-package ivy
  :defer 2
  :diminish ivy-mode
  :init
  ;; clear default ^ for counsel-M-x and friends
  (setq ivy-initial-inputs-alist '())
  :config
  (use-package ivy-hydra :defer)
  (use-package flx :defer)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))


(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-s s" . swiper)
         ("M-s t" . haba/counsel-projectile-rg-todo)
         ("M-s r" . counsel-rg)
         ("C-c s" . swiper-all)
         ("C-x b" . ivy-switch-buffer)
         ("M-y" . counsel-yank-pop))
  :config
  ;; counsel uses smex for better sorting
  (use-package smex :defer)

  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

  (setq ivy-ignore-buffers '(".*-autoloads.el"))
  (setq ivy-switch-buffer-faces-alist '((dired-mode . ivy-subdir)))

  (setq counsel-yank-pop-separator (concat "\n" (make-string 70 ?-) "\n"))


  (defun haba/counsel-projectile-rg-todo ()
    (interactive)
    (ivy-set-prompt 'haba/counsel-projectile-rg-todo counsel-prompt-function)
    (setq counsel--git-grep-dir (projectile-project-root))
    (ivy-read "TODO"
               (lambda (string)
                 (counsel-ag-function (concat "TODO: " string)
                                      counsel-rg-base-command ""))
               :initial-input ""
               :dynamic-collection t
               :keymap counsel-ag-map
               :history 'counsel-git-grep-history
               :action #'counsel-git-grep-action
               :unwind (lambda ()
                         (counsel-delete-process)
                         (swiper--cleanup))
               :caller 'haba/counsel-projectile-rg-todo))

  (counsel-set-async-exit-code 'haba/counsel-projectile-rg-todo 1 "No matches found")
  )


(use-package projectile
  :commands (projectile-project-root)
  :bind-keymap (("C-c p" . projectile-mode-map))
  :bind (:map projectile-mode-map ("C-c p s r" . projectile-ripgrep))
  :config
  (use-package projectile-ripgrep)
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package hydra
  :bind ("C-c w" . hydra-windows/body)
  :bind ("C-c t" . hydra-toggle-theme/body)
  :bind ("C-x o" . hydra-other-window/body)

  :config

  (defhydra hydra-windows (:hint nil)
    "
^Windows^                    ^Size^              ^Split^
--------------------------------------------------------------------
_w_: Winner undo          _]_: + width         _t_: Toggle
_W_: Winner redo          _[_: - width         _h_: Below
_p_: Ivy push view        _=_: + height        _v_: Right
_P_: Ivy pop view         _-_: - height        _m_: Maximize current
^ ^                       _b_: balance         _c_: Close current
"
    ("w" winner-undo)
    ("W" winner-redo)
    ("t" haba/toggle-window-split)
    ("h" split-window-below)
    ("v" split-window-right)
    ("m" delete-other-windows)
    ("c" delete-window)
    ("]" enlarge-window-horizontally)
    ("[" shrink-window-horizontally)
    ("=" enlarge-window)
    ("-" shrink-window)
    ("b" balance-windows)
    ("p" ivy-push-view)
    ("P" ivy-pop-view)
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
    ("q" nil "quit")))

;; Complete Anything
(use-package company
  :defer 2
  :diminish company-mode
  :config
  (use-package company-flx :config (company-flx-mode +1))

  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)

  (setq company-minimum-prefix-length 2)

  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf
           company-yasnippet
           )
          (company-abbrev company-dabbrev)
          ))

  (define-key company-active-map [tab] 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-j") 'company-abort)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  (global-company-mode))



(use-package multiple-cursors
  :bind-keymap (("C-x m" . haba/mc-map))
  :bind (("M-j" . haba/mark-next-word-like-this)
         ("M-J" . mc/unmark-next-like-this)
         :map haba/mc-map
         ("m" . mc/mark-all-like-this-dwim)
         ("d" . mc/mark-all-symbols-like-this-in-defun)
         ("i" . mc/insert-numbers)
         ("l" . mc/edit-lines)
         ("e" . mc/edit-ends-of-lines)
         ("a" . mc/edit-beginnings-of-lines))
  :init
  (define-prefix-command 'haba/mc-map)
  (define-key ctl-x-map "m" 'haba/mc-map)
  :config
  (defun haba/mark-next-word-like-this (arg)
    (interactive "p")
    (if (region-active-p)
        (let ((mc/enclose-search-term 'words))
          (mc/mark-next-like-this arg))
      (mc--select-thing-at-point 'word))
    (when (not (region-active-p))
        (mc/mark-next-lines 1))))



(use-package rainbow-delimiters
  :defer
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package smartparens
  :diminish smartparens-mode
  :defer 1
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'sp)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode)
  (show-smartparens-global-mode)

  ;; wrap selection with a symbols
  (sp-pair "*" "*" :actions '(wrap))
  (sp-pair "_" "_" :actions '(wrap))
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "/" "/" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  (sp-pair "-" "-" :actions '(wrap))
  )

(use-package magit
  :commands (magit-status)
  :bind ("C-c g" . magit-status)
  :config
  (setq magit-log-arguments '("--graph" "--show-signature")
        magit-push-always-verify nil
        magit-popup-use-prefix-argument 'default
        magit-revert-buffers t))


;; git-gutter and git-gutter-fringe are mutually exclusive
;; fringe version doesn't work for terminal
(use-package git-gutter-fringe
  :defer 4
  :if (display-graphic-p)
  :config
  (global-git-gutter-mode 1))

(use-package git-gutter
  :defer 4
  :if (not (display-graphic-p))
  :config
  (global-git-gutter-mode 1))

(use-package git-timemachine
  :commands (git-timemachine))

(use-package markdown-mode
  :mode ("\\.\\(markdown\\|md\\)$" . markdown-mode))

(use-package plantuml-mode
  :mode ("\\.\\(uml\\)$" . plantuml-mode))

(use-package go-mode
  :mode ("\\.\\(go\\)$" . go-mode))

(use-package web-mode
  :mode ("\\.\\(html\\|css\\)$" . web-mode)
  :config
  (setq web-mode-enable-auto-expanding t
        web-mode-enable-auto-pairing t))

(use-package groovy-mode
  :mode ("\\.groovy$" . groovy-mode))

;; flycheck
(use-package flycheck
  :defer
  :diminish 'flycheck-mode
  :config (global-flycheck-mode))

;; flyspell - use aspell instead of ispell
(use-package flyspell
  :defer
  :commands (flyspell-mode flyspell-prog-mode)
  :config (setq ispell-program-name (executable-find "aspell")
                ispell-extra-args '("--sug-mode=ultra")))


(use-package neotree
  :defer
  :commands (neotree-toggle)
  :bind ("M-1" . neotree-toggle))


;; yasnippets
(use-package yasnippet
  :defer 2
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))


(use-package python-mode
  :mode ("\\.\\(py\\)$" . python-mode)
  :config
  ;; (setq python-shell-interpreter "python3")
  )

(use-package rust-mode
  :mode ("\\.\\(rs\\)$" . rust-mode)
  :config
  (setq rust-format-on-save t)
  )

(use-package clojure-mode
  :mode ("\\.\\(clj\\)$" . clojure-mode)
  :config
  (use-package cider
    :config
    (setq cider-repl-display-help-banner nil)))

(use-package slime
  :commands (slime)
  :config
  (setq inferior-lisp-program "ros -Q run")
  (slime-setup '(slime-fancy))
  )

;; (use-package lispy
;;   :defer
;;   :init
;;   (defun haba/enable-lispy-mode ()
;;     (lispy-mode 1))

;;   (add-hook 'emacs-lisp-mode-hook 'haba/enable-lispy-mode)
;;   (add-hook 'clojure-mode-hook 'haba/enable-lispy-mode))

;; haskell stuff
(use-package intero
  :mode ("\\.\\(hs\\)$" . inter-mode))

;; Music that just works (if you have mplayer installed :))
(use-package bongo
  :defer
  :commands (bongo)
  :config
  (use-package volume)
  (setq bongo-display-inline-playback-progress t)
  (setq bongo-insert-album-covers t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Built-in packages

;; dired-jump befor dired is used
(use-package dired-x
  :ensure nil
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)))


;; Outline-mode
(use-package outline
  :bind (("M-o" . hydra-outline/body))
  :diminish outline-mode outline-minor-mode
  :defer
  :ensure nil
  :config
  (outline-minor-mode 1)

  (defhydra hydra-outline ()
    "Outline"
    ("o" outline-toggle-children "Toggle children")
    ("h" hide-body "Hide bodies")
    ("a" show-all "Show all")

    ("SPC" nil "quit")
    ("q" nil "quit"))
  )


(use-package rcirc
  :defer
  :commands (irc rcirc)
  :ensure nil
  :config
  (setq rcirc-default-user-name "habamax"
        rcirc-default-nick      "habamax"
        rcirc-default-full-name "Maxim Kim")
  (setq rcirc-auto-authenticate-flag t)
  (setq rcirc-time-format "[%H:%M] ")
  (rcirc-track-minor-mode 1)

  (ignore-errors
    (load (concat user-emacs-directory "freenode-pass"))
    (setq rcirc-authinfo
          `(("freenode" nickserv "habamax" ,freenode-habamax-pass))))

  (add-to-list 'rcirc-server-alist
               '("irc.freenode.net"
                 :nick "habamax"
                 :channels ("#emacs" "#lor")))


  (defun haba/rcirc-mode-setup ()
    "Sets things up for channel and query buffers spawned by rcirc."
    ;; rcirc-omit-mode always *toggles*, so we first 'disable' it
    ;; and then let the function toggle it *and* set things up.
    (setq rcirc-omit-mode nil)
    (rcirc-omit-mode))

  (add-hook 'rcirc-mode-hook 'haba/rcirc-mode-setup)
  )

(use-package calendar
  :init
  ;; Calendar -- говорим и показываем по русски.
  (setq calendar-date-style 'iso
        calendar-week-start-day 1
        calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
        calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель"
                                   "Май" "Июнь" "Июль" "Август"
                                   "Сентябрь" "Октябрь" "Ноябрь" "Декабрь"]))


;;; init.el ends here
