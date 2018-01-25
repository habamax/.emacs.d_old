;;; init.el -- Emacs initialization file
;;; Maxim Kim <habamax@gmail.com>

;;; Commentary:
;; Carefully crafted settings for alien's editor I become used to.

;;; Code:

;; Measure loading time
(defconst emacs-start-time (current-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Non-Package setup

;; disable gc for init
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))


(defun OSX? ()
  "return true if OSX"
  (eq system-type 'darwin))

(defun windows? ()
  "return true if Windows"
  (eq system-type 'windows-nt))

;; General OSX setup
(when (OSX?)
  ;; No new frames for files that are opened from OSX
  (setq ns-pop-up-frames nil)
  ;; Show menu by default
  (menu-bar-mode 1)
  ;; command to meta, option to control
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'control))

(when (not (OSX?))
  (menu-bar-mode -1))

(tool-bar-mode -1)

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

;; This is windows only
;; I have finally solved the problem of external processes to use Russian language,
;; Now rg.exe can actually search Russian words from Emacs.
(when (windows?)
  (setq default-process-coding-system '(utf-8-dos . cp1251-dos)))

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

;; can help with very long lines processing (speedwise)
(setq-default bidi-display-reordering nil)

;; fill text with M-q or auto-fill-mode
(setq-default fill-column 80)

;; do not wrap lines by default
(setq-default truncate-lines t)

;; scroll screen when there is 1 line left
;; it is also good for follow-mode -- it prevents cursor to go to followed window
(setq scroll-margin 1)

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
(save-place-mode 1)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fix the minibuffer gc triggering

(defun haba/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun haba/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'haba/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'haba/minibuffer-exit-hook)


;; default frame is fullscreen and has no scrollbars
;; (setq default-frame-alist '((fullscreen . maximized) (vertical-scroll-bars . nil)))
;; use -geometry 120x40 for example to set initial size
(if (OSX?)
    (setq default-frame-alist '((fullscreen . maximized)
                                (vertical-scroll-bars . nil)))
  (setq default-frame-alist '((fullscreen . nil)
                              (vertical-scroll-bars . nil))))
(setq frame-title-format "%b")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Set up packaging system
(let ((package-protocol (if (windows?) "http://" "https://")))
  (setq package-archives `(("elpa" . ,(concat package-protocol "elpa.gnu.org/packages/"))
                           ("melpa" . ,(concat package-protocol "melpa.org/packages/")))))

(package-initialize)
(setq package-enable-at-startup nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(setq use-package-always-ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Local packages

(use-package haba-stuff
  :ensure nil
  :load-path "lisp/"
  :commands (haba/next-buffer
             haba/previous-buffer
             haba/fill-or-unfill
             haba/pretty-print-xml-region
             load-theme)
  :bind (("C-w" . haba/kill-region)
         ("M-w" . haba/kill-ring-save)
         ("M-;" . haba/toggle-comment)
         ("C-a" . haba/move-beginning-of-line)
         ("M-j" . haba/join-line)
         ("C-o" . haba/open-line)
         ("M-2" . haba/duplicate-line)
         ("C-M-2" . haba/duplicate-line-inc-numbers)
         ("M-3" . haba/move-line-down)
         ("M-4" . haba/move-line-up)
         ("M-9" . haba/previous-window)
         ("M-0" . haba/next-window)
         ("C-c o i" . haba/open-init-file)
         ("C-c o s" . haba/open-scratch-buffer)
         ("C-c o t" . haba/open-todo-file)
         ([remap fill-paragraph] . haba/fill-or-unfill)
         ("C-c i d" . haba/insert-current-date)
         ("<C-wheel-up>" . text-scale-increase)
         ("<C-wheel-down>" . text-scale-decrease)
         ("<f10>" . menu-bar-mode)
         ("C-x C-b" . ibuffer))
  :config
  (defun disable-all-themes (&rest args)
    (mapcar #'disable-theme custom-enabled-themes))
  (advice-add 'load-theme :before #'disable-all-themes)
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

(use-package diminish
  :diminish abbrev-mode auto-revert-mode subword-mode)

(use-package habamax-theme
  :load-path "habamax-theme/"
  :config
  (load-theme 'habamax t))

(use-package leuven-theme :defer)
(use-package kosmos-theme :defer)
(use-package solarized-theme :defer
  :init
  (setq solarized-high-contrast-mode-line t))


(use-package whitespace-cleanup-mode
  :defer 5
  :diminish
  :commands (whitespace-cleanup-mode)
  :config
  (global-whitespace-cleanup-mode))

(use-package winner
  :defer 3
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package asciidoctor-mode
  :ensure nil
  :load-path "lisp/"
  :mode ("\\.\\(adoc\\|asciidoc\\)$" . asciidoctor-mode)
  :bind (:map asciidoctor-mode-map
              ("C-c C-o C-o" . browse-url-of-buffer)
              ("C-c C-h C-h" . asciidoctor-compile-html)
              ("C-c C-p C-p" . asciidoctor-compile-pdf)
              ("C-c C-h C-o" . asciidoctor-open-html)
              ("C-c C-p C-o" . asciidoctor-open-pdf)
              ("C-c C-y" . asciidoctor-save-image-insert-link))
  :config
  ;; (setq asciidoctor-pdf-executable (concat "ruby " (expand-file-name "~/projects/asciidoctor-pdf/bin/asciidoctor-pdf")))
  (setq asciidoctor-pdf-stylesdir "~/docs/AsciiDocThemes")
  (setq asciidoctor-pdf-fontsdir "~/docs/AsciiDocThemes/fonts")
  (setq asciidoctor-pdf-extensions '("asciidoctor-diagram"))
  (setq asciidoctor-extensions '("asciidoctor-diagram" "asciidoctor-rouge"))
  (when (OSX?)
      (setq asciidoctor-clipboard-backend "pngpaste %s%s")))

;; STARTUP: 0.8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Melpa packages

;; PATH for OSX
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  :if (OSX?))

;; sometimes text mangling is just too cumbersome with emacs bindings...
;; be EVIL then and SHOW THEM ALL!!!
(use-package evil
  :bind (("C-c e" . haba/become-evil-or-not)
         :map evil-emacs-state-map
         ([escape] . evil-normal-state)
         ("ESC" . evil-normal-state)
         :map evil-visual-state-map
         ([escape] . evil-normal-state)
         ("ESC" . evil-normal-state))
  :commands (evil-mode)
  :config
  (defalias 'evil-insert-state 'evil-emacs-state)

  (defun haba/become-evil-or-not ()
    (interactive)
    (if (bound-and-true-p evil-state)
        (progn
        (evil-mode -1)
        (message "Calm down. Don't be evil, be nice..."))
      (progn
        (evil-mode)
        (message "I!     AM!     EVIL!!!!")))))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

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
  :bind (("C-l" . avy-goto-char-timer)))

(use-package ace-window
  :bind (("C-x o" . ace-window))
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; (use-package treemacs
;;   :bind (("<f8>" . treemacs-toggle))
;;   :config
;;   (treemacs-follow-mode t)
;;   (treemacs-filewatch-mode t))

;; (use-package treemacs-projectile
;;   :after treemacs
;;   :bind (("S-<f8>" . treemacs-projectile-toggle)))

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
                                (t . ivy--regex-fuzzy)))

  (defun haba/save-ivy-views ()
    (interactive)
    (with-temp-file (concat user-emacs-directory "ivy-views")
      (prin1 ivy-views (current-buffer))
      (message "Save ivy-views")))

  (defun haba/load-ivy-views ()
    (interactive)
    (setq ivy-views
          (with-temp-buffer
            (insert-file-contents (concat user-emacs-directory "ivy-views"))
            (read (current-buffer))))
    (message "Load ivy-views"))
  )


(use-package telephone-line
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-primary-right-separator 'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-nil)

  (setq telephone-line-height 22)
  (telephone-line-mode 1))

;; counsel uses smex for better sorting
(use-package smex :after counsel)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-h a" . counsel-apropos)
         ("C-x C-f" . counsel-find-file)
         ("M-s s" . swiper)
         ("M-s t" . haba/counsel-projectile-rg-todo)
         ("M-s r" . counsel-rg)
         ("C-x b" . ivy-switch-buffer)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :config
  (setq counsel-yank-pop-preselect-last t)
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; pdf-files
         "\\(?:\\`.+?\\.pdf\\'\\)"
         ;; file names beginning with # or .
         "\\|\\(?:\\`[#.]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

  (setq ivy-ignore-buffers '(".*-autoloads.el"))
  (setq ivy-switch-buffer-faces-alist '((dired-mode . ivy-subdir)))
  (setq ivy-use-selectable-prompt t)

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


;; goto last change
(use-package goto-chg
  :bind (("M-5" . goto-last-change)
         ("M-6" . goto-last-change-reverse)))

;; sort buffers by git projects
(use-package ibuffer-vc
  :commands (ibuffer)
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package projectile-ripgrep :after projectile)

(use-package projectile
  :commands (projectile-project-root)
  :bind-keymap (("C-c p" . projectile-mode-map))
  :bind (:map projectile-mode-map ("C-c p s r" . projectile-ripgrep))
  :diminish projectile-mode
  :config

  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package hydra :defer)

;; Complete Anything
(use-package company
  :defer 3
  ;; :demand
  :diminish company-mode
  :bind (("TAB" . company-indent-or-complete-common))
  :config
  (use-package company-flx :config (company-flx-mode +1))

  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-idle-delay 0.2)

  (setq company-minimum-prefix-length 2)

  (company-tng-configure-default)
  (global-company-mode))



(use-package multiple-cursors
  :defer 3
  :bind-keymap (("C-x m" . haba/mc-map))
  :bind (("C-M-m" . haba/mark-next-like-this)
         ;; ("C-M-u" . mc/unmark-next-like-this)
         ("C-M-S-m" . haba/skip-to-next-like-this)
         ("C-8" . mc/mark-next-lines)
         ("C-7" . mc/unmark-next-like-this)
         ("M-S-<mouse-1>" . mc/add-cursor-on-click)
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
  (setq mc/insert-numbers-default 1)
  (defun haba/mark-next-like-this (arg)
    (interactive "p")
    (if (region-active-p)
        (progn
          (mc/mark-next-like-this arg)
          (mc/cycle-forward))
      (mc--select-thing-at-point 'word)))
  (defun haba/skip-to-next-like-this ()
    (interactive)
    (when (and (region-active-p) (mc/furthest-cursor-before-point))
      (mc/cycle-backward)
      (mc/mark-more-like-this t 'forwards)
      (mc/cycle-forward))
    (mc/maybe-multiple-cursors-mode)))



(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(use-package page-break-lines :diminish :hook (prog-mode . turn-on-page-break-lines-mode))



(use-package smartparens
  :diminish smartparens-mode
  :defer 3
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
  (sp-pair "«" "»" :actions '(wrap))
  (sp-pair "“" "”" :actions '(wrap))
  (sp-pair "´" "´" :actions '(wrap))
  (sp-pair "`" "`" :actions '(wrap))
  (sp-pair "'" "'" :actions '(wrap)))

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
  :diminish git-gutter-mode
  :if (display-graphic-p)
  :config
  (global-git-gutter-mode 1))

(use-package git-gutter
  :defer 4
  :if (not (display-graphic-p))
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(use-package git-timemachine
  :commands (git-timemachine))

(use-package markdown-mode
  :mode ("\\.\\(markdown\\|md\\)$" . markdown-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package plantuml-mode
  :mode ("\\.\\(uml\\)$" . plantuml-mode))

(use-package go-mode
  :mode ("\\.\\(go\\)$" . go-mode))

(use-package web-mode
  :mode ("\\.\\(html\\|css\\)$" . web-mode)
  :config
  (setq web-mode-enable-auto-expanding t
        web-mode-enable-auto-pairing t))


(use-package elixir-mode
  :mode ("\\.\\(ex\\|exs\\)$" . elixir-mode))

(use-package alchemist
  :after elixir-mode)


(use-package yaml-mode
  :mode ("\\.\\(yml\\)$" . yaml-mode))

(use-package emmet-mode
  :defer
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode))

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


(use-package ztree :defer
  ;; :bind (:map ztree-mode-map
              ;; ("n" . next-line)
              ;; ("p" . previous-line)
              ;; ("v" . scroll-up-command))
  )

(use-package beacon
  :defer 6
  :config
  (setq beacon-color 0.3)
  (setq beacon-size 20)
  (setq beacon-blink-duration 0.2)
  (setq beacon-blink-when-window-scrolls nil
        beacon-blink-when-point-moves-horizontally nil
        beacon-blink-when-buffer-changes nil)
  (beacon-mode 1))

;; (use-package minimap :defer :commands minimap-mode)

(use-package string-edit :commands string-edit)

(use-package string-inflection :bind (("C-c C-u" . string-inflection-toggle)))

;; yasnippets
(use-package yasnippet
  :defer 5
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
    (setq cider-repl-display-help-banner nil)
    (setq cider-repl-use-pretty-printing t)))

(use-package slime
  :commands (slime)
  :config
  (setq inferior-lisp-program "ros -Q run")
  (slime-setup '(slime-fancy)))

;; new golden-ratio
(use-package zoom
  :defer 5
  ;; :bind ("C-x +" . zoom)
  ;; :preface
  ;; (defun size-callback ()
  ;;   (cond ((> (frame-pixel-width) 1280) '(0.618 . 0.618))
  ;;         (t '(0.5 . 0.5))))
  :config
  (setq zoom-size '(90 . 30))
  ;; (setq zoom-ignored-buffer-names '("Treemacs"))
  (zoom-mode))

(use-package dired+
  :commands (dired dired-jump)
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (require 'dired+)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t))

(use-package hledger-mode
  :mode ("\\.journal$" . hledger-mode)
  :bind (("C-c o l" . hledger-jentry)
         ("C-c j" . hledger-run-command))
  :init
  ;; make it aware of non english accounts
  (setq hledger-account-regex "\\(\\([[:alnum:]-]+\\)\\(:[[:alnum:]-]+\\)+\\)")
  :config
  (setq hledger-currency-string "RUR")
  (setq hledger-jfile "~/fin/2018.journal")
  (setq hledger-life-expectancy 80)
  (setq hledger-year-of-birth 1978)
  ;; XXX company is deffered thus -- this gives errors
  (add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company))))

(use-package restclient
  :commands (restclient-mode))

;; music FTW
(use-package emms
  :bind (("C-c m d" . emms-play-directory-tree)
         ("C-c m c" . emms-playlist-mode-go)
         ("C-c m p" . emms-pause)
         ("C-c m n" . emms-next)
         ("C-c m r" . emms-random)
         ("C-c m s" . emms-stop))
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)

  (setq emms-source-file-default-directory "~/Music")

  (setq emms-mode-line-icon-color "yellow")

  (setq emms-repeat-playlist t))


(use-package nov
  :mode ("\\.\\(epub\\)$" . nov-mode))

(use-package htmlize
  :defer
  :commands (htmlize-buffer htmlize-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Built-in packages


(use-package remember
  :bind (("C-c r r" . remember)
         ("C-c r n" . remember-notes))
  :config
  (setq remember-data-file (substitute-env-in-file-name "$HOME/docs/notes.adoc"))
  (setq remember-notes-initial-major-mode 'asciidoctor-mode)
  (setq remember-leader-text "== ")
  (defun haba/remember-add-to-file ()
  "Remember, with description DESC, the given TEXT. Add text to the beginning."
  (let* ((text (buffer-string))
         (desc (remember-buffer-desc))
         (remember-text (concat "\n" remember-leader-text (format-time-string "%F %H:%M:%S")
                                " " desc "\n\n" text
                                (save-excursion (goto-char (point-max))
                                                (if (bolp) nil "\n")))))
    (with-current-buffer (find-file-noselect remember-data-file)
      (goto-char (point-min))
      (search-forward-regexp "^[[:blank:]]*$") ;; dumb skip asciidoctor title
      (insert remember-text)
      (if remember-save-after-remembering (save-buffer)))))

  (setq remember-handler-functions 'haba/remember-add-to-file))

(use-package autorevert
  :mode ("\\(catalina.out\\|\\.log$\\)" . auto-revert-tail-mode)
  :config
  ;; (view-mode)
  (defun haba/log-tail-handler ()
    (end-of-buffer)
    (setq auto-revert-interval 1)
    (auto-revert-set-timer)
    (make-variable-buffer-local 'auto-revert-verbose)
    (setq auto-revert-verbose nil)
    (view-mode t)
    (font-lock-mode 0)
    (when (fboundp 'show-smartparens-mode)
      (show-smartparens-mode 0)))

  (add-hook 'auto-revert-tail-mode-hook 'haba/log-tail-handler))

(use-package eshell
  :ensure nil
  :bind ("M-`" . haba/toggle-eshell-here)
  :config

  (defun haba/company-off ()
     (interactive)
     (company-mode -1))
  (add-hook 'eshell-mode-hook 'haba/company-off)

  (defun haba/eshell-here (eshell-name)
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (let ((height (/ (window-total-height) 3)))
      (split-window-vertically (- height))
      (other-window 1)

      (if (get-buffer eshell-name)
          (switch-to-buffer eshell-name)
        (progn
          (eshell "new")
          (rename-buffer eshell-name)
          ;; (insert (concat "ls"))
          ;; (eshell-send-input)
          ))))

  (defun haba/toggle-eshell-here ()
    "Toggle eshell-here."
    (interactive)

   (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
          (name   (car (last (split-string parent "/" t))))
          (eshell-name (concat "*eshell: " name "*")))

     (cond ((string-prefix-p "*eshell: " (buffer-name (window-buffer)))
            ;; (message "Visible and focused")
            (insert "exit")
            (eshell-send-input)
            (delete-window))
           ((get-buffer-window eshell-name)
            ;; (message "Visible and unfocused")
            (switch-to-buffer-other-window eshell-name))
           (t
            ;; (message "Not visible")
            (haba/eshell-here eshell-name)))))

  (defun eshell/wintree ()
    "Generate current dir tree using ASCII and built in Windows tree executable"
    (interactive "P")
    (insert "tree /A /F")
    (eshell-send-input))
  )

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show emacs startup time
(defun display-startup-echo-area-message ()
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading Emacs configuration... DONE (%.3fs),  overral emacs-init-time: %s" elapsed (emacs-init-time))))

;;; init.el ends here
