;;; init.el -- Emacs initialization file
;;; Maxim Kim <habamax@gmail.com>

;;; Commentary:
;; Carefully crafted settings for alien's editor I become used to.

;;; Code:

;; Start measuring loading time
(defconst emacs-start-time (current-time))

(defconst *is-osx* (eq system-type 'darwin))
(defconst *is-windows* (eq system-type 'windows-nt))

;; ================================================================================
;; Non-Package setup
;; ================================================================================

;; disable gc for init
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))


;; General OSX setup
(when *is-osx*
  ;; No new frames for files that are opened from OSX
  (setq ns-pop-up-frames nil)
  ;; Show menu by default
  (menu-bar-mode 1)
  ;; command to meta, option to control
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'control))

(when (not *is-osx*)
  (menu-bar-mode -1))

(when window-system
  (tool-bar-mode -1))

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
(when *is-windows*
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

;; autosave minibuffer history
(savehist-mode 1)

;; C-u C-SPC C-SPC to pop mark twice...
(setq set-mark-command-repeat-pop t)

;; Tab to indent or complete
;; (setq tab-always-indent 'complete)

;; abbrev
(setq abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
;; (setq save-abbrevs t)
(setq-default abbrev-mode t)


;; use hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; S-left, S-right, S-down, S-up to switch windows
(windmove-default-keybindings)

;; Keep 'Customize' stuff separated
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Just in case, C-n speedup
;; See https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
(setq auto-window-vscroll nil)

;; --------------------------------------------------------------------------------
;; Fix the minibuffer gc triggering
;; --------------------------------------------------------------------------------

(defun haba/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun haba/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'haba/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'haba/minibuffer-exit-hook)


;; For OSX emacs fullscreen is OK, Windows on the other hand flicks and flocks on startup.
;; For Windows and Linux use "-geometry 120x40" (example) to set initial size.
(if *is-osx*
    (setq default-frame-alist '((fullscreen . maximized)
                                (vertical-scroll-bars . nil)))
  (setq default-frame-alist '((fullscreen . nil)
                              (vertical-scroll-bars . nil))))
(setq frame-title-format "%b")

;; ================================================================================
;; Set up packaging system
;; ================================================================================
(let ((package-protocol (if *is-windows* "http://" "https://")))
  (setq package-archives `(("elpa" . ,(concat package-protocol "elpa.gnu.org/packages/"))
                           ("melpa" . ,(concat package-protocol "melpa.org/packages/"))
                           ("SC"   . "http://joseito.republika.pl/sunrise-commander/"))))

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

;; ================================================================================
;; My local packages
;; ================================================================================
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
         ("M-SPC" . haba/just-one-space)
         ("M-2" . haba/duplicate-line)
         ("C-M-2" . haba/duplicate-line-inc-numbers)
         ("M-<up>" . haba/move-line-up)
         ("M-<down>" . haba/move-line-down)
         ("M-9" . haba/previous-buffer-like-this)
         ("M-0" . haba/next-buffer-like-this)
         ("C-M-9" . previous-buffer)
         ("C-M-0" . next-buffer)
         ("C-c o i" . haba/open-init-file)
         ("C-c o s" . haba/open-scratch-buffer)
         ("C-c o t" . haba/open-todo-file)
         ([remap fill-paragraph] . haba/fill-or-unfill)
         ("C-c i d" . haba/insert-current-date)
         ("<C-wheel-up>" . text-scale-increase)
         ("<C-wheel-down>" . text-scale-decrease)
         ("<f10>" . menu-bar-mode)
         ("C-x C-b" . ibuffer)
         ("C-<tab>" . haba/other-frame)
         ("C-S-<tab>" . haba/make-new-frame))
  :config
  (defun disable-all-themes (&rest args)
    (mapcar #'disable-theme custom-enabled-themes))

  (advice-add 'load-theme :before #'disable-all-themes)

  (defun haba/open-scratch-buffer ()
    "Open scratch buffer"
    (interactive)
    (switch-to-buffer "*scratch*"))

  (defun haba/make-new-frame ()
    (interactive)
    (make-frame `((width . ,(frame-width)) (height . ,(frame-height)))))

  (defun haba/other-frame ()
    (interactive)
    (if (eql (length (frame-list)) 1)
        (haba/make-new-frame)
      (other-frame 1)))

  (defun haba/open-todo-file ()
    "Open todo.adoc file"
    (interactive)
    (find-file "~/docs/todo.adoc")))

(use-package habamax-theme
  :load-path "habamax-theme/"
  :config
  (load-theme 'habamax t))

(use-package kosmos-theme :defer
  :load-path "habamax-theme/")

(use-package asciidoctor-mode
  :ensure nil
  :load-path "lisp/"
  :mode ("\\.\\(adoc\\|asciidoc\\)$" . asciidoctor-mode)
  :hook ((asciidoctor-mode . goto-address-mode))
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
  (when *is-osx*
    (setq asciidoctor-clipboard-backend "pngpaste %s%s")))


;; ================================================================================
;; Melpa packages
;; ================================================================================

(use-package leuven-theme :defer)
(use-package cyberpunk-theme :defer)


(use-package diminish
  :defer
  :diminish abbrev-mode auto-revert-mode subword-mode)

;; PATH for OSX
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  :if *is-osx*)

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
  :bind (("C-l" . avy-goto-word-or-subword-1)))

(use-package ace-window
  :bind (("C-x o" . ace-window))
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

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
;;   (use-package ido-completing-read+ :config (ido-ubiquitous-mode 1))
;;   (use-package flx-ido
;;     :config
;;     (setq ido-enable-flex-matching t)
;;     (setq ido-use-faces nil)
;;     (flx-ido-mode 1))
;;   ;; (use-package ido-vertical-mode
;;   ;;   :config
;;   ;;   (ido-vertical-mode 1)
;;   ;;   (setq ido-vertical-show-count t)
;;   ;;   (setq ido-vertical-define-keys 'C-n-and-C-p-only))
;;   )


(use-package whitespace-cleanup-mode
  :defer 3
  :diminish
  :commands (whitespace-cleanup-mode)
  :config
  (global-whitespace-cleanup-mode))

(use-package ivy
  ;; :defer 2
  :diminish ivy-mode
  :init
  ;; clear default ^ for counsel-M-x and friends
  (setq ivy-initial-inputs-alist nil)
  :config
  (use-package ivy-hydra :defer)
  (use-package flx :defer)
  (setq ivy-ignore-buffers '(".*-autoloads.el"))
  (setq ivy-switch-buffer-faces-alist '((dired-mode . ivy-subdir)))
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-rich-switch-buffer-align-virtual-buffer t)
  (setq ivy-rich-switch-buffer-name-max-length 40)
  (setq ivy-rich-path-style 'abbrev))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-s g" . counsel-git)
         ("M-s s" . counsel-grep-or-swiper)
         ("M-s r" . counsel-rg)
         ("C-M-y" . counsel-yank-pop)
         ("C-h a" . counsel-apropos)
         ("C-x 8 RET" . counsel-unicode-char)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :diminish counsel-mode
  :config
  (use-package swiper
    :bind (:map swiper-map
                ("M-c" . haba/swiper-mc-fixed))
    :init
    (bind-key "M-s" #'swiper-from-isearch isearch-mode-map)
    :config
    (defun haba/swiper-mc-fixed()
      (interactive)
      (setq swiper--current-window-start nil)
      (swiper-mc)))

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

  (setq counsel-git-cmd "rg --files")
  (setq counsel-rg-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s .")

  (setq counsel-yank-pop-separator (concat "\n" (make-string 70 ?-) "\n"))

  (counsel-set-async-exit-code 'haba/counsel-projectile-rg-todo 1 "No matches found")
  (counsel-mode 1))

;; (use-package counsel-projectile
;;   :bind (("C-c p SPC" . counsel-projectile)
;;          ("C-c p p" . counsel-projectile-switch-project)
;;          ("C-c p f" . counsel-projectile-find-file))
;;   :config
;;   (counsel-projectile-mode))

;; (use-package projectile :defer :diminish projectile-mode)

;; counsel uses smex for better sorting
;; (use-package smex :after counsel)
(use-package amx :after counsel)

;; (use-package telephone-line
;;   :config
;;   (setq telephone-line-primary-left-separator 'telephone-line-flat
;;         telephone-line-secondary-left-separator 'telephone-line-nil
;;         telephone-line-primary-right-separator 'telephone-line-flat
;;         telephone-line-secondary-right-separator 'telephone-line-nil)

;;   (setq telephone-line-height 22)
;;   (telephone-line-mode 1))



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
  (setq company-idle-delay 0)

  (setq company-minimum-prefix-length 5)

  (company-tng-configure-default)
  (global-company-mode))


;; (use-package auto-complete
;;   :defer 2
;;   :bind (("C-M-i" . auto-complete))
;;   :config
;;   (use-package fuzzy :defer)
;;   (setq ac-use-fuzzy t)
;;   (ac-config-default))


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
(use-package rainbow-mode :hook ((css-mode html-mode) . rainbow-mode))
(use-package kurecolor
  :bind (("<f7>" . kurecolor-increase-hue-by-step)
         ("M-<f7>" . kurecolor-decrease-hue-by-step)
         ("<f8>" . kurecolor-increase-saturation-by-step)
         ("M-<f8>" . kurecolor-decrease-saturation-by-step)
         ("<f9>" . kurecolor-increase-brightness-by-step)
         ("M-<f9>" . kurecolor-decrease-brightness-by-step)))

;; (use-package page-break-lines :diminish :hook (prog-mode . turn-on-page-break-lines-mode))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode lisp-mode) . aggressive-indent-mode))


(use-package smartparens
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :bind (:map smartparens-mode-map
              ("C-M-k" . sp-kill-hybrid-sexp)
              ("C-M-;" . sp-comment)
              ("C-M-j" . sp-join-sexp)
              ("C-M-<up>" . sp-raise-sexp)
              :map lisp-interaction-mode-map
              ("M-2" . sp-clone-sexp)
              ("M-t" . sp-transpose-sexp)
              :map emacs-lisp-mode-map
              ("M-2" . sp-clone-sexp)
              ("M-t" . sp-transpose-sexp)
              :map lisp-mode-map
              ("M-2" . sp-clone-sexp)
              ("M-t" . sp-transpose-sexp))
  ;; sp-emit-sexp
  ;; sp-split-sexp
  ;; sp-absorb-sexp
  ;; sp-splice-sexp (remove brackets M-Backspace)
  ;; sp-cheat-sheet (very good)
  ;; sp-next-sexp (C-M-n)
  ;; sp-convolute-sexp
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


(use-package ztree :defer)

(use-package string-edit :commands string-edit)

(use-package string-inflection :bind (("C-c C-u" . string-inflection-toggle)))

;; yasnippets
(use-package yasnippet
  :defer 3
  :diminish yas-minor-mode
  :bind (("C-c i s" . yas-insert-snippet)
         :map yas-minor-mode-map
         ("C-c y" . yas-expand))
  :config

  (use-package yasnippet-snippets :defer)
  (yas-global-mode t))


(use-package python-mode
  :mode ("\\.\\(py\\)$" . python-mode)
  :config
  ;; (setq python-shell-interpreter "python3")
  )

(use-package haskell-mode
  :mode ("\\.\\(hs\\)$" . haskell-mode))

(use-package intero
  :hook (haskell-mode . intero-mode))

(use-package clojure-mode
  :mode ("\\.\\(clj\\)$" . clojure-mode)
  :config
  (use-package cider
    :config
    (setq cider-repl-display-help-banner t)
    (setq cider-repl-use-pretty-printing t)))

(use-package slime
  :commands (slime)
  :config
  ;; (setq inferior-lisp-program "sbcl")
  (load (expand-file-name "~/.roswell/helper.el"))
  (setq inferior-lisp-program "ros -Q run")
  (slime-setup '(slime-fancy)))

;; new golden-ratio
(use-package zoom
  :diminish zoom-mode
  :defer 3
  :config
  (setq zoom-size '(90 . 30))
  (zoom-mode))


(use-package restclient
  :mode ("\\.\\(rest\\)$" . restclient-mode)
  :commands (restclient-mode))

(use-package csv-mode
  :mode ("\\.\\(csv\\)$" . csv-mode)
  :init
  (setq csv-separators '("," ";" "|"))
  (setq csv-header-lines 1))


(use-package nov
  :mode ("\\.\\(epub\\)$" . nov-mode))

(use-package htmlize
  :defer
  :commands (htmlize-buffer htmlize-region))


(use-package webpaste
  :commands (webpaste-paste-buffer webpaste-paste-region))

;; ================================================================================
;; Built-in packages
;; ================================================================================

;; restore layout
(use-package winner
  :ensure nil
  :defer 3
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package bookmark
  :ensure nil
  :bind (("<f5>" . bookmark-jump)))

;; dired extra stuff
(use-package dired
  :ensure nil
  :defer t
  :commands (dired dired-jump dired-jump-other-window)
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)
         :map dired-mode-map
         ("b" . bookmark-jump)
         ("j" . dired-up-directory)
         ("J" . dired-goto-file)
         ("<C-return>" . haba/dired-open-in-os))
  :init
  ;; by default hide all details
  ;; you can toggle details using `('
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  :config
  ;; dired user another dired buffer as destination for copy/move
  (setq dired-dwim-target t)

  ;; directories first?
  ;; OSX has ls but it is not gnu compatible and can't group directories first
  ;; Load ls-lisp to use built in ls implemented in lisp.
  ;; For windows it is preloaded automatically.
  ;; PS: it is possible to install coreutils with GNU ls...
  (when *is-osx*
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))
  (setq ls-lisp-dirs-first t)

  (defun haba/dired-open-in-os ()
    "Open file/folder under cursor in OS."
    (interactive)
    (let ((file (ignore-errors (dired-get-file-for-visit))))
      (browse-url (file-truename file)))))

(use-package dired-x
  :ensure nil
  :after dired)

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle)))

(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
              ("M-w" . dired-ranger-copy)
              ("C-y" . dired-ranger-paste)
              ("C-c C-y" . dired-ranger-move)))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

(use-package diredfl
  :after dired
  :config
  (diredfl-global-mode))


;; <CR> to open file under cursor in OS
;; (use-package dired-open
;;   :after dired
;;   :config
;;   (defun haba/dired-open-in-os ()
;;     "Open file/folder under cursor in OS."
;;     (interactive)
;;     (let ((file (ignore-errors (dired-get-file-for-visit))))
;;       (browse-url (file-truename file))))
;;   (setq dired-open-functions '(haba/dired-open-in-os))
;;   ;; (setq dired-open-use-nohup nil))
;;   )
;; dired-open
;; dired-filter


;; (use-package dired-collapse
;;   :after dired
;;   :init
;;   (add-hook 'dired-mode-hook 'dired-collapse-mode))

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
             (haba/eshell-here eshell-name))))))

;; (use-package sunrise-commander
;;   :commands sunrise
;;   :bind (("<f10>" . sunrise))
;;   :config
;;   ;; dirty hack -- otherwise opening smth from sunrise-commander with OS handling doesn't workd
;;   ;; ERROR feature browse-url not available!
;;   (ignore-errors (browse-url))
;;   (setq sr-cursor-follows-mouse nil)
;;   (define-key sr-mode-map [mouse-1] nil)
;;   (define-key sr-mode-map [mouse-movement] nil)
;;   (add-to-list 'savehist-additional-variables 'sr-history-registry)
;;   (setq sr-attributes-display-mask '(nil nil nil nil t t t))
;;   (setq sr-show-file-attributes nil))

;; (use-package sunrise-x-checkpoints
;;   :after sunrise-commander)

;; (use-package sunrise-x-w32-addons
;;   :if (windows?)
;;   :after sunrise-commander)

;; (use-package sunrise-x-loop
;;   :after sunrise-commander)


(use-package outline
  :bind (("M-o o" . outline-toggle-children)
         ("M-o h" . outline-hide-body)
         ("M-o a" . outline-show-all))
  :diminish outline-mode outline-minor-mode
  :init
  (global-set-key (kbd "M-o") nil)
  :config
  (outline-minor-mode 1))



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
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  ;; (setq calendar-today-marker "T")
  ;; Calendar -- говорим и показываем по русски.
  (setq calendar-date-style 'iso)
  (setq calendar-week-start-day 1)
  (setq calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"])
  (setq calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель"
                                   "Май" "Июнь" "Июль" "Август"
                                   "Сентябрь" "Октябрь" "Ноябрь" "Декабрь"]))



;; ================================================================================
;; show emacs startup time
;; ================================================================================
(defun display-startup-echo-area-message ()
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading Emacs configuration... DONE (%.3fs),  overral emacs-init-time: %s" elapsed (emacs-init-time))))

;;; init.el ends here
