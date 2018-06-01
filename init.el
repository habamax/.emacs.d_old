;;; init.el -- Emacs initialization file
;;; Maxim Kim <habamax@gmail.com>

;;; Commentary:
;; Carefully crafted settings for alien's editor I become used to.

;;; Code:

;; Start measuring loading time
(defconst +EMACS-START-TIME+ (current-time))

(defconst +IS-OSX+ (eq system-type 'darwin))
(defconst +IS-WINDOWS+ (eq system-type 'windows-nt))

;; ================================================================================
;; Non-Package setup
;; ================================================================================

;; disable gc for init
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))


;; General OSX setup
(when +IS-OSX+
  ;; No new frames for files that are opened from OSX
  (setq ns-pop-up-frames nil)
  ;; command to meta, option to control
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'control))

;; (when (not +IS-OSX+)
;;   (menu-bar-mode -1))

;; Show menu by default
(menu-bar-mode 1)

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
(when +IS-WINDOWS+
  (setq default-process-coding-system '(utf-8-dos . cp1251-dos)))

;; make unix lineendings default
(setq default-buffer-file-coding-system 'utf-8-unix)

;; scroll to the top or bottom with C-v and M-v
(setq scroll-error-top-bottom t)

;; single line horizontal scroll
(setq auto-hscroll-mode 'current-line)

;; not so jumpy mouse scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

;; M-a and M-e use punct and single space as sentence delimiter
(setq sentence-end-double-space nil)

(delete-selection-mode 1)
(setq ring-bell-function #'ignore)
(setq disabled-command-function nil)
(setq suggest-key-bindings t)

(electric-indent-mode t)
(electric-layout-mode t)
(electric-quote-mode t)
(setq electric-quote-context-sensitive t)

(global-subword-mode t)
(show-paren-mode t)
(column-number-mode t)
(global-display-line-numbers-mode t)
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
;; (windmove-default-keybindings)

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
(if +IS-OSX+
    (setq default-frame-alist '((fullscreen . maximized)
                                (vertical-scroll-bars . nil)))
  (setq default-frame-alist '((fullscreen . nil)
                              (vertical-scroll-bars . nil))))
(setq frame-title-format "%b")

;; ================================================================================
;; Set up packaging system
;; ================================================================================
(let ((package-protocol (if +IS-WINDOWS+ "http://" "https://")))
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

;; ================================================================================
;; My local packages
;; ================================================================================
(use-package haba-stuff
  :defer t
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
         ("<f1>" . haba/previous-buffer-like-this)
         ("<f2>" . haba/next-buffer-like-this)
         ("M-<f1>" . previous-buffer)
         ("M-<f2>" . next-buffer)
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
    (let* ((fullscreen (frame-parameter nil 'fullscreen))
           (geom-width (cdr (assoc 'width frame-initial-frame-alist))) ; from -geometry parameter
           (geom-height (cdr (assoc 'height frame-initial-frame-alist))) ; from -geometry parameter
           (width (if fullscreen geom-width (frame-width)))
           (height (if fullscreen geom-height (frame-height))))
      (make-frame `((width . ,width)
                    (height . ,height)
                    (fullscreen . ,fullscreen)))))

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
  (setq habamax-theme-variable-heading-heights t)
  (load-theme 'habamax t))

(use-package kosmos-theme :defer
  :ensure t
  :load-path "kosmos-theme/")

(use-package asciidoctor-mode
  :ensure nil
  :load-path "lisp/"
  :mode "\\.\\(adoc\\|asciidoc\\)$"
  :hook ((asciidoctor-mode . goto-address-mode))
  :bind (:map asciidoctor-mode-map
              ("C-c C-o C-o" . browse-url-of-buffer)
              ("C-c C-h C-h" . asciidoctor-compile-html)
              ("C-c C-p C-p" . asciidoctor-compile-pdf)
              ("C-c C-d C-d" . asciidoctor-compile-docx)
              ("C-c C-h C-o" . asciidoctor-open-html)
              ("C-c C-p C-o" . asciidoctor-open-pdf)
              ("C-c C-d C-o" . asciidoctor-open-docx)
              ("C-c C-y" . asciidoctor-save-image-insert-link))
  :config
  ;; (setq asciidoctor-pdf-executable (concat "ruby " (expand-file-name "~/projects/asciidoctor-pdf/bin/asciidoctor-pdf")))
  (setq asciidoctor-pandoc-data-dir "~/docs/templates/")
  (setq asciidoctor-pdf-stylesdir "~/docs/AsciiDocThemes")
  (setq asciidoctor-pdf-fontsdir "~/docs/AsciiDocThemes/fonts")
  (setq asciidoctor-pdf-extensions '("asciidoctor-diagram"))
  (setq asciidoctor-extensions '("asciidoctor-diagram" "asciidoctor-rouge"))
  (when +IS-OSX+
    (setq asciidoctor-clipboard-backend "pngpaste %s%s")))


;; ================================================================================
;; Melpa packages
;; ================================================================================

;; (use-package leuven-theme :defer)
(use-package cyberpunk-theme :defer)
(use-package zenburn-theme :defer)


(use-package diminish
  :defer
  :diminish abbrev-mode auto-revert-mode subword-mode)

;; PATH for OSX
(use-package exec-path-from-shell
  :if +IS-OSX+
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; ;; sometimes text mangling is just too cumbersome with emacs bindings...
;; ;; be EVIL then and SHOW THEM ALL!!!
;; (use-package evil
;;   :bind (("C-c e" . haba/become-evil-or-not)
;;          :map evil-emacs-state-map
;;          ([escape] . evil-normal-state)
;;          ("ESC" . evil-normal-state)
;;          :map evil-visual-state-map
;;          ([escape] . evil-normal-state)
;;          ("ESC" . evil-normal-state))
;;   :commands (evil-mode)
;;   :config
;;   (defalias 'evil-insert-state 'evil-emacs-state)

;;   (defun haba/become-evil-or-not ()
;;     (interactive)
;;     (if (bound-and-true-p evil-state)
;;         (progn
;;           (evil-mode -1)
;;           (message "Calm down. Don't be evil, be nice..."))
;;       (progn
;;         (evil-mode)
;;         (message "I!     AM!     EVIL!!!!")))))

;; (use-package evil-commentary
;;   :after evil
;;   :config
;;   (evil-commentary-mode))

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
  :bind (("C-l" . avy-goto-word-or-subword-1)
         ("C-M-l" . avy-goto-char)))

(use-package ace-window
  :bind (("C-x o" . ace-window))
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package rotate
  :bind (("<f5>" . rotate-layout)
         ("M-<f5>" . rotate-window))
  :config
  (setq rotate-functions '(rotate:main-horizontal
                           rotate:main-vertical
                           rotate:tiled))

  ;; redefine tiled functions
  ;; prefer 3 columns when there are more than 4 windows (was 6)
  (defun rotate:tiled-n (num)
    (cond
     ((<= num 2)
      (split-window-vertically))
     ((<= num 4)
      (rotate:tiled-2column num))
     (t
      (rotate:tiled-3column num))))

  ;; if there is non even windows make bigger top left one
  ;; was the last bottom right
  (defun rotate:tiled-3column (num)
    (rotate:vertically-n (/ (+ num 2) 3))
    (dotimes (i (/ (+ num 1) 3))
      (rotate:horizontally-n 3)
      (other-window 3))
    (when (= (% num 3) 2)
      (other-window 1)
      (delete-window)))

  ;; make it even for 2 windows
  (defun rotate:main-horizontally-n (num)
    (if (<= num 2)
        (split-window-horizontally)
      (split-window-vertically)
      (other-window 1)
      (rotate:horizontally-n (- num 1))))

  ;; make it even for 2 windows
  (defun rotate:main-vertically-n (num)
    (if (<= num 2)
        (split-window-vertically)
      (split-window-horizontally)
      (other-window 1)
      (rotate:vertically-n (- num 1)))))


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


(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-s g" . counsel-git)
         ("C-s" . counsel-grep-or-swiper)
         ("M-s r" . counsel-rg)
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
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s %s")

  ;; (setq counsel-yank-pop-separator (concat "\n" (make-string 70 ?-) "\n"))

  (counsel-mode 1))

(use-package counsel-projectile
  :bind (("C-c p SPC" . counsel-projectile)
         ("C-c p p" . counsel-projectile-switch-project)
         ("C-c p f" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

(use-package projectile
  :defer
  :diminish projectile-mode
  :bind (("C-c p v" . projectile-vc))
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)

  (when (executable-find "rg")
    (setq projectile-indexing-method 'alien)

    (defun projectile-get-ext-command ()
      "Always use `rg' for getting a list of all files in the project."
      "rg --line-number --smart-case --follow --mmap --null --files")))

;; counsel uses smex for better sorting
(use-package smex :after counsel)
;; (use-package amx :after counsel)

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
  :bind (("C-M-," . goto-last-change)
         ("C-M-." . goto-last-change-reverse)))

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

;; doesn’t play well with company-childframe
;; (use-package company-quickhelp
;;   :after company
;;   :config
;;   (company-quickhelp-mode))

(use-package company-childframe
  :after company
  :config
  (company-childframe-mode 1))


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
;; (use-package kurecolor
;;   :bind (("<f7>" . kurecolor-increase-hue-by-step)
;;          ("M-<f7>" . kurecolor-decrease-hue-by-step)
;;          ("<f8>" . kurecolor-increase-saturation-by-step)
;;          ("M-<f8>" . kurecolor-decrease-saturation-by-step)
;;          ("<f9>" . kurecolor-increase-brightness-by-step)
;;          ("M-<f9>" . kurecolor-decrease-brightness-by-step)))

;; (use-package page-break-lines :diminish :hook (prog-mode . turn-on-page-break-lines-mode))

(use-package aggressive-indent
  :diminish
  :hook ((emacs-lisp-mode lisp-mode) . aggressive-indent-mode))

(use-package hl-lisp-globals-mode
  :ensure nil
  :diminish
  :load-path "lisp"
  :hook ((emacs-lisp-mode lisp-mode) . hl-lisp-globals-mode))


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
  :mode "\\.\\(markdown\\|md\\)$"
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package plantuml-mode
  :mode "\\.uml$")

;; (use-package go-mode
;;   :mode ("\\.\\(go\\)$" . go-mode))

(use-package web-mode
  :mode "\\.\\(html\\|css\\)$"
  :config
  (setq web-mode-enable-auto-expanding t
        web-mode-enable-auto-pairing t))

(use-package elixir-mode
  :mode "\\.\\(ex\\|exs\\)$")

(use-package alchemist
  :after elixir-mode)


(use-package yaml-mode
  :mode "\\.yml$")

(use-package emmet-mode
  :defer
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode))

(use-package groovy-mode
  :mode "\\.groovy$")

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


(use-package smartparens
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode)
  :bind (:map smartparens-mode-map
              ("C-M-k" . sp-kill-hybrid-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-a" . sp-backward-down-sexp)
              ("C-M-e" . sp-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("M-<backspace>" . sp-unwrap-sexp)
              ("C-S-<backspace>" . sp-splice-sexp-killing-forward)
              ("C-M-<backspace>" . sp-splice-sexp-killing-around)
              ("C-9" . sp-backward-slurp-sexp)
              ("C-0" . sp-forward-slurp-sexp)
              ("M-9" . sp-backward-barf-sexp)
              ("M-0" . sp-forward-barf-sexp)
              ("C-M-SPC" . sp-mark-sexp)
              :map lisp-interaction-mode-map
              ("M-2" . sp-clone-sexp)
              ("M-e" . sp-end-of-sexp)
              ("M-a" . sp-beginning-of-sexp)
              :map emacs-lisp-mode-map
              ("M-2" . sp-clone-sexp)
              ("M-e" . sp-end-of-sexp)
              ("M-a" . sp-beginning-of-sexp)
              :map lisp-mode-map
              ("M-2" . sp-clone-sexp)
              ("M-e" . sp-end-of-sexp)
              ("M-a" . sp-beginning-of-sexp))
  ;; add more "standard bindings"
  ;; and maybe the following
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
  ;;(setq sp-base-key-bindings 'sp)
  ;;(sp-use-smartparens-bindings)
  (smartparens-global-mode)
  (show-smartparens-global-mode)

  ;; wrap selection with a symbols
  ;; (sp-pair "*" "*" :actions '(wrap))
  ;; (sp-pair "_" "_" :actions '(wrap))
  ;; (sp-pair "=" "=" :actions '(wrap))
  ;; (sp-pair "+" "+" :actions '(wrap))
  ;; (sp-pair "/" "/" :actions '(wrap))
  ;; (sp-pair "$" "$" :actions '(wrap))
  ;; (sp-pair "-" "-" :actions '(wrap))
  ;; (sp-pair "«" "»" :actions '(wrap))
  ;; (sp-pair "“" "”" :actions '(wrap))
  ;; (sp-pair "´" "´" :actions '(wrap))
  ;; (sp-pair "`" "`" :actions '(wrap))
  ;; (sp-pair "'" "'" :actions '(wrap))
  )


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
  :mode "\\.py$"
  :config
  ;; (setq python-shell-interpreter "python3")
  )

;; (use-package haskell-mode
;;   :mode ("\\.\\(hs\\)$" . haskell-mode))

;; (use-package intero
;;   :hook (haskell-mode . intero-mode))

(use-package rust-mode
  :mode "\\.rs$")

(use-package toml-mode
  :mode "\\.toml$")

(use-package clojure-mode
  :mode "\\.clj$"
  :config
  (use-package cider
    :config
    (setq cider-repl-display-help-banner t)
    (setq cider-repl-use-pretty-printing t)))

(use-package slime
  :commands (slime)
  :config
  (setq inferior-lisp-program "sbcl --noinform")
  ;; (setq inferior-lisp-program "ros -Q run")

  (setq common-lisp-hyperspec-root (expand-file-name "~/quicklisp/hyperspec/HyperSpec/"))

  (slime-setup '(; this is slime-fancy without slime-editing-commands and slime-c-p-c
                 slime-repl
                 slime-autodoc
                 slime-fancy-inspector
                 slime-fancy-trace
                 slime-fuzzy
                 slime-mdot-fu
                 slime-macrostep
                 slime-presentations
                 slime-scratch
                 slime-references
                 slime-package-fu
                 slime-fontifying-fu
                 slime-trace-dialog
                 ;; and this are other addons
                 slime-asdf
                 slime-quicklisp
                 slime-indentation slime-banner slime-company)))


(use-package slime-company
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy))

;; new golden-ratio
;; (use-package zoom
;;   :diminish zoom-mode
;;   :defer 3
;;   :config
;;   (setq zoom-size '(90 . 33))
;;   (zoom-mode))


(use-package restclient
  :mode ("\\.rest$" . restclient-mode)
  :commands (restclient-mode))

(use-package csv-mode
  :mode "\\.csv$"
  :init
  (setq csv-separators '("," ";" "|"))
  (setq csv-header-lines 1))


(use-package nov
  :mode ("\\.epub$" . nov-mode))

(use-package htmlize
  :defer
  :commands (htmlize-buffer htmlize-region))


(use-package webpaste
  :commands (webpaste-paste-buffer webpaste-paste-region))


(use-package define-word
  :bind (("C-c d" . define-word-at-point))
  :commands (define-word define-word-at-point))


(use-package powershell
  :if +IS-WINDOWS+
  :commands (powershell))



;; ================================================================================
;; Built-in packages
;; ================================================================================


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
          (insert (concat "ls"))
          (eshell-send-input)))))

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

;; restore layout
(use-package winner
  :ensure nil
  :defer 3
  :bind (("<f7>" . winner-redo)
         ("<f6>" . winner-undo))
  :config
  (winner-mode 1))

;; (use-package bookmark
;;   :ensure nil
;;   :bind (("M-g b" . bookmark-jump)))

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
         ("TAB" . haba/dired-tab)
         ("<mouse-2>" . haba/dired-mouse-find-file))
  :init
  ;; by default hide all details
  ;; you can toggle details using `('
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  ;; C-x M-o to toggle omit-mode
  ;; Let it be off by default for now
  ;; (add-hook 'dired-mode-hook 'dired-omit-mode)

  :config
  ;; dired user another dired buffer as destination for copy/move
  (setq dired-dwim-target t)

  ;; directories first?
  ;; OSX has ls but it is not gnu compatible and can't group directories first
  ;; Load ls-lisp to use built in ls implemented in lisp.
  ;; For windows it is preloaded automatically.
  ;; PS: it is possible to install coreutils with GNU ls...
  (when +IS-OSX+
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))
  (setq ls-lisp-dirs-first t)

  (defun haba/dired-mouse-find-file (event)
    "In Dired, visit the file or directory name you click on in the same window."
    (interactive "e")
    (let (window pos file)
      (save-excursion
        (setq window (posn-window (event-end event))
              pos (posn-point (event-end event)))
        (if (not (windowp window))
            (error "No file chosen"))
        (set-buffer (window-buffer window))
        (goto-char pos)
        (setq file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (or (and (cdr dired-subdir-alist)
                   (dired-goto-subdir file))
              (progn
                (select-window window)
                (dired-find-file)))
        (select-window window)
        (find-file (file-name-sans-versions file t)))))

  ;; need for -select function
  (require 'dash)

  (defun haba/dired-buffer-p (w)
    "Check if window has dired buffer."
    (with-current-buffer (window-buffer w)
      (when (eql major-mode 'dired-mode)
        (window-buffer w))))

  (defun haba/dired-tab ()
    "Create a new dired window if there is only one visible or switch to the next visible dired window.

 New dired window visits directory under cursor of previous dired(?)."
    (interactive)
    (let* ((dired-windows (-select #'haba/dired-buffer-p (window-list)))
           (dired-selected-file (ignore-errors (dired-get-filename)))
           (new-path (if (and dired-selected-file (file-directory-p dired-selected-file)) dired-selected-file (dired-current-directory))))
      (if (> (length dired-windows) 1)
          (select-window (cadr dired-windows))
        (dired-other-window new-path))))

  ;; Probably should be extracted to some package?
  (defvar dired-mouse-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-2] 'dired-follow-link)
      (define-key map [return] 'dired-follow-link)
      (define-key map [follow-link] 'mouse-face)
      map)
    "Keymap for mouse when in `dired-mode'.")

  (defun dired-follow-link (event)
    "Follow the link in the dired directory heading, causing a new
dired buffer to be opened."
    (interactive (list last-nonmenu-event))
    (run-hooks 'mouse-leave-buffer-hook)
    (with-current-buffer (window-buffer (posn-window (event-start event)))
      (let ((path (get-text-property (posn-point (event-start event)) 'breadcrumb)))
        (dired path))))

  (defun dired-propertize-directory-heading ()
    (interactive)
    (unless (buffer-narrowed-p)
      (let* (
             p beg end path peol
             (inhibit-read-only t) )
        (save-excursion
          (goto-char (point-min))
          (setq peol (point-at-eol))
          (set-text-properties (point) peol nil)
          (re-search-forward "\\([^/\\]+\\)[/\\]" peol t)
          (when (looking-back "\\(^ +\\)\\([a-zA-Z]:\\)?/")
            (setq p (match-end 1))
            (setq path (if (match-string 2) (concat (match-string 2) "/") "/"))
            (add-text-properties
             (point-min)
             (1- (match-end 0))
             (list 'breadcrumb path
                   'mouse-face 'highlight
                   'help-echo (format "mouse-2, RET: Follow the link to \"%s\"." path)
                   'keymap 'dired-mouse-map)))
          (while (re-search-forward "\\([^/\\]+\\)[/\\]" peol t)
            (setq beg (match-beginning 1))
            (setq end (match-end 1))
            (setq path (buffer-substring-no-properties p end))
            (add-text-properties beg end (list
                                          'breadcrumb path
                                          'mouse-face 'highlight
                                          'help-echo (format "mouse-2, RET: Follow the link to \"%s\"." path)
                                          'keymap dired-mouse-map)))
          (setq path (buffer-substring-no-properties p (1- peol)))
          (add-text-properties (point) (1- peol) (list
                                                  'breadcrumb path
                                                  'mouse-face 'highlight
                                                  'help-echo (format "mouse-2, RET: Follow the link to \"%s\"." path)
                                                  'keymap dired-mouse-map))))))

  (add-hook 'dired-after-readin-hook 'dired-propertize-directory-heading)

  )

(use-package dired-x
  :ensure nil
  :after dired)

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


;; dired-filter


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
                 :channels ("#emacs" "#lor" "#lisp")))


  (defun haba/rcirc-mode-setup ()
    "Sets things up for channel and query buffers spawned by rcirc."
    ;; rcirc-omit-mode always *toggles*, so we first 'disable' it
    ;; and then let the function toggle it *and* set things up.
    (setq rcirc-omit-mode nil)
    (rcirc-omit-mode))

  (add-hook 'rcirc-mode-hook 'haba/rcirc-mode-setup))

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
                                            +EMACS-START-TIME+))))
    (message "Loading Emacs configuration... DONE (%.3fs),  overral emacs-init-time: %s" elapsed (emacs-init-time))))

;;; init.el ends here
