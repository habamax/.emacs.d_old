;;; habamax-theme.el --- Boring white background color that gets the job done.

;; Author: Maxim Kim <habamax@gmail.com>
;; Url: https://github.com/habamax/habamax-theme
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;;; TODO:
;; magit highlight
;; diff mode colors refinements
;; isearch?

;;; Code:

(deftheme habamax "Boring white. Gets the job done.")

(defcustom habamax-theme-variable-heading-heights nil
  "Use different heights for headings for Asciidoctor, Org or Markdown."
  :type 'boolean)

(let
    ((color-fg "#000000")
     (color-bg "#ffffff")
     (color-dim-bg "#f5f9fe")
     (color-dim-fg "#204060")
     (color-keyword "#0000ff")
     (color-keyword-dim "#006363")
     (color-str "#811111")
     (color-comment "#808080")
     (color-gray "#7a7a7a")
     (color-title "#000000")
     (color-heading "#000000")
     (color-url "#3737D6")
     (color-url-visited "#806088")
     (color-bg-highlight "#ececef")
     (color-bg-highlight-2 "#cadfca")
     (height-1 (if habamax-theme-variable-heading-heights 1.6 1))
     (height-2 (if habamax-theme-variable-heading-heights 1.4 1))
     (height-3 (if habamax-theme-variable-heading-heights 1.2 1))
     (height-4 (if habamax-theme-variable-heading-heights 1.1 1))
     (height-5 (if habamax-theme-variable-heading-heights 1.1 1))
     (height-6 (if habamax-theme-variable-heading-heights 1.1 1)))

  (custom-theme-set-faces
   'habamax

;;; standard faces
   `(default ((t (:background ,color-bg :foreground ,color-fg))))
   '(cursor ((nil (:background "#000000"))))
   '(region ((t (:background "#d0e0f0"))))
   `(highlight ((nil (:background ,color-bg-highlight))))
   `(minibuffer-prompt ((t (:foreground ,color-dim-fg :background ,color-dim-bg :weight bold))))
   ;; '(widget-field-face ((t (:background "#a0a0a0" :foreground "#000000"))))
   `(header-line ((t (:foreground ,color-fg :background "#e9e590" :box (:line-width 1 :color "#a9a550")))))

   ;; Default isearch is OK for now.
   ;; `(isearch ((t (:background ,color-search-bg :foreground ,color-search-fg))))
   ;; `(lazy-highlight ((t (:background ,color-lazysearch-bg :foreground ,color-lazysearch-fg))))
   ;; match?
   ;; `(isearch-fail ((t (:background ,color-bg-search-fail :foreground ,color-fg-search-fail :weight bold :underline (:color ,color-fg-search-fail)))))


   ;; frame UI
   '(mode-line ((t (:background "#b5d5f5" :foreground "#406582" :box (:line-width 1 :color "#77B3E5")))))
   `(mode-line-highlight ((t (:foreground ,color-keyword))))
   '(mode-line-buffer-id ((t (:foreground "#000000" :weight bold))))
   '(mode-line-inactive ((t (:background "#e0e5e2" :foreground "#505552" :box (:line-width 1 :color "#CECECE")))))
   `(vertical-border ((nil (:foreground ,color-gray))))
   `(fringe ((nil (:background ,color-bg))))

   ;; powerline default theme
   `(powerline-active1 ((t (:foreground "#406582" :background "#95b5c5"))))
   `(powerline-active2 ((t (:foreground "#b0c5e2" :background "#6585b5"))))
   `(powerline-inactive1 ((t (:foreground "#505552" :background "#c0c5c2"))))
   `(powerline-inactive2 ((t (:foreground "#505552" :background "#b0b5b2"))))

   ;; telephone-line
   `(telephone-line-accent-active ((t (:foreground "#e0e0f0" :background "#364780"))))
   `(telephone-line-accent-inactive ((t (:foreground "#505552" :background "#c0c5c2"))))

   ;; syntax font-lock I DO care about
   `(font-lock-string-face ((t (:foreground ,color-str))))
   `(font-lock-comment-face ((t (:foreground ,color-comment :slant italic))))
   `(font-lock-keyword-face ((t (:foreground ,color-keyword))))
   `(font-lock-builtin-face ((t (:foreground ,color-keyword-dim))))
   `(font-lock-function-name-face ((t (:foreground ,color-fg))))
   ;; syntax font-lock I DON'T care about
   '(font-lock-variable-name-face ((t nil)))
   '(font-lock-constant-face ((t nil)))
   '(font-lock-type-face ((t (nil))))
   ;; review this later.
   ;; `(font-lock-regexp-grouping-backslash ((t (:foreground ,color-str :weight bold))))
   ;; `(font-lock-regexp-grouping-construct ((t (:foreground ,color-str :weight bold :slant italic))))

   '(lisp-global-var-face ((t (:inherit default :weight bold))))
   '(lisp-constant-var-face ((t (:inherit lisp-global-var-face))))


   ;; parenthesis and pairs
   `(show-paren-match ((t :background "#b0f0f0")))
   ;; `(sp-show-pair-match-face ((t (:background ,color-bg-hl-parens))))

   ;; links
   `(link ((t (:foreground ,color-url :underline (:color ,color-url)))))
   `(link-visited ((t (:foreground ,color-url-visited :underline (:color ,color-url-visited)))))
   `(mouse-face ((t (:foreground ,color-url-visited :underline (:color ,color-url-visited)))))

   ;; dired
   '(dired-directory ((t (:inherit default :weight bold))))
   `(dired-mark ((t (:foreground ,color-keyword :weight bold))))
   ;; TODO: think of the "good" marked color
   ;; `(dired-marked ((t (:foreground ,color-keyword :background "#f0cccc"))))
   '(dired-header ((t (:inherit default :weight bold :box (:line-width 1 :color "#AAAAAA")))))

   ;; diredfl
   '(diredfl-dir-name ((t (:inherit dired-directory))))
   '(diredfl-dir-heading ((t (:inherit dired-header))))
   '(diredfl-file-name ((t (:inherit default))))
   '(diredfl-ignored-file-name ((t (:inherit default))))
   '(diredfl-file-suffix ((t (:foreground "#305090"))))
   '(diredfl-compressed-file-suffix ((t (:foreground "#0000FF"))))
   `(diredfl-date-time ((t (:foreground ,color-keyword-dim))))
   `(diredfl-number ((t (:foreground ,color-keyword-dim))))
   '(diredfl-dir-priv ((t (:inherit default :weight bold))))
   '(diredfl-read-priv ((t (:background "#DDFFDD" :foreground "#005000"))))
   '(diredfl-write-priv ((t (:background "#EDEDFF" :foreground "#0000AA"))))
   '(diredfl-exec-priv ((t (:background "#FFEDFFp" :foreground "#AA00AA"))))
   '(diredfl-no-priv ((t (:inherit default))))
   '(diredfl-flag-mark-line ((t (:background "#FFE0E0"))))

   ;; sunrise-commander
   '(sr-active-path-face ((t (:background "#000000" :foreground "#ffffff" :weight bold))))
   '(sr-passive-path-face ((t (:background "#e0e0e0" :foreground "#000000" :weight bold))))
   '(sr-highlight-path-face ((t (:background "#000000" :foreground "#ff7070" :weight bold))))
   '(sr-editing-path-face ((t (:background "#ff0000" :foreground "#ffff00" :weight bold))))
   '(sr-marked-file-face ((t (:background "#fff0ff" :foreground "#ff00ff"))))
   '(sr-marked-dir-face ((t (:background "#fff0ff" :foreground "#ff00ff" :weight bold))))
   '(sr-compressed-face ((t (:foreground "#1e90ff"))))

   ;; eshell
   '(eshell-ls-directory ((t (:inherit default :weight bold))))
   `(eshell-prompt ((t (:foreground ,color-keyword))))


   ;; flycheck
   ;; default flycheck colors are just fine

   ;; which-key
   `(which-key-key-face ((t (:foreground ,color-keyword))))
   `(which-key-command-description-face ((t (:foreground ,color-fg))))
   `(which-key-group-description-face ((t (:foreground ,color-dim-fg :background ,color-dim-bg :weight bold))))
   `(which-key-separator-face ((t (:foreground ,color-dim-fg))))


   ;; company
   ;; default company colors are just fine for me

   ;; erc
   ;; '(erc-current-nick-face ((t (:foreground "#ffffff"))))
   ;;   ;; if erc-nick-default-face has foreground setup then it could not be
   ;;   ;; overriden by erc-my-nick-face
   ;;   ;; '(erc-nick-default-face ((t (:foreground "#779977"))))
   ;; '(erc-my-nick-face ((t (:foreground "#cc5555"))))
   ;; '(erc-input-face ((t (:foreground "#8dbdbd"))))
   ;; '(erc-timestamp-face ((t (:foreground "Wheat"))))
   ;; '(erc-notice-face ((t (:foreground "#555555"))))
   ;; '(erc-action-face ((nil (:slant italic))))
   ;; '(erc-button ((t (:underlined on))))


   ;; rcirc
   `(rcirc-server ((t (:foreground ,color-comment))))
   `(rcirc-timestamp ((t (:foreground ,color-comment))))
   `(rcirc-other-nick ((t (:foreground ,color-str))))
   `(rcirc-my-nick ((t (:foreground ,color-str :weight bold))))
   `(rcirc-nick-in-message ((t (:foreground ,color-str :weight bold))))
   `(rcirc-url ((t (:foreground ,color-url :underline t))))

   ;; `(rcirc-url ((t (:foreground ,color-fg-url :weight normal :underline (:color ,color-fg-url)))))


   ;; magit
   '(git-commit-summary ((t (:inherit default :weight bold))))

   ;; git gutter fringe
   `(git-gutter-fr:modified ((nil (:foreground "#f000f0" :weight bold))))
   `(git-gutter-fr:added ((nil (:foreground "#00c000" :weight bold))))
   `(git-gutter-fr:deleted ((nil (:foreground "#ff0000" :weight bold))))
   `(git-gutter:modified ((nil (:foreground "#f000f0" :weight bold))))
   `(git-gutter:added ((nil (:foreground "#00c000" :weight bold))))
   `(git-gutter:deleted ((nil (:foreground "#ff0000" :weight bold))))

   ;; Info
   `(info-title-1 ((t (:inherit default :weight bold :height ,height-1))))
   `(info-title-2 ((t (:inherit default :weight bold :height ,height-2))))
   `(info-title-3 ((t (:inherit default :weight bold :height ,height-3))))
   `(info-title-4 ((t (:inherit default :weight bold :height ,height-4))))

   ;; ace-window
   '(aw-leading-char-face ((nil (:foreground "#ff0000" :weight bold))))


   ;; ivy
   `(ivy-current-match ((t (:background ,color-bg-highlight-2))))
   `(ivy-minibuffer-match-face-1 ((t (:foreground ,color-dim-fg))))
   '(ivy-minibuffer-match-face-2 ((t (:foreground "#b030b0" :weight bold :underline (:color "b030b0")))))
   '(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2))))
   '(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-3))))
   '(ivy-modified-buffer ((nil (:slant italic))))
   `(ivy-remote ((t (:foreground ,color-dim-fg))))
   `(ivy-virtual ((t (:foreground ,color-comment))))

   ;; swiper
   `(swiper-match-face-1 ((t (:foreground ,color-dim-fg))))
   '(swiper-match-face-2 ((t (:inherit ivy-minibuffer-match-face-2))))
   '(swiper-match-face-3 ((t (:inherit swiper-match-face-2))))
   '(swiper-match-face-4 ((t (:inherit swiper-match-face-3))))


   ;; org
   `(org-document-title ((t (:foreground ,color-heading :weight bold :height ,height-1))))

   `(org-level-1 ((t (:foreground ,color-heading :weight bold :height ,height-1))))
   `(org-level-2 ((t (:foreground ,color-heading :weight bold :height ,height-2))))
   `(org-level-3 ((t (:foreground ,color-heading :weight bold :height ,height-3))))

   `(org-level-4 ((t (:foreground ,color-heading :slant italic :height ,height-4))))
   `(org-level-5 ((t (:foreground ,color-heading :slant italic :height ,height-5))))
   `(org-level-6 ((t (:foreground ,color-heading :slant italic :height ,height-6))))

   `(org-level-7 ((t (:foreground ,color-heading :slant italic :height ,height-6))))
   `(org-level-8 ((t (:foreground ,color-heading :slant italic :height ,height-6))))
   `(org-level-9 ((t (:foreground ,color-heading :slant italic :height ,height-6))))
   `(org-level-10 ((t (:foreground ,color-heading :slant italic :height ,height-6))))

   `(org-tag ((nil (:foreground ,color-comment))))

   '(org-table ((t (:inherit default))))
   `(org-date ((t (:foreground ,color-keyword-dim))))
   `(org-verbatim ((nil (:background ,color-dim-bg :foreground ,color-fg))))
   `(org-code ((nil (:background ,color-dim-bg :foreground ,color-fg))))

   `(org-special-keyword ((t (:foreground ,color-comment))))

   `(org-agenda-structure ((t (:foreground ,color-fg :height 1.6 :weight bold))))
   `(org-agenda-date ((nil (:inherit default))))
   `(org-agenda-date-today ((t (:background ,color-dim-bg :foreground ,color-dim-fg))))
   `(org-agenda-date-weekend ((t (:inherit default :weight bold))))

   '(org-meta-line ((t (:inherit org-special-keyword))))
   '(org-document-info-keyword ((t (:inherit org-meta-line))))

   `(org-block-begin-line ((t :foreground ,color-comment)))
   `(org-block ((t :background "#fafafa" :foreground ,color-fg)))
   `(org-block-end-line ((t (:foreground ,color-comment))))


   ;; calendar
   `(calendar-month-header ((t (:foreground ,color-fg :weight bold))))
   `(calendar-weekday-header ((t (:foreground ,color-dim-fg))))
   `(calendar-weekend-header ((t (:foreground ,color-str :weight bold))))
   '(calendar-today ((t (:background "#f0c0f0" :foreground "#000000" :weight bold))))



   ;; LaTeX
   ;; '(font-latex-sectioning-1-face ((t (:inherit org-level-1))))
   ;; '(font-latex-sectioning-2-face ((t (:inherit org-level-2))))
   ;; '(font-latex-sectioning-3-face ((t (:inherit org-level-3))))
   ;; '(font-latex-string-face ((t (:inherit font-lock-string-face))))
   ;; '(font-latex-bold-face ((t (:inherit bold))))

   ;; CSS
   ;; '(css-selector ((t (:inherit font-lock-keyword-face))))
   ;;   ;; '(css-property ((t (:inherit font-lock-keyword-face))))


   ;; XML
   `(nxml-element-local-name ((t (:foreground ,color-dim-fg :background ,color-dim-bg))))
   `(nxml-tag-delimiter ((t (:foreground ,color-dim-fg :background ,color-dim-bg))))
   ;; `(nxml-namespace-attribute-xmlns ((t (:foreground ,color-fg-dim))))
   `(nxml-attribute-local-name ((t (:foreground ,color-dim-fg))))
   `(nxml-attribute-value ((t (:foreground ,color-str))))
   `(nxml-cdata-section-CDATA ((t (:foreground ,color-dim-fg :background ,color-dim-bg))))
   `(nxml-cdata-section-delimiter ((t (:foreground ,color-dim-fg :background ,color-dim-bg))))
   `(nxml-cdata-section-content ((t (:background ,color-dim-bg))))


   ;; web-mode
   `(web-mode-html-tag-face ((t (:foreground ,color-dim-fg :background ,color-dim-bg))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,color-dim-fg :background ,color-dim-bg))))
   `(web-mode-html-attr-name-face ((t (:foreground ,color-dim-fg))))

   ;; whitespace-mode
   '(whitespace-space ((t (:foreground "#e7e7e7"))))
   '(whitespace-trailing ((t (:background "#f07070" :foreground "#f0f000"))))
   '(whitespace-space-after-tab ((t (:background "#f07070" :foreground "#f0f000"))))
   '(whitespace-indentation ((t (:background "#f0f0f0" :foreground "#aaaaaa"))))
   '(whitespace-line ((nil (:background "#f7f7f0"))))


   ;; rainbow-delimiters
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#000000"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#8a2be2"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#008b8b"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#ff7f50"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#3450F2"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#6F1E54"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#0DB100"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "#47D2E5"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "#000000"))))

   ;; asciidoctor-mode
   ;; `(asciidoctor-header-delimiter-face ((t (:foreground ,color-fg-dim))))

   `(asciidoctor-header-face-1 ((t (:foreground ,color-title :weight bold :height ,height-1))))
   `(asciidoctor-header-face-2 ((t (:foreground ,color-heading :weight bold :height ,height-1))))
   `(asciidoctor-header-face-3 ((t (:foreground ,color-heading :weight bold :height ,height-2))))
   `(asciidoctor-header-face-4 ((t (:foreground ,color-heading :weight bold :height ,height-3))))
   `(asciidoctor-header-face-5 ((t (:foreground ,color-heading :slant italic :height ,height-4))))
   `(asciidoctor-header-face-6 ((t (:foreground ,color-heading :slant italic :height ,height-5))))

   `(asciidoctor-option-face ((t (:foreground ,color-gray))))
   ;; `(asciidoctor-option-markup-face ((t (:foreground ,color-fg-dim))))


   ;; markdown-mode
   `(markdown-header-face-1 ((t (:foreground ,color-title :weight bold :height ,height-1))))
   `(markdown-header-face-2 ((t (:foreground ,color-heading :weight bold :height ,height-2))))
   `(markdown-header-face-3 ((t (:foreground ,color-heading :weight bold :height ,height-3))))
   `(markdown-header-face-4 ((t (:foreground ,color-heading :weight bold :height ,height-4))))
   `(markdown-header-face-5 ((t (:foreground ,color-heading :slant italic :height ,height-5))))
   `(markdown-header-face-6 ((t (:foreground ,color-heading :slant italic :height ,height-6))))
   `(markdown-code-face ((t (:background ,color-dim-bg))))


   ;; keycast-mode
   '(keycast-key ((t (:foreground "#FF0000" :background "#FFFF00" :weight bold :height 1.0))))
   '(keycast-command ((t (:foreground "#000000" :height 1.0))))


   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'habamax)

;;; noname-theme.el ends here
