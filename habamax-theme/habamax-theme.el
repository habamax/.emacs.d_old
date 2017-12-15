;;; habamax-theme.el --- no description, but it should be whity and plainy

;; Author: Maxim Kim <habamax@gmail.com>
;; Url: https://github.com/habamax/habamax-theme
;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; 0. background -- snowwhite
;; 1. keywords -- blueish
;; 2. strings -- reddish
;; 3. comments -- gray gray gray

;;; TODO:

;; highlight line
;; magit highlight
;; diff mode colors refinements
;; TODO: colors
;; minimal org mode refinements
;; ivy and swiper
;; isearch

;;; Code:

(deftheme habamax "and no description yet...")

(let
    ((color-fg "#000000")
     (color-bg "#ffffff")
     (color-dim-bg "#f5f9fe")
     ;; (color-keyword "#8b2323")
     (color-keyword "#0000ff")
     (color-keyword-dim "#204060")
     (color-str "#901515")
     (color-comment "#808080")
     (color-gray "#7a7a7a")
     ;; (color-fg-todo "#bdabab")
     ;; (color-bg-todo "#775555")
     ;; (color-fg-done "#abbdab")
     ;; (color-bg-done "#777777")
     (color-title "#000000")
     (color-heading "#000000")
     (color-url "#0000ff")
     (color-url-visited "#7070ff")
     ;; (color-bg-hl-parens "#703000")
     ;; (color-bg-alt "#252520")
     ;; (color-bg-whitespace "#1a1a1f")
     ;; (color-fg-whitespace "#303040")
     ;; (color-fg-search "#fff68f")
     ;; (color-bg-search "#606020")
     ;; (color-fg-lazysearch "#40e0d0")
     ;; (color-bg-lazysearch "#206060")
     ;; (color-fg-search-fail "#da70d6")
     ;; (color-bg-search-fail "#603060")
     (color-bg-highlight "#c4f0c4")
)



  (custom-theme-set-faces
   'habamax

   ;; standard faces
   `(default ((t (:background ,color-bg :foreground ,color-fg))))
   '(cursor ((nil (:background "#000000"))))
   '(region ((t (:background "#f0e0a0"))))
   `(highlight ((nil (:background ,color-bg-highlight))))
   `(minibuffer-prompt ((t (:foreground ,color-keyword-dim :background ,color-dim-bg :weight bold))))
   ;; '(widget-field-face ((t (:background "#a0a0a0" :foreground "#000000"))))
   `(header-line ((t (:foreground ,color-fg :background "#e9e590" :box (:line-width 1 :color "#a9a550")))))
   
   ;; Default isearch is OK for now.
   ;; `(isearch ((t (:background ,color-bg-search :foreground ,color-fg-search :weight bold :underline (:color ,color-fg-search)))))
   ;; `(lazy-highlight ((t (:background ,color-bg-lazysearch :foreground ,color-fg-lazysearch :weight bold :underline (:color ,color-fg-lazysearch)))))
   ;; match?
   ;; `(isearch-fail ((t (:background ,color-bg-search-fail :foreground ,color-fg-search-fail :weight bold :underline (:color ,color-fg-search-fail)))))


   ;; frame UI
   '(mode-line ((t (:background "#b5d5f5" :foreground "#406582"))))
   `(mode-line-highlight ((t (:foreground ,color-keyword))))
   '(mode-line-buffer-id ((t (:foreground "#000000" :weight bold))))
   '(mode-line-inactive ((t (:background "#e0e5e2" :foreground "#505552"))))
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
   `(font-lock-comment-face ((t (:foreground ,color-comment))))
   `(font-lock-keyword-face ((t (:foreground ,color-keyword))))
   `(font-lock-builtin-face ((t (:foreground ,color-keyword-dim))))
   `(font-lock-function-name-face ((t (:foreground ,color-fg))))
   `(font-lock-type-face ((t (:foreground ,color-keyword))))
   ;; syntax font-lock I DON'T care about
   '(font-lock-variable-name-face ((t nil)))
   '(font-lock-constant-face ((t nil)))
   ;; review this later.
   ;; `(font-lock-regexp-grouping-backslash ((t (:foreground ,color-str :weight bold))))
   ;; `(font-lock-regexp-grouping-construct ((t (:foreground ,color-str :weight bold :slant italic))))


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
   '(dired-header ((t (:inherit default :weight bold))))

   ;; eshell
   '(eshell-ls-directory ((t (:inherit default :weight bold))))
   `(eshell-prompt ((t (:foreground ,color-keyword))))


   ;; flycheck
   ;; '(flycheck-warning ((t (:underline (:color "Wheat3" :style wave)))))
   ;; '(flycheck-error ((t (:underline (:color "Coral" :style wave)))))
   ;; '(flycheck-fringe-warning ((t (:foreground "Wheat3"))))
   ;; '(flycheck-fringe-error ((t (:foreground "Coral"))))

   ;; which-key
   `(which-key-key-face ((t (:foreground ,color-keyword))))
   `(which-key-command-description-face ((t (:foreground ,color-fg))))
   `(which-key-group-description-face ((t (:foreground ,color-keyword-dim :background ,color-dim-bg :weight bold))))
   `(which-key-separator-face ((t (:foreground ,color-keyword-dim))))

   
   ;; company
   ;; '(company-tooltip ((t (:background "Gray20" :foreground "Gray80"))))
   ;; '(company-tooltip-selection ((t (:background "LightSteelBlue4" :foreground "White"))))
   ;; '(company-tooltip-annotation ((t (:foreground "Gray60" :slant italic))))
   ;; '(company-tooltip-annotation-selection ((t (:foreground "Gray80"))))
   ;; '(company-tooltip-common ((t (:slant italic))))
   ;; '(company-scrollbar-bg ((t (:background "Gray40"))))
   ;; '(company-scrollbar-fg ((t (:background "Gray80"))))
   ;; '(company-preview ((t (:inherit company-tooltip-selection))))
   ;; '(company-preview-common ((t (:inherit company-preview :slant italic))))

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

   ;; ivy
   ;; `(ivy-current-match ((t (:background ,color-bg-highlight))))
   ;; `(ivy-minibuffer-match-face-1 ((t (:foreground ,color-fg-dim))))
   ;; `(ivy-minibuffer-match-face-2 ((t (:foreground ,color-fg-search :weight bold :underline (:color ,color-fg-search)))))
   ;; `(ivy-minibuffer-match-face-3 ((t (:foreground ,color-fg-lazysearch :weight bold :underline (:color ,color-bg-lazysearch)))))
   ;; `(ivy-minibuffer-match-face-4 ((t (:foreground ,color-fg-search-fail :weight bold :underline (:color ,color-bg-search-fail)))))
   ;; `(ivy-modified-buffer ((nil (:foreground ,color-keyword :slant italic))))
   ;; '(ivy-remote ((t (:inherit font-lock-comment-face))))
   ;; `(ivy-virtual ((t (:foreground ,color-fg-dim))))

   ;; swiper
   ;; `(swiper-match-face-1 ((t (:foreground ,color-fg-dim))))
   ;; `(swiper-match-face-2 ((t (:background ,color-bg-search :foreground ,color-fg-search :weight bold :underline (:color ,color-fg-search)))))
   ;; `(swiper-match-face-3 ((t (:background ,color-bg-lazysearch :foreground ,color-fg-lazysearch :weight bold :underline (:color ,color-bg-lazysearch)))))
   ;; `(swiper-match-face-4 ((t (:background ,color-bg-search-fail :foreground ,color-fg-search-fail :weight bold :underline (:color ,color-bg-search-fail)))))
   

   ;; org
   ;; `(org-document-title ((t (:foreground ,color-keyword :weight bold :height 1.6))))
   
   ;; `(org-level-1 ((t (:foreground ,color-heading :weight bold :height 1.3))))
   ;; `(org-level-2 ((t (:foreground ,color-heading :weight bold :height 1.2))))
   ;; `(org-level-3 ((t (:foreground ,color-heading :weight bold :height 1.1))))

   ;; `(org-level-4 ((t (:foreground ,color-heading :slant italic :height 1.1))))
   ;; `(org-level-5 ((t (:foreground ,color-heading :slant italic :height 1.1))))
   ;; `(org-level-6 ((t (:foreground ,color-heading :slant italic :height 1.1))))

   ;; `(org-level-7 ((t (:foreground ,color-heading :slant italic :height 1))))
   ;; `(org-level-8 ((t (:foreground ,color-heading :slant italic :height 1))))
   ;; `(org-level-9 ((t (:foreground ,color-heading :slant italic :height 1))))
   ;; `(org-level-10 ((t (:foreground ,color-heading :slant italic :height 1))))

   ;; `(org-tag ((nil (:foreground ,color-comment))))

   ;; `(org-archived ((nil (:foreground ,color-gray))))
   ;; todo: play with colors of the box
   ;; `(org-todo ((nil (:background ,color-bg-todo :foreground ,color-fg-todo :weight bold :box (:line-width 1 :color ,color-fg-todo)))))
   ;; `(org-done ((nil (:background ,color-bg-done :foreground ,color-fg-done :weight bold :box (:line-width 1 :color ,color-fg-done)))))
   
   ;; '(org-table ((t (:inherit default))))
   
   ;; `(org-date ((t (:foreground ,color-comment :underline (:color ,color-comment)))))
   
   ;; `(org-verbatim ((nil (:background ,color-bg-alt :foreground ,color-fg))))
   
   ;; `(org-special-keyword ((t (:foreground ,color-gray :background ,color-bg-modeline-inactive))))

   ;; `(org-agenda-structure ((t (:foreground ,color-fg :height 1.6 :weight bold))))
   ;; `(org-agenda-date ((nil (:height 1.0))))
   ;; `(org-agenda-date-today ((t (:height 1.5 :weight bold))))
   ;; `(org-agenda-date-weekend ((t (:foreground ,color-title :height 1.3))))

   ;; '(org-scheduled ((t (:inherit :default))))
   ;; `(org-scheduled-today ((t (:inherit :default :foreground ,color-keyword))))
   ;; `(org-scheduled-previously ((t (:inherit :default :foreground "#d05050"))))
   ;; `(org-agenda-done ((t (:inherit :default :foreground ,color-gray))))
   ;; `(org-warning ((t (:foreground "#d0a000"))))

   ;; `(org-agenda-clocking ((t (:background "#303030"))))

   ;; '(org-meta-line ((t (:foreground "#707070" ))))
   ;; '(org-document-info-keyword ((t (:inherit org-meta-line))))

   ;; `(org-block-begin-line ((t :foreground ,color-comment)))
   ;; `(org-block-end-line ((t (:foreground ,color-comment))))


   ;; calendar
   ;; `(calendar-month-header ((t (:foreground ,color-keyword :weight bold))))
   ;; `(calendar-weekday-header ((t (:foreground ,color-comment))))
   ;; `(calendar-weekend-header ((t (:foreground ,color-str :weight bold))))
   ;; `(calendar-today ((t (:foreground ,color-keyword :weight bold))))


   ;; emms
   ;; '(emms-playlist-track-face ((t (:inherit default))))
   ;; '(emms-playlist-selected-face ((t (:background "#20408b" :foreground "white" :weight bold))))


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
   `(nxml-element-local-name ((t (:foreground ,color-keyword-dim :background ,color-dim-bg))))
   `(nxml-tag-delimiter ((t (:foreground ,color-keyword-dim :background ,color-dim-bg))))
   ;; `(nxml-namespace-attribute-xmlns ((t (:foreground ,color-fg-dim))))
   `(nxml-attribute-local-name ((t (:foreground ,color-keyword-dim))))
   `(nxml-attribute-value ((t (:foreground ,color-str))))
   `(nxml-cdata-section-CDATA ((t (:foreground ,color-keyword-dim :background ,color-dim-bg))))
   `(nxml-cdata-section-delimiter ((t (:foreground ,color-keyword-dim :background ,color-dim-bg))))
   `(nxml-cdata-section-content ((t (:background ,color-dim-bg))))


   ;; web-mode
   `(web-mode-html-tag-face ((t (:foreground ,color-keyword-dim :background ,color-dim-bg))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,color-keyword-dim :background ,color-dim-bg))))
   `(web-mode-html-attr-name-face ((t (:foreground ,color-keyword-dim))))

   ;; whitespace-mode
   ;; `(whitespace-space ((t (:foreground ,color-fg-whitespace))))
   ;; `(whitespace-newline ((t (:foreground ,color-fg-whitespace))))
   ;; `(whitespace-indentation ((t (:foreground ,color-fg-whitespace :background ,color-bg-whitespace))))
   ;; `(whitespace-line ((nil (:background ,color-bg-whitespace))))


   ;; ;; rainbow-delimiters
   ;; `(rainbow-delimiters-depth-1-face ((t (:foreground ,color-keyword))))
   ;; '(rainbow-delimiters-depth-2-face ((t (:foreground "#000000"))))
   ;; `(rainbow-delimiters-depth-3-face ((t (:foreground ,color-keyword))))
   ;; '(rainbow-delimiters-depth-4-face ((t (:foreground "#000000"))))
   ;; `(rainbow-delimiters-depth-5-face ((t (:foreground ,color-keyword))))
   ;; '(rainbow-delimiters-depth-6-face ((t (:foreground "#000000"))))
   ;; `(rainbow-delimiters-depth-7-face ((t (:foreground ,color-keyword))))
   ;; '(rainbow-delimiters-depth-8-face ((t (:foreground "#000000"))))
   ;; `(rainbow-delimiters-depth-9-face ((t (:foreground ,color-keyword))))

   ;; rainbow-delimiters
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#000000"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#8a2be2"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#008b8b"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#ff7f50"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#000000"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#8a2be2"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#008b8b"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "#ff7f50"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "#000000"))))


   ;; asciidoctor-mode
   ;; `(asciidoctor-header-delimiter-face ((t (:foreground ,color-fg-dim))))

   `(asciidoctor-header-face-1 ((t (:foreground ,color-title :weight bold :height 1.3))))
   `(asciidoctor-header-face-2 ((t (:foreground ,color-heading :weight bold :height 1.3))))
   `(asciidoctor-header-face-3 ((t (:foreground ,color-heading :weight bold :height 1.2))))
   `(asciidoctor-header-face-4 ((t (:foreground ,color-heading :weight bold :height 1.1))))
   `(asciidoctor-header-face-5 ((t (:foreground ,color-heading :slant italic :height 1.1))))
   `(asciidoctor-header-face-6 ((t (:foreground ,color-heading :slant italic :height 1.1))))

   `(asciidoctor-option-face ((t (:foreground ,color-gray))))
   ;; `(asciidoctor-option-markup-face ((t (:foreground ,color-fg-dim))))


   ;; markdown-mode
   `(markdown-header-face-1 ((t (:foreground ,color-title :weight bold :height 1.3))))
   `(markdown-header-face-2 ((t (:foreground ,color-heading :weight bold :height 1.2))))
   `(markdown-header-face-3 ((t (:foreground ,color-heading :weight bold :height 1.1))))
   `(markdown-header-face-4 ((t (:foreground ,color-heading :weight bold :height 1.1))))
   `(markdown-header-face-5 ((t (:foreground ,color-heading :slant italic :height 1.1))))
   `(markdown-header-face-6 ((t (:foreground ,color-heading :slant italic :height 1.1))))
   `(markdown-code-face ((t (:background ,color-dim-bg))))

   ;; `(bongo-comment ((t (:foreground ,color-fg-dim))))
   ;; `(bongo-elapsed-track-part ((t (:background ,color-bg-alt))))
   ;; `(bongo-currently-playing-track ((t (:slant italic))))

   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'habamax)

;;; noname-theme.el ends here
