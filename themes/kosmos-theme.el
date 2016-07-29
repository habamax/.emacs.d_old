;;; kosmos-theme.el --- Have you set up optical filters for you space suit? I have...

;;; Commentary:
;;; Sometimes I feel there are too many colors for the code I stare at.
;;; And it looks like I care of only 3 colored things:
;;; 1. Comments -- clearly visible and distinct but not too aggressive (I am looking at you, firebrick!)
;;; 2. Strings  -- did I miss a quote?
;;; 3. Keywords -- a bit of standout
;;;
;;; There are many other things in Emacs that should be colored.

;; TODO: find color for git-commit-nonempty-second-line
;; clojure (font-lock-warning-face --> cider-repl-stderr-face) is ugly
;; snippet mode clash white {} with pair highlighting




;;; Code:


(deftheme kosmos "Have you set up optical filters for you space suit? I have...")

(let
    ((kosmos-fg "#bdbdbd")
     (kosmos-bg "#000000")
     (kosmos-bg-modeline-active "#2f4f4f")
     (kosmos-bg-modeline-inactive "#202020")
     (kosmos-box-modeline-active "#3f5f5f")
     (kosmos-box-modeline-inactive "#303030")
     (kosmos-keyword "#ffffff")
     (kosmos-str "#77cc77")
     ;; (kosmos-comment "#50abab")
     (kosmos-comment "#5797a0")
     (kosmos-gray "#777777")
     (kosmos-fg-dim "#777777")
     (kosmos-fg-dim-2 "#506060")
     (kosmos-fg-todo "#bdabab")
     (kosmos-bg-todo "#775555")
     (kosmos-fg-done "#abbdab")
     ;; (kosmos-bg-done "#557755")
     (kosmos-bg-done "#777777")
     (kosmos-h1 "#b0b0b0")
     (kosmos-h2 "#b0b090")
     (kosmos-bg-hl-parens "#324035")
     (kosmos-bg-alt "#252520")
     (kosmos-fg-modeline-hl "#ffff00")
     (kosmos-bg-whitespace "#1a1a1f")
     (kosmos-fg-whitespace "#303040")
     )



  (custom-theme-set-faces
   'kosmos

   `(default ((t (:background ,kosmos-bg :foreground ,kosmos-fg))))
   '(cursor ((nil (:background "#f0f0f0"))))
   `(region ((t (:background "#668b8b" :foreground ,kosmos-bg))))
   ;; '(highlight ((t (:background "#65a7e2" :foreground "#000000"))))
   ;; '(highlight ((t (:background "#202090" :foreground "#bdbdbd"))))
   '(highlight ((nil (:background "#304050"))))
   '(bold ((t (:weight bold))))

   '(isearch ((t (:background "wheat" :foreground "#000000" :weight bold))))
   '(lazy-highlight ((t (:background "honeydew" :foreground "#000000"))))



   `(mode-line ((t (:background ,kosmos-bg-modeline-active :foreground ,kosmos-keyword :box (:line-width 1 :color ,kosmos-box-modeline-active)))))
   `(mode-line-inactive ((t (:background ,kosmos-bg-modeline-inactive :foreground ,kosmos-gray :box (:line-width 1 :color ,kosmos-box-modeline-inactive)))))

   `(mode-line-highlight ((nil (:foreground ,kosmos-fg-modeline-hl :box (:line-width 1 :color ,kosmos-fg)))))


   `(vertical-border ((nil (:foreground ,kosmos-box-modeline-inactive))))
   `(fringe ((nil (:background ,kosmos-bg))))


   ;; font-lock I care about
   `(font-lock-string-face ((t (:foreground ,kosmos-str))))
   ;; review this later.
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,kosmos-str :weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,kosmos-str :weight bold :slant italic))))

   ;; can't decide if cyan of wheat is better for comments.
   `(font-lock-comment-face ((t (:foreground ,kosmos-comment))))
   ;; '(font-lock-comment-face ((t (:foreground "wheat"))))

   `(font-lock-keyword-face ((t (:foreground ,kosmos-keyword))))



   ;; font-lock I don't care about
   '(font-lock-builtin-face ((t nil)))
   '(font-lock-type-face ((t nil)))
   '(font-lock-function-name-face ((t nil)))
   '(font-lock-variable-name-face ((t nil)))
   '(font-lock-constant-face ((t nil)))


   ;; parenthesis and pairs
   `(show-paren-match ((t :background ,kosmos-bg-hl-parens)))
   `(sp-show-pair-match-face ((t (:background ,kosmos-bg-hl-parens))))



   ;; links
   `(link ((t (:foreground ,kosmos-keyword :underline (:color ,kosmos-keyword)))))
   `(link-visited ((t (:foreground ,kosmos-fg :underline (:color ,kosmos-fg)))))

   ;; dired
   '(dired-directory ((t (:inherit font-lock-keyword-face :weight bold))))

   ;; flycheck
   '(flycheck-warning ((t (:underline (:color "Wheat3" :style wave)))))
   '(flycheck-error ((t (:underline (:color "Coral" :style wave)))))
   '(flycheck-fringe-warning ((t (:foreground "Wheat3"))))
   '(flycheck-fringe-error ((t (:foreground "Coral"))))

   ;; which-key
   '(which-key-key-face ((t (:foreground "#ffa500"))))
   '(which-key-separator-face ((t (:foreground "#4d4d4d"))))

   ;; company
   '(company-tooltip ((t (:background "Gray20" :foreground "Gray80"))))
   '(company-tooltip-selection ((t (:background "LightSteelBlue4" :foreground "White"))))
   '(company-tooltip-annotation ((t (:foreground "Gray60" :slant italic))))
   '(company-tooltip-annotation-selection ((t (:foreground "Gray80"))))
   '(company-tooltip-common ((t (:slant italic))))
   '(company-scrollbar-bg ((t (:background "Gray40"))))
   '(company-scrollbar-fg ((t (:background "Gray80"))))
   '(company-preview ((t (:inherit company-tooltip-selection))))
   '(company-preview-common ((t (:inherit company-preview :slant italic))))

   ;; erc
   '(erc-current-nick-face ((t (:foreground "#ffffff"))))
   '(erc-nick-default-face ((t (:foreground "#779977"))))
   '(erc-my-nick-face ((t (:slant italic))))
   '(erc-input-face ((t (:foreground "#cccccc"))))
   '(erc-timestamp-face ((t (:foreground "Wheat"))))
   '(erc-notice-face ((t (:foreground "#555555"))))
   '(erc-action-face ((nil (:slant italic))))
   '(erc-button ((t (:underlined on))))

   '(header-line ((t (:foreground "#ffffff" :background "#333333" :box (:line-width 1 :color "#555555" :style released-button)))))

   ;; magit
   '(git-commit-summary ((t (:inherit font-lock-string-face))))

   ;; ivy
   '(ivy-current-match ((nil (:inherit highlight))))
   '(ivy-minibuffer-match-face-1 ((nil (:inherit highlight))))
   '(ivy-minibuffer-match-face-2 ((nil (:weight bold))))
   '(ivy-minibuffer-match-face-3 ((nil (:weight bold))))
   '(ivy-minibuffer-match-face-4 ((nil (:weight bold))))
   ;; '(ivy-minibuffer-match-face-2 ((nil (:background "#202020" :weight bold))))
   ;; '(ivy-minibuffer-match-face-3 ((nil (:background "#303030" :weight bold))))
   ;; '(ivy-minibuffer-match-face-4 ((nil (:background "#404040" :weight bold))))
   '(ivy-modified-buffer ((t (:inherit font-lock-keyword-face))))
   '(ivy-remote ((t (:inherit font-lock-comment-face))))
   '(ivy-virtual ((t (:foreground "#787878"))))

   ;; swiper
   '(swiper-match-face-1 ((nil (:inherit ivy-minibuffer-match-face-1))))
   '(swiper-match-face-2 ((nil (:inherit ivy-minibuffer-match-face-2))))
   '(swiper-match-face-3 ((nil (:inherit ivy-minibuffer-match-face-3))))
   '(swiper-match-face-4 ((nil (:inherit ivy-minibuffer-match-face-4))))



   ;; org
   `(org-document-title ((t (:foreground ,kosmos-keyword :weight bold :height 1.6))))

   `(org-level-1 ((t (:foreground ,kosmos-h1 :weight bold :height 1.3))))
   `(org-level-2 ((t (:foreground ,kosmos-h2 :weight bold :height 1.2))))
   `(org-level-3 ((t (:foreground ,kosmos-h1 :weight bold :height 1.1))))

   `(org-level-4 ((t (:foreground ,kosmos-h2 :slant italic :height 1.1))))
   `(org-level-5 ((t (:foreground ,kosmos-h1 :slant italic :height 1.1))))
   `(org-level-6 ((t (:foreground ,kosmos-h2 :slant italic :height 1.1))))

   `(org-level-7 ((t (:foreground ,kosmos-h1 :slant italic :height 1))))
   `(org-level-8 ((t (:foreground ,kosmos-h2 :slant italic :height 1))))
   `(org-level-9 ((t (:foreground ,kosmos-h1 :slant italic :height 1))))
   `(org-level-10 ((t (:foreground ,kosmos-h2 :slant italic :height 1))))

   `(org-tag ((nil (:foreground ,kosmos-comment))))

   `(org-archived ((nil (:foreground ,kosmos-gray))))
   ;; todo: play with colors of the box
   `(org-todo ((nil (:background ,kosmos-bg-todo :foreground ,kosmos-fg-todo :weight bold :box (:line-width 1 :color ,kosmos-fg-todo)))))
   `(org-done ((nil (:background ,kosmos-bg-done :foreground ,kosmos-fg-done :weight bold :box (:line-width 1 :color ,kosmos-fg-done)))))

   '(org-table ((t (:inherit default))))

   `(org-date ((t (:foreground ,kosmos-comment :underline (:color ,kosmos-comment)))))

   `(org-verbatim ((nil (:background ,kosmos-bg-alt :foreground ,kosmos-fg))))

   `(org-special-keyword ((t (:foreground ,kosmos-gray :background ,kosmos-bg-modeline-inactive))))

   `(org-agenda-structure ((t (:foreground ,kosmos-fg :height 1.6 :weight bold))))
   `(org-agenda-date ((nil (:height 1.0))))
   `(org-agenda-date-today ((t (:height 1.5 :weight bold))))
   `(org-agenda-date-weekend ((t (:foreground ,kosmos-h2 :height 1.3))))

   '(org-scheduled ((t (:inherit :default))))
   `(org-scheduled-today ((t (:inherit :default :foreground ,kosmos-keyword))))
   `(org-scheduled-previously ((t (:inherit :default :foreground "#d05050"))))
   `(org-agenda-done ((t (:inherit :default :foreground ,kosmos-gray))))
   `(org-warning ((t (:foreground "#d0a000"))))

   `(org-agenda-clocking ((t (:background "#303030"))))

   '(org-meta-line ((t (:foreground "#707070" ))))
   '(org-document-info-keyword ((t (:inherit org-meta-line))))

   `(org-block-begin-line ((t :foreground ,kosmos-comment)))
   `(org-block-end-line ((t (:foreground ,kosmos-comment))))


   ;; calendar
   `(calendar-month-header ((t (:foreground ,kosmos-keyword :weight bold))))
   `(calendar-weekday-header ((t (:foreground ,kosmos-comment))))
   `(calendar-weekend-header ((t (:foreground ,kosmos-str :weight bold))))
   `(calendar-today ((t (:foreground ,kosmos-keyword :weight bold))))


   ;; emms
   '(emms-playlist-track-face ((t (:inherit default))))
   '(emms-playlist-selected-face ((t (:background "#20408b" :foreground "white" :weight bold))))

   ;; widget
   '(widget-field-face ((t (:background "#a0a0a0" :foreground "#000000"))))


   ;; LaTeX
   '(font-latex-sectioning-1-face ((t (:inherit org-level-1))))
   '(font-latex-sectioning-2-face ((t (:inherit org-level-2))))
   '(font-latex-sectioning-3-face ((t (:inherit org-level-3))))
   '(font-latex-string-face ((t (:inherit font-lock-string-face))))
   '(font-latex-bold-face ((t (:inherit bold))))

   ;; CSS
   '(css-selector ((t (:inherit font-lock-keyword-face))))
   ;; '(css-property ((t (:inherit font-lock-keyword-face))))


   ;; XML
   `(nxml-element-local-name ((t (:foreground ,kosmos-fg-dim))))
   `(nxml-tag-delimiter ((t (:foreground ,kosmos-fg-dim))))
   `(nxml-attribute-local-name ((t (:foreground ,kosmos-fg-dim-2))))
   `(nxml-attribute-value ((t (:foreground ,kosmos-fg-dim-2))))

   ;; whitespace-mode
   `(whitespace-space ((t (:foreground ,kosmos-fg-whitespace))))
   `(whitespace-indentation ((t (:foreground ,kosmos-fg-whitespace :background ,kosmos-bg-whitespace))))

   )
  )

(provide-theme 'kosmos)

;;; kosmos-theme.el ends here
