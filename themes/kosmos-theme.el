;;; kosmos-theme.el --- Have you set up filters for you space suit? I have...

;;; Commentary:
;;; Sometimes I feel there are too many colors for the code I stare at.
;;; And it looks like I care of only 3 colored things:
;;; 1. Comments -- clearly visible and distinct but not too aggressive (I am looking at you, firebrick!)
;;; 2. Strings  -- did I miss a quote?
;;; 3. Keywords -- a bit of standout
;;;
;;; There are many other things in Emacs that should be colored.

;; TODO: fix colors
;; package-list-packages
;; swiper
;; TODO: change color names to color codes
;; TODO: find color for git-commit-nonempty-second-line



;;; Code:


(deftheme kosmos "Have you set up filters for you space suit? I have...")

(custom-theme-set-faces
 'kosmos

 '(default ((t (:background "#000000" :foreground "#aaaaaa"))))
 '(cursor ((nil (:background "#f070f0"))))
 '(region ((t (:background "#668b8b" :foreground "#eeeeee"))))
 '(highlight ((t (:background "#c1cdc1" :foreground "#000000"))))

 '(isearch ((t (:background "wheat" :foreground "#000000" :weight bold))))
 '(lazy-highlight ((t (:background "honeydew" :foreground "#000000"))))

 ;; font-lock I care about
 '(font-lock-string-face ((t (:foreground "#77cc77"))))

 ;; can't decide if cyan of wheat is better for comments.
 '(font-lock-comment-face ((t (:foreground "cyan3"))))
 ;; '(font-lock-comment-face ((t (:foreground "wheat"))))

 '(font-lock-keyword-face ((t (:foreground "#eeeeee"))))

 ;; font-lock I don't care about
 '(font-lock-builtin-face ((t nil)))
 '(font-lock-type-face ((t nil)))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-constant-face ((t nil)))

 ;; parenthesis
 '(show-paren-match ((t :background "Wheat" :foreground "black")))

 ;; links
 ;; FIXME: link-visited
 '(link ((t (:foreground "#eeeeee" :underline (:color "#eeeeee")))))

 ;; dired
 '(dired-directory ((t (:inherit font-lock-keyword-face :weight bold))))

 ;; flycheck
 '(flycheck-warning ((t (:underline (:color "Wheat3" :style wave)))))
 '(flycheck-error ((t (:underline (:color "Coral" :style wave)))))
 '(flycheck-fringe-warning ((t (:foreground "Wheat3"))))
 '(flycheck-fringe-error ((t (:foreground "Coral"))))

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

 ;; org
 '(org-document-title ((t (:weight bold :height 1.8))))
 
 '(org-level-1 ((t (:height 1.6))))
 '(org-level-2 ((t (:height 1.4))))
 '(org-level-3 ((t (:height 1.2))))
 
 '(org-level-4 ((t (:slant italic :height 1.1))))
 '(org-level-5 ((t (:slant italic :height 1.1))))
 '(org-level-6 ((t (:slant italic :height 1.1))))

 '(org-meta-line ((t (:background "#171717" :foreground "#707070" ))))
 '(org-document-info-keyword ((t (:inherit org-meta-line))))

 '(org-block-begin-line ((t (:inherit org-meta-line))))
 '(org-block-background ((t (:background "#101010"))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))))



 )

(provide-theme 'kosmos)

;;; kosmos-theme.el ends here
