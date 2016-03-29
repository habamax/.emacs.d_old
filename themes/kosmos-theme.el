;;; kosmos-theme.el --- Black and "white" theme.

;;; Commentary:
;; TODO: fix colors
;; rcirc
;; package-list-packages
;; swiper
;; incremental search
;; TODO: change color names to color codes
;; TODO: find color for git-commit-nonempty-second-line



;;; Code:


(deftheme kosmos "Have you set up filters for you space suit? I have...")

(custom-theme-set-faces
 'kosmos

 '(default ((t (:background "black" :foreground "#aaaaaa"))))
 '(cursor ((t (:background "Coral3"))))
 '(region ((t (:background "cadetblue4" :foreground "black"))))
 '(highlight ((t (:background "#c1cdc1" :foreground "#000000"))))
 
 '(font-lock-string-face ((t (:foreground "wheat3"))))
 '(font-lock-comment-face ((t (:foreground "cyan3"))))
 '(font-lock-keyword-face ((t (:foreground "#dddddd" :slant italic))))
 
 '(font-lock-builtin-face ((t nil)))
 '(font-lock-type-face ((t nil)))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-constant-face ((t nil)))

 '(show-paren-match ((t :background "Wheat" :foreground "black")))

 '(dired-directory ((t (:inherit font-lock-keyword-face :weight bold))))

 
 '(flycheck-warning ((t (:underline (:color "Wheat3" :style wave)))))
 '(flycheck-error ((t (:underline (:color "Coral" :style wave)))))
 '(flycheck-fringe-warning ((t (:foreground "Wheat3"))))
 '(flycheck-fringe-error ((t (:foreground "Coral"))))

 '(company-tooltip ((t (:background "Gray20" :foreground "Gray80"))))
 '(company-tooltip-selection ((t (:background "LightSteelBlue4" :foreground "White"))))
 '(company-tooltip-annotation ((t (:foreground "Gray60" :slant italic))))
 '(company-tooltip-annotation-selection ((t (:foreground "Gray80"))))
 '(company-tooltip-common ((t (:slant italic))))
 '(company-scrollbar-bg ((t (:background "Gray40"))))
 '(company-scrollbar-fg ((t (:background "Gray80"))))
 '(company-preview ((t (:inherit company-tooltip-selection))))
 '(company-preview-common ((t (:inherit company-preview :slant italic))))

 
 '(erc-current-nick-face ((t (:foreground "#ffffff"))))
 '(erc-nick-default-face ((t (:foreground "#779977"))))
 '(erc-my-nick-face ((t (:slant italic))))
 '(erc-input-face ((t (:foreground "#cccccc"))))
 '(erc-timestamp-face ((t (:foreground "Wheat"))))
 '(erc-notice-face ((t (:foreground "#555555"))))
 '(erc-action-face ((nil (:slant italic))))
 '(erc-button ((t (:underlined on))))

 '(header-line ((t (:foreground "#ffffff" :background "#333333" :box (:line-width 1 :color "#555555" :style released-button)))))

 '(git-commit-summary ((t (:inherit font-lock-string-face))))


 '(org-level-1 ((t (:height 1.5))))
 '(org-level-2 ((t (:height 1.25))))
 '(org-level-3 ((t (:height 1.125))))
 
 '(org-level-4 ((t (:slant italic :height 1.1))))
 '(org-level-5 ((t (:slant italic :height 1.1))))
 '(org-level-6 ((t (:slant italic :height 1.1))))


 )

(provide-theme 'kosmos)

;;; kosmos-theme.el ends here
