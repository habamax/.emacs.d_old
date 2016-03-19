;;; redeyemir-theme.el --- Black and "white" theme.

;;; Commentary:
;; TODO: fix colors
;; `comments' too
;; (+) `erc' is too colorfull
;; erc modeline activity status is invisible
;; (+) `smartparens' selection is ugly(helkjsf)(slkjsfd)
;; (+) paired parens are ugly


;;; Code:


(deftheme redeyemir
  "Created 2016-03-16.")

(custom-theme-set-faces
 'redeyemir
 '(default ((t (:background "black" :foreground "Gray90"))))
 '(cursor ((t (:background "Coral3"))))
 '(region ((t (:background "DarkSlateGray" :foreground "Gray70"))))
 '(highlight ((t (:background "LightSteelBlue4" :foreground "White"))))
 
 '(font-lock-constant-face ((t (:slant italic))))
 '(font-lock-keyword-face ((t (:foreground "Gray60" :slant italic))))
 '(font-lock-builtin-face ((t (:foreground "Gray60"))))
 '(font-lock-string-face ((t (:foreground "SlateGray3"))))
 '(font-lock-comment-face ((t (:foreground "Aquamarine4"))))
 '(font-lock-type-face ((t (:foreground "Gray60"))))
 '(font-lock-function-name-face ((t (:slant italic))))
 '(font-lock-variable-name-face ((t nil)))

 '(show-paren-match ((t :background "Wheat" :foreground "black")))

 
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

 '(erc-nick-default-face ((t (:foreground "Gray60"))))
 '(erc-current-nick-face ((t (:foreground "Gray60" :slant italic))))

 '(erc-timestamp-face ((t (:foreground "Wheat"))))
 '(erc-notice-face ((t (:inherit font-lock-comment-face))))
 '(erc-action-face ((nil (:slant italic))))
 '(erc-button ((t (:underlined on))))
 



 )

(provide-theme 'redeyemir)

;;; redeyemir-theme.el ends here
