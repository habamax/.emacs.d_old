(deftheme chaplin
  "Created 2016-03-14.")

(custom-theme-set-faces
 'chaplin
 '(font-lock-constant-face ((t (:slant italic))))
 '(default ((t (:background "black" :foreground "Gray90"))))
 '(font-lock-keyword-face ((t (:foreground "Gray60" :slant italic))))
 '(font-lock-builtin-face ((t (:foreground "Gray60"))))
 '(font-lock-string-face ((t (:foreground "SlateGray3"))))
 '(font-lock-comment-face ((t (:foreground "aquamarine4"))))
 '(font-lock-type-face ((t (:foreground "Gray60"))))
 '(font-lock-function-name-face ((t (:slant italic))))
 '(font-lock-variable-name-face ((t nil)))
 '(cursor ((t (:background "coral3"))))
 '(flycheck-warning ((t (:underline (:color "Wheat3" :style wave)))))
 '(region ((t (:background "LightSteelBlue4"))))
 '(flycheck-error ((t (:underline (:color "Coral" :style wave)))))
 '(flycheck-fringe-warning ((t (:foreground "wheat3"))))
 '(flycheck-fringe-error ((t (:foreground "coral"))))
 '(company-tooltip ((t (:background "gray20" :foreground "gray80"))))
 '(company-tooltip-selection ((t (:background "LightSteelBlue4" :foreground "white"))))
 '(company-scrollbar-bg ((t (:background "gray40"))))
 '(company-scrollbar-fg ((t (:background "gray80"))))
 '(company-tooltip-annotation ((t (:foreground "gray60" :slant italic))))
 '(company-tooltip-annotation-selection ((t (:foreground "Gray80")))))

(provide-theme 'chaplin)
