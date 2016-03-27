(deftheme default-light
  "Created 2016-03-23.")

(custom-theme-set-faces
 'default-light
 '(default ((t (:background "#D7DCD7" :foreground "black"))))
 '(font-lock-comment-face ((t (:foreground "#707070"))))

 '(region ((nil (:background "#AACCAA"))))

 '(erc-timestamp-face ((t (:foreground "#33AA33"))))
 
 ;; '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
 ;; '(show-paren-match ((t (:foreground "white" :background "#333399"))))
 ;; '(show-paren-mismatch ((((class color)) (:background "lightred"))))
 )

(provide-theme 'default-light)
