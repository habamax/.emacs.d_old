(deftheme default-black
  "Created 2016-03-23.")

(custom-theme-set-faces
 'default-black
 '(default ((t (:background "black" :foreground "white"))))
 '(region ((nil (:background "#464740"))))
; '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
 '(font-lock-comment-face ((t (:foreground "#707070"))))

 ;; '(show-paren-match ((t (:foreground "white" :background "#333399"))))
 ;; '(show-paren-mismatch ((((class color)) (:background "lightred"))))
 )

(provide-theme 'default-black)
