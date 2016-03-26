(defvar *haba-current-theme* 'default-black)

;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))


(defun haba/toggle-theme ()
  (interactive)
  (if (eq *haba-current-theme* 'default-black)
      (progn
        (load-theme 'default-light t)
        (setq *haba-current-theme* 'default-light))
    (progn
      (load-theme 'default-black t)
      (setq *haba-current-theme* 'default-black))
    )
  )







(provide 'haba-appearance)
