;; (defvar *haba-current-theme* 'default-black)

(defvar *haba-theme-dark* 'zenburn)
(defvar *haba-theme-light* 'default-light)
(defvar *haba-current-theme* *haba-theme-dark*)


;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))


(defun haba/toggle-theme ()
  (interactive)
  (if (eq *haba-current-theme* *haba-theme-dark*)
      (progn
        (load-theme *haba-theme-light* t)
        (setq *haba-current-theme* *haba-theme-light*))
    (progn
      (load-theme *haba-theme-dark* t)
      (setq *haba-current-theme* *haba-theme-dark*))
    )
  )



(load-theme *haba-current-theme* t)



(provide 'haba-appearance)
