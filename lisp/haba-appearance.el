;; (defvar *haba-theme-dark* 'default-black)
(defvar *haba-theme-dark* 'kosmos)
(defvar *haba-theme-light* 'leuven)
(defvar *haba-current-theme* *haba-theme-dark*)


;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))


(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))


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
