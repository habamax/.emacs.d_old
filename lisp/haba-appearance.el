;; (defvar *haba-theme-dark* 'default-black)
(defvar *haba-theme-dark* 'kosmos)
(defvar *haba-theme-light* 'leuven)
(defvar *haba-theme-light2* 'eclipse)
(defvar *haba-current-theme* *haba-theme-dark*)


;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))


(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))


(defun haba/next-theme (theme)
  (load-theme theme t)
  (setq *haba-current-theme* theme))

(defun haba/toggle-theme ()
  (interactive)
  (cond ((eq *haba-current-theme* *haba-theme-dark*) (haba/next-theme *haba-theme-light*))
        ((eq *haba-current-theme* *haba-theme-light*) (haba/next-theme *haba-theme-light2*))
        ((eq *haba-current-theme* *haba-theme-light2*) (haba/next-theme *haba-theme-dark*))))



(load-theme *haba-current-theme* t)



(provide 'haba-appearance)
