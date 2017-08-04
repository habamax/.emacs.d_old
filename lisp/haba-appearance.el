(defvar *haba-theme-dark* 'kosmos)
(defvar *haba-theme-light* 'leuven)
(defvar *haba-theme-light2* 'habamax)
(defvar *haba-current-theme* 'default)


;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "habamax-theme"))


(defun disable-all-themes (&rest args)
  (mapcar #'disable-theme custom-enabled-themes))

(defun store-current-theme (&rest args)
  (setq *haba-current-theme* (car args)))

(advice-add 'load-theme :before #'disable-all-themes)
(advice-add 'load-theme :after #'store-current-theme)


(defun haba/next-theme (theme)
  (if (eq theme 'default)
      (disable-theme *haba-current-theme*)
    (load-theme theme t))
  (setq *haba-current-theme* theme))

(defun haba/toggle-theme ()
  (interactive)
  (cond ((eq *haba-current-theme* *haba-theme-dark*) (haba/next-theme *haba-theme-light*))
        ((eq *haba-current-theme* *haba-theme-light*) (haba/next-theme *haba-theme-light2*))
        ((eq *haba-current-theme* *haba-theme-light2*) (haba/next-theme 'default))
        ((eq *haba-current-theme* 'default) (haba/next-theme *haba-theme-dark*))
        (t (haba/next-theme 'default))))


(defun haba/read-current-theme ()
  (ignore-errors
    (let ((theme-file (concat user-emacs-directory "current-theme")))
      (if (file-exists-p theme-file)
          (with-temp-buffer
            (insert-file-contents theme-file)
            (buffer-string))
        nil))))

(defun haba/save-current-theme ()
  (ignore-errors
    (with-temp-file (concat user-emacs-directory "current-theme")
      (insert (symbol-name *haba-current-theme*)))))

(defun haba/set-current-theme ()
  "Load current theme"
  (interactive)
  (let ((theme-name (haba/read-current-theme)))
    (when theme-name
      (haba/next-theme (intern theme-name)))))


;; default frame is fullscreen and has no scrollbars
(setq default-frame-alist '((fullscreen . maximized) (vertical-scroll-bars . nil)))

(add-hook 'kill-emacs-hook 'haba/save-current-theme)

(provide 'haba-appearance)
