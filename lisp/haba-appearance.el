(defvar *haba-theme-dark* 'kosmos)
(defvar *haba-theme-light* 'leuven)
(defvar *haba-current-theme* *haba-theme-dark*)


(defun haba/set-font (fonts-list font-size)
  (catch 'loop
    (dolist (font fonts-list)
      (when (find-font (font-spec :name font))
        (progn
          (set-face-attribute 'default nil :family font :height font-size)
          (throw 'loop font))))))



;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))


(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))


(defun haba/next-theme (theme)
  (if (eq theme 'default)
      (disable-theme *haba-current-theme*)
    (progn
      (load-theme theme t)))
  (setq *haba-current-theme* theme))

(defun haba/toggle-theme ()
  (interactive)
  (cond ((eq *haba-current-theme* *haba-theme-dark*) (haba/next-theme *haba-theme-light*))
        ((eq *haba-current-theme* *haba-theme-light*) (haba/next-theme 'default))
        ((eq *haba-current-theme* 'default) (haba/next-theme *haba-theme-dark*))))


(defun haba/read-current-theme ()
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents (concat user-emacs-directory "current-theme"))
      (buffer-string))))

(defun haba/save-current-theme ()
  (ignore-errors
    (with-temp-file (concat user-emacs-directory "current-theme")
      (insert (symbol-name *haba-current-theme*)))))


(let ((theme-name (haba/read-current-theme)))
  (if theme-name
      (haba/next-theme (intern theme-name))
    (haba/next-theme *haba-current-theme*)))


(when window-system
  (setq default-frame-alist '((fullscreen . maximized)))
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  (let ((font-size (if (string-equal (system-name) "MKIM") 110 140)))
    (haba/set-font '("Input" "Menlo" "Roboto Mono" "Dejavu Sans Mono" "Fira Mono" "Consolas")
                   font-size))
  )



(add-hook 'kill-emacs-hook 'haba/save-current-theme)

(provide 'haba-appearance)
