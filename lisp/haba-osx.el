(setq mac-command-modifier 'control)

(let ((path
       (concat (getenv "HOME") "/bin:" (getenv "PATH"))))
  (setenv "PATH" path)
  (setq exec-path (split-string path ":")))

(set-face-attribute 'default nil
		    :family "Menlo"
		    :height 160)

(setq default-frame-alist
      '(
	(width . 120) ; character
	(height . 38) ; lines
	))
  


(provide 'haba-osx)
