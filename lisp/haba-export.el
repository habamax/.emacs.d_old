(provide 'haba-export)

(defun haba/pandoc-md2html ()
  "Compile markdown file to HTML, using pandoc."
  (interactive)
  (when buffer-file-name
    (message "Pandoc markdown to HTML compilation...")
    (shell-command
     (concat "pandoc"
             " -o "
             (concat (file-name-base) ".html")
             " "
             buffer-file-name))))
