(setq mac-command-modifier 'control)

;; choose font
;; TODO: make a function with a loop
(cond 
 ((find-font (font-spec :name "Hack"))
  (set-face-attribute 'default nil
                      :family "Hack"
                      :height 160))
 ((find-font (font-spec :name "Menlo"))
  (set-face-attribute 'default nil
                      :family "Menlo"
                      :height 160))
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height 160)))


(setq default-frame-alist
      '(
	(width . 120)
	(height . 38)
	))
  


(provide 'haba-osx)
