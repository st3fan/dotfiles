;; st3fan-theme.el

(if (eq window-system 'ns)
  (load-theme 'flatui t)
  (load-theme 'cyberpunk t))

(when (eq window-system 'ns)
  (set-frame-height (selected-frame) 40)
  (set-frame-width (selected-frame) 100)
  (custom-set-faces
   '(default ((t (:height 140 :width normal :family "Menlo"))))))
