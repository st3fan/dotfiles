;; st3fan-theme.el

;; This gem removes bold from all faces - http://stackoverflow.com/a/20693389

(defun remap-faces-default-attributes ()
  (let ((family (face-attribute 'default :family))
        (height (face-attribute 'default :height)))
    (mapcar (lambda (face)
              (face-remap-add-relative
               face :family family :weight 'normal :height height))
          (face-list))))

(when (display-graphic-p)
  (add-hook 'minibuffer-setup-hook 'remap-faces-default-attributes)
  (add-hook 'change-major-mode-after-body-hook 'remap-faces-default-attributes))

(if (eq window-system 'ns)
    (load-theme 'flatui t)
  (load-theme 'cyberpunk t))

(when (eq window-system 'ns)
  (when (eq system-type 'darwin)
    (set-frame-height (selected-frame) 42)
    (set-frame-width (selected-frame) 120)
    (custom-set-faces
     '(default ((t (:height 110 :family "Menlo")))))))
