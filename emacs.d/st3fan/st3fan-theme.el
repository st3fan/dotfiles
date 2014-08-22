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

;; Change font to Menlo if we are on OS X

(when (eq window-system 'ns)
  (when (eq system-type 'darwin)
    (custom-set-faces
     '(default ((t (:height 110 :family "Menlo")))))))

;; Some custom faces that I like on top of flatui

(when (eq window-system 'ns)
  (custom-set-faces
   '(default ((t (:height 110 :family "Menlo"))))
   '(mode-line ((t (:background "gridColor" :foreground "labelColor" :box nil))))
   '(mode-line-buffer-id ((t (:foreground "#e74c3c" :weight normal))))
   '(mode-line-highlight ((t nil)))
   '(mode-line-inactive ((t (:background "controlHighlightColor" :foreground "scrollBarColor" :box nil))))))

;; Change the window size - Should do this based on screen size

(when (eq window-system 'ns)
  (set-frame-height (selected-frame) 42)
  (set-frame-width (selected-frame) 120))
