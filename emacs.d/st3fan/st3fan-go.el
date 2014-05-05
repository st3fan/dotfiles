
(add-hook 'before-save-hook #'gofmt-before-save)

(add-hook 'go-mode-hook (lambda ()
                          (setq indent-tabs-mode t)
                          (setq tab-width 4)))
