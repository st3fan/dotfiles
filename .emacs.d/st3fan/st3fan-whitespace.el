;; st3fan-whitespace.el

(setq whitespace-line-column 100)
(setq whitespace-style '(face trailing lines-tail))

(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'clojure-mode-hook 'whitespace-mode)
(add-hook 'c-mode-hook 'whitespace-mode)
(add-hook 'c++-mode-hook 'whitespace-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))
