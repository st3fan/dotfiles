;; st3fan-clojure.el

(setq nrepl-hide-special-buffers t)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.cider-repl-history")

(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
