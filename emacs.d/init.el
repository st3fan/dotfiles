;; init.el

;; My setup really only works on Emacs 24 and up. So give up right
;; away if we are on an older version. Bummer.


(if (< emacs-major-version 24)
  (progn
      (message "This init.el only works on Emacs 24 and newer."))
  (progn
    (add-to-list 'load-path "~/.emacs.d/st3fan/")
    (load "st3fan-packages.el")
    (load "st3fan-paths.el")
    (load "st3fan-theme.el")
    (load "st3fan-whitespace.el")
    (load "st3fan-clojure.el")
    (load "st3fan-go.el")
    (load "st3fan-custom.el")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("c739f435660ca9d9e77312cbb878d5d7fd31e386a7758c982fa54a49ffd47f6e" "1ce0ce1c263b3da9ddac74143ca8de896b4f754f657d8cef263abcc2d658e7c7" "50edb7914e8d369bc03820d2dcde7e74b7efe2af5a39511d3a130508e2f6ac8f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
