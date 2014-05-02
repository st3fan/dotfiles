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
    (load "st3fan-custom.el")))
