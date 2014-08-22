;; init.el

;; The following makes emacs run GC less frequently. Specially since
;; startup. The default is set very low which causes GC during package
;; loading many many times.

(setq gc-cons-threshold (* 32 1024 1024))

;; My setup really only works on Emacs 24 and up. So give up right
;; away if we are on an older version. Bummer.

(unless (>= 24 emacs-major-version)
  (error "requires Emacs 24 or later."))

;; TODO: Remove that silly st3fan- prefix from all files.

(progn
  (add-to-list 'load-path "~/.emacs.d/st3fan/")
  (load "st3fan-packages.el")
  (load "st3fan-paths.el")
  (load "st3fan-theme.el")
  (load "st3fan-whitespace.el")
  (load "st3fan-clojure.el")
  (load "st3fan-go.el")
  ;;(load "st3fan-flycheck.el")
  (load "st3fan-custom.el")
  (load "st3fan-functions.el")
  (add-to-list 'load-path "~/.emacs.d")
  (load "init.local.el"))

;; Store custom-set-variables in it's own file instead of here

(when (file-exists-p "~/.emacs.d/custom.el")
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file))
