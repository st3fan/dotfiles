
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/")  t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(cyberpunk-theme
    flatui-theme
    clojure-mode
    clojure-test-mode
    cider                         ; https://github.com/clojure-emacs/cider
    gist                          ; https://github.com/defunkt/gist.el
    ack-and-a-half                ; https://github.com/jhelwig/ack-and-a-half
    go-mode
    smartparens
    expand-region
    rainbow-delimiters
    magit)
  "list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
