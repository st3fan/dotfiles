;; init.el

;; ==========================================================================
;; The following makes emacs run GC less frequently. Specially since
;; startup. The default is set very low which causes GC during package
;; loading many many times.
;; ==========================================================================

(setq gc-cons-threshold (* 32 1024 1024))

;; ==========================================================================
;; This setup really only works on Emacs 24 and up. So give up right
;; away if we are on an older version. Bummer.
;; ==========================================================================

(unless (>= 24 emacs-major-version)
  (error "requires Emacs 24 or later."))

;; ==========================================================================
;; Store custom-set-variables in it's own file instead of here
;; ==========================================================================

(when (file-exists-p "~/.emacs.d/custom.el")
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file))

;; ==========================================================================
;; Personal info
;; ==========================================================================

(setq user-full-name "Stefan Arentz")
(setq user-mail-address "stefan@arentz.ca")

;; ==========================================================================
;; Install packages from MELPA.
;; ==========================================================================

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(cyberpunk-theme
    flatui-theme
    clojure-mode
    clojure-test-mode
    cider                         ; https://github.com/clojure-emacs/cider
    yagist                        ; https://github.com/mhayashi1120/yagist.el
    ack-and-a-half                ; https://github.com/jhelwig/ack-and-a-half
    swift-mode                    ; https://github.com/chrisbarrett/swift-mode
    flycheck                      ; https://github.com/flycheck/flycheck
    multiple-cursors              ; https://github.com/magnars/multiple-cursors.el
    exec-path-from-shell          ; https://github.com/purcell/exec-path-from-shell
    go-mode
    go-eldoc
    web-mode
    smartparens
    expand-region
    rainbow-delimiters
    magit)
  "list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ==========================================================================
;; Initialize things from the UNIX env
;; ==========================================================================

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; ==========================================================================
;; Setup themes and UI changes
;; ==========================================================================

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

;; ==========================================================================
;; Multiple cursors
;; ==========================================================================

(require 'multiple-cursors)

;; ==========================================================================
;; Whitespace
;; ==========================================================================

(setq whitespace-line-column 120)
(setq whitespace-style '(face trailing lines-tail))

(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'clojure-mode-hook 'whitespace-mode)
(add-hook 'c-mode-hook 'whitespace-mode)
(add-hook 'c++-mode-hook 'whitespace-mode)

(defun st3fan/whitespace-hook ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(add-hook 'prog-mode-hook 'st3fan/whitespace-hook)

;; ==========================================================================
;; Clojure
;; ==========================================================================

(setq nrepl-hide-special-buffers t)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.cider-repl-history")

(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)

;; ==========================================================================
;; Go
;; ==========================================================================

(exec-path-from-shell-copy-env "GOPATH")

(add-hook 'before-save-hook #'gofmt-before-save)

(add-hook 'go-mode-hook (lambda ()
                          (setq truncate-lines t)
                          (setq indent-tabs-mode t)
                          (setq tab-width 4)
                          (setq compile-command "go build")
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                          (local-set-key (kbd "C-c k") #'recompile)
                          (go-eldoc-setup)
			  (set-face-attribute 'eldoc-highlight-function-argument nil :foreground "green")))

;; ==========================================================================
;; Backups and Autosave - I actually do like to have backup files, just not
;; littered everywhere. So we keep them in one place. Also store autosave
;; files. Lots of them.
;; ==========================================================================

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; ==========================================================================
;; IDO Mode
;; ==========================================================================

(ido-mode 1)
(setq ido-auto-merge-work-directories-length -1)

;; ==========================================================================
;; Customizations
;; ==========================================================================

(transient-mark-mode 1)                 ; highlight text selection
(delete-selection-mode 1)               ; delete seleted text when typing

(show-paren-mode 1)                     ; turn on paren match highlighting
(setq show-paren-delay 0)               ; disable delay
(setq show-paren-style 'parenthesis)    ; highlight entire bracket expression

(column-number-mode 1)

;;(setq make-backup-files nil)            ; stop creating those backup~ files
;;(setq auto-save-default nil)            ; stop creating those #autosave# files

(recentf-mode 1)                        ; keep a list of recently opened files

(global-hl-line-mode 1)                 ; turn on highlighting current line

(setq inhibit-splash-screen t)          ;

(unless (getenv "TMUX")                 ; don't display the time if we are
  (setq display-time-24hr-format 1)     ; running in tmux because it
  (display-time-mode 1))                ; will already display a clock

(setq-default indent-tabs-mode nil)     ;

(when (not (display-graphic-p))         ; disable the menu when running in the terminal
  (menu-bar-mode -1))

(when (display-graphic-p)
  (tool-bar-mode -1))                   ; kill the toolbar

(when (display-graphic-p)
  (scroll-bar-mode -1))                 ; kill the scrollbar

(when (display-graphic-p)
  (set-fringe-style 0))                 ; no fringe

(blink-cursor-mode 0)                   ; no blinking cursor please

(setq ring-bell-function 'ignore)       ; stop beeping

(desktop-save-mode 0)                   ; save/restore opened files
(recentf-mode 1)                        ; keep a list of recently opened files

;;

;;(require 'expand-region)
;;(global-set-key (kbd "M-e") 'er/expand-region)

;;

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

;;

;;(smartparens-global-mode t)

;; ==========================================================================
;; ack-and-a-half
;; ==========================================================================

(require 'ack-and-a-half)

(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; ==========================================================================
;; dired
;; ==========================================================================

(require 'dired-x)
(setq dired-listing-switches "-alh")

;; ==========================================================================
;; winner mode
;; ==========================================================================

(winner-mode 1)

;; ==========================================================================
;; Magit
;; ==========================================================================

(require 'magit)

(global-set-key (kbd "C-c C-g") 'magit-status)

(defadvice magit-status (around magit-fullscreen activate)
  "Make magit-status run alone in a frame."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; ==========================================================================
;; Disable scrolling because it is horrible
;; ==========================================================================

(when (display-graphic-p)
  (mouse-wheel-mode -1)
  (global-set-key [wheel-up] 'ignore)
  (global-set-key [wheel-down] 'ignore)
  (global-set-key [double-wheel-up] 'ignore)
  (global-set-key [double-wheel-down] 'ignore)
  (global-set-key [triple-wheel-up] 'ignore)
  (global-set-key [triple-wheel-down] 'ignore))

;; ==========================================================================
;; Random Functions
;; ==========================================================================

;; Taken from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/

(defun sma-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `sma-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'sma-move-beginning-of-line)

;; Taken from http://stackoverflow.com/a/10541426/56837

(defun sma-scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (unless (eq (window-start) (point-min))
    (scroll-down n)))

(defun sma-scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))

(global-set-key (kbd "ESC <down>") 'sma-scroll-up-in-place)
(global-set-key (kbd "ESC <up>") 'sma-scroll-down-in-place)

;; ==========================================================================
;; Finally load a 'local' config, which is not stored in version
;; control.  This file can contain things like API keys for example.
;; ==========================================================================

(when (file-exists-p "~/.emacs.local.el")
  (load "~/.emacs.local.el"))
