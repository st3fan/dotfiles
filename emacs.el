;;

(unless (< 24 emacs-major-version)
  (error "requires Emacs 24 or later."))

;;

(setq gc-cons-threshold (* 64 1024 1024))

;;

(setq custom-file "~/.emacs.custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; ==========================================================================
;; Bootstrap MELPA and use-package
;; ==========================================================================

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; The following paths do not exist everywhere but they are common enough to add without checks.

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "~/go/bin")

;; ==========================================================================
;; Configure packages
;; ==========================================================================

(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :config (setq whitespace-line-column 120 whitespace-style '(face trailing lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode) (add-hook 'cc-mode-hook 'whitespace-cleanup-mode))

(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind ("C-c g" . magit-status))

(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

(use-package expand-region
  :init (global-set-key (kbd "M-e") 'er/expand-region))

;; TODO This should inherit from the shell instead
;; (setenv "GOPATH" "/Users/stefan/go")
;; (add-to-list 'exec-path "/Users/stefan/go/bin")

;;
;; Install the following tools:
;;
;;   go get -u -v golang.org/x/tools/cmd/goimports
;;   go get -u -v github.com/nsf/gocode
;;

(use-package go-mode
  :ensure t
  :init
    (add-hook 'go-mode-hook
	      (lambda ()
		(setq gofmt-command "goimports")
		(add-hook 'before-save-hook 'gofmt-before-save)
		(setq truncate-lines t)
		(setq indent-tabs-mode t)
		(setq tab-width 4))))

(use-package go-eldoc
  :ensure t
  :init (add-hook 'go-mode-hook (lambda ()
                                  (go-eldoc-setup)))) ;; TODO better way?

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook 'global-company-mode))

(use-package company-go
  :ensure t
  :init (add-hook 'go-mode-hook (lambda ()
                                  (set (make-local-variable 'company-backends) '(company-go))
                                  (company-mode))))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode))

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))

;; (use-package cyberpunk-theme
;;   :ensure t
;;   :config (load-theme 'cyberpunk t))

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :init
;;     (require 'color-theme-sanityinc-tomorrow)
;;     (color-theme-sanityinc-tomorrow-bright))

(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox t))

;; (use-package git-gutter
;;   :ensure t
;;   :diminish git-gutter-mode
;;   :config (global-git-gutter-mode))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-fringe-mode
  :config (global-git-gutter-mode))

(use-package paren
  :ensure t
  :init (show-paren-mode)
  :config (setq show-paren-when-point-inside-paren nil
                show-paren-when-point-in-periphery t))

(use-package hl-line
  :ensure t
  :init (global-hl-line-mode))

(use-package go-playground
  :ensure t
  :config (setq go-playground-basedir "~/Go/src/github.com/st3fan/goplaygrounds"
                go-playground-ask-for-file-name t)
  :bind ("C-c r" . go-playground-save-and-run))

;; (use-package yasnippet
;;   :ensure t
;;   :config (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;;   :init (yas-global-mode 1))

(use-package cc-mode
  :config (setq c-default-style "ellemtel"))

;; (use-package helm
;;   :ensure t
;;   :config (helm-mode t))

;; ==========================================================================
;; Random customizations
;; ==========================================================================

(setq user-full-name "Stefan Arentz")
(setq user-mail-address "stefan@arentz.ca")

(transient-mark-mode 1)                 ; highlight text selection
(delete-selection-mode 1)               ; delete seleted text when typing

(column-number-mode 1)

(setq make-backup-files nil)            ; stop creating those backup~ files
(setq auto-save-default nil)            ; stop creating those #autosave# files

(recentf-mode 1)                        ; keep a list of recently opened files

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

;; (when (display-graphic-p)
;;   (set-fringe-style 0))                 ; no fringe

(blink-cursor-mode 0)                   ; no blinking cursor please

(setq ring-bell-function 'ignore)       ; stop beeping

(desktop-save-mode 0)                   ; save/restore opened files
(recentf-mode 1)                        ; keep a list of recently opened files

;; ==========================================================================
;; Finally load a 'local' config, which is not stored in version
;; control.  This file can contain things like API keys for example.
;; ==========================================================================

(when (file-exists-p "~/.emacs.local.el")
  (load "~/.emacs.local.el"))
