;; st3fan-custom.el

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(transient-mark-mode 1)                 ; highlight text selection
(delete-selection-mode 1)               ; delete seleted text when typing

(show-paren-mode 1)                     ; turn on paren match highlighting
(setq show-paren-delay 0)               ; disable delay
(setq show-paren-style 'parenthesis)    ; highlight entire bracket expression

(column-number-mode 1)

(setq make-backup-files nil)            ; stop creating those backup~ files
(setq auto-save-default nil)            ; stop creating those #autosave# files

(recentf-mode 1)                        ; keep a list of recently opened files

(global-hl-line-mode 1)                 ; turn on highlighting current line

(setq inhibit-splash-screen t)          ;

(setq display-time-24hr-format 1)       ;
(display-time-mode 1)                   ;

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

;;
(add-hook 'go-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)))

;;


;; ack-and-a-half

(require 'ack-and-a-half)

(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; dired

(setq dired-listing-switches "-alh")

;; Change the *scratch* buffer to text mode.

(setq initial-major-mode 'text-mode)

(setq initial-scratch-message "\
# This buffer is for notes you don't want to save. If you want to
# create a file, visit that file with C-x C-f, then enter the text
# in that file's own buffer.\n")

;; magit

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

;; Disable mouse wheel (and two finger swipe) scrolling because it scrolls horribly

(mouse-wheel-mode -1)

(global-set-key [wheel-up] 'ignore)
(global-set-key [wheel-down] 'ignore)

(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)

(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)
