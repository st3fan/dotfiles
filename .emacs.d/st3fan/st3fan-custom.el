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

(when (not (featurep 'ns-win))          ; disable the menu when running in the terminal
  (menu-bar-mode -1))
(tool-bar-mode -1)                      ; kill the toolbar
(scroll-bar-mode -1)                    ; kill the scrollbar
(blink-cursor-mode 0)                   ; no blinking cursor please
(set-fringe-style 0)                    ; no fringe

(desktop-save-mode 1)                   ; save/restore opened files
(recentf-mode 1)                        ; keep a list of recently opened files

;;

(require 'expand-region)
(global-set-key (kbd "M-e") 'er/expand-region)

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