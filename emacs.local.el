
;;
;; All of this should really move to emacs.el
;;

(custom-set-faces
 '(info-xref ((t (:inherit link :foreground "yellow"))))
 '(linum ((t (:background "#282828" :foreground "DarkOrange4" :height 0.7))))
 '(company-scrollbar-bg ((t (:background "#414141"))))
 '(company-scrollbar-fg ((t (:background "#343434"))))
 '(company-template-field ((t (:background "lime green" :foreground "black" :underline nil :weight light))))
 '(company-tooltip ((t (:inherit default :background "#2d2d2d"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))

(when (memq window-system '(mac ns))
  (setq-default left-fringe-width  10)
  (fringe-helper-define 'git-gutter-fr:added nil
    "........"
    "........"
    "...X...."
    "...X...."
    ".XXXXX.."
    "...X...."
    "...X...."
    "........")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "........"
    "........"
    "........"
    "........"
    ".XXXXX.."
    "........"
    "........"
    "........")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "........"
    "........"
    "........"
    "..XXX..."
    "..XXX..."
    "..XXX..."
    "........"
    "........"))

(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))


