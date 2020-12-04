;;;; User interface
(setq-default indent-tabs-mode nil)
(column-number-mode 1)
(show-paren-mode 1)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;;; enable features

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (quote [M-down])  'scroll-up-line)
(global-set-key (quote [M-up])  'scroll-down-line)

(require 'cl)

;; http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/
;; unset C- and M- digit keys
(dotimes (n 9)
  (global-unset-key (kbd (format "C-%d" (+ 1 n))))
  (global-unset-key (kbd (format "M-%d" (+ 1 n)))))

(define-prefix-command 'dmj-map)
(global-set-key (kbd "C-1") 'dmj-map)

(define-key dmj-map (kbd "r") 'recompile)

(defun dmj-test-foo()
  "This is a silly test"
  (interactive)
  (message "foo"))
(define-key dmj-map (kbd "[") 'dmj-test-foo)
(define-key dmj-map (kbd "l") 'delete-trailing-whitespace)
(define-key dmj-map (kbd "w") 'whitespace-mode)
