;;; 10-paredit -- Summary
;;; Commentary:
;;; Code:
;; http://danmidwood.com/content/2014/11/21/animated-paredit.html

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode))))
(provide '10-paredit)
;;; 10-paredit.el ends here
