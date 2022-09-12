;;; 10-avy -- Summary
;;; Commentary:
;;; Code:
;; allows quick jumping to points on the screen

(use-package avy
  :bind (
         ("C-:" . avy-goto-char)
         :map dmj-map
         ("w" . avy-goto-word-1)
         ("1" . avy-goto-char-timer)))
(provide '10-avy)
;;; 10-avy.el ends here
