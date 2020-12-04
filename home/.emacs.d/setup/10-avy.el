;; allows quick jumping to points on the screen

(use-package avy
  :ensure t
  :bind (
         ("C-:" . avy-goto-char)
         :map dmj-map
         ("w" . avy-goto-word-1)
         ("1" . avy-goto-char-timer)))
