;;;; js2

(use-package js2-mode
  :mode (("\\.json\\'" . js-mode)
         ("\\.js\\'" . js2-mode))
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-highlight-level 3))

(use-package ac-js2
  :hook js2-mode-hook)
