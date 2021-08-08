(use-package ace-window
  :bind ([(f12)] . ace-window))

;; http://irreal.org/blog/?p=760
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode)
  :config
  (add-hook 'ace-jump-mode-before-jump-hook
            (lambda () (push-mark (point) t))))
