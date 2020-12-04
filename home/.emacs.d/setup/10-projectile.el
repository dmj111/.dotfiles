(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (projectile-mode t))
