(use-package projectile
  :ensure t
  :after ivy
  :diminish projectile-mode
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  ;; (setq projectile-project-search-path '("~/code" "~/.dotfiles"))
  (projectile-mode t))
