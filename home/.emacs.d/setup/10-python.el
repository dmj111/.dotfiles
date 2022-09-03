;;;; python
(use-package python
  :init
  (defun my-python-mode-hook ()
    "Stuff to run when python-mode loads"
    (setq-default py-indent-offset 4)
    (setq indent-tabs-mode nil)
    ;;(setq py-python-command nil)
    ;;(require 'virtualenv)
    (transient-mark-mode t)
    (message "ran my-python-mode-hook"))

  (add-hook 'python-mode-hook 'my-python-mode-hook)
  ;; (use-package flymake-python-pyflakes-autoloads
  ;;   :init
  ;;   (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

  ;; (use-package jedi-autoloads
  ;;   :init
  ;;   (add-hook 'python-mode-hook 'jedi:setup)
  ;;   (setq jedi:setup-keys t)
  ;;   (setq jedi:complete-on-dot t))
  )
