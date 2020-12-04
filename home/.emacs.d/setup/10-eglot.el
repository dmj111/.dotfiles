;;; copied from:
;;; https://ddavis.io/posts/eglot-cpp-ide/

(defvar my-clangd-exe (executable-find "clangd")
  "clangd executable path")

(require 'projectile)
(defun my/projectile-proj-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package company :ensure t)

(use-package eglot
  :ensure t)

(require 'project)

(defun my/cpp-eglot-enable ()
  "enable variables and hooks for eglot cpp IDE"
  (interactive)
  (setq company-backends
        (cons 'company-capf
              (remove 'company-capf company-backends)))
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions
                 'my/projectile-proj-find-function))
  (add-to-list 'eglot-server-programs
               `((c++-mode) ,my-clangd-exe))
  (add-hook 'c++-mode-hook 'eglot-ensure))

(defun my/cpp-eglot-disable ()
  "disable hook for eglot"
  (interactive)
  (remove-hook 'c++-mode-hook 'eglot-ensure))
