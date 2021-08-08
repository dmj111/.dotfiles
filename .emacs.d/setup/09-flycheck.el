(use-package flycheck
  :ensure t
  :config
  ;; TODO [ ] https://github.com/abo-abo/hydra/wiki/Flycheck
  ;; Force flycheck to always use c++11 support. We use
  ;; the clang language backend so this is set to clang

    ;; Turn flycheck on everywhere
  ;; (global-flycheck-mode)
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-clang-language-standard "c++14")))

  ;; Requires pylint and flake8 to be installed.
  (flycheck-add-next-checker `python-pylint '(warning . python-flake8))
  (setq  flycheck-python-flake8-executable "flake8"))
