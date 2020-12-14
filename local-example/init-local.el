(add-to-list 'exec-path "/usr/local/bin" t)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq ispell-program-name "/usr/local/bin/aspell")

;; (set-frame-font "source code pro-13")
(set-frame-font "fira code-14")

(use-package zenburn-theme :ensure t)
;; (add-to-list 'custom-theme-load-path "/Users/dave/.local-dotfiles/.emacs.d/emacs-color-theme-solarized")
;; (setq my-default-theme 'solarized)


(add-to-list 'exec-path "/usr/local/opt/llvm/bin/" t)

(provide 'init-local)
;;; init-local.el ends here
