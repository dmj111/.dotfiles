(defun add-to-list-if-file-exists (list path)
  (when (file-exists-p path) (add-to-list list path)))

;;;; yasnippet
(use-package yasnippet
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'snippet-mode-hook 'yas-minor-mode)

  :config
  ;; yas-reload-all
  (add-to-list-if-file-exists
   'yas-snippet-dirs (expand-file-name "snippets" my-config-dir))
  (add-to-list-if-file-exists
   'yas-snippet-dirs (expand-file-name "snippets" my-local-dir))
  (add-to-list-if-file-exists
   'yas-snippet-dirs (expand-file-name "~/code/templates/snippets"))
  (yas-reload-all)
  (yas-global-mode -1)

  (setq yas-prompt-functions
        '(yas-ido-prompt
          yas-x-prompt
          yas-dropdown-prompt
          yas-completing-prompt
          yas-no-prompt)))
