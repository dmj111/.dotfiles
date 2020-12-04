;;;; auto-complete
(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories
               (expand-file-name "ac-dict" *config-dir*))
  (ac-config-default)
  ;; Trigger key
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))
