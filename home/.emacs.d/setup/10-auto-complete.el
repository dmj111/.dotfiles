;;; 10-auto-complete -- Summary
;;; Commentary:
;;; Code:
;;;; auto-complete
(use-package auto-complete
  :disabled t
  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories
               (expand-file-name "ac-dict" my-config-dir))
  (ac-config-default)
  ;; Trigger key
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))
(provide '10-auto-complete)
;;; 10-auto-complete.el ends here
