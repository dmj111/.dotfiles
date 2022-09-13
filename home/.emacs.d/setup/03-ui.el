;;; 03-ui -- Summary
;;; Commentary:
;;; Code:

(require 'cl-lib)

;; http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/
;; unset C- and M- digit keys
(dotimes (n 9)
  (global-unset-key (kbd (format "C-%d" (+ 1 n))))
  (global-unset-key (kbd (format "M-%d" (+ 1 n)))))

(define-prefix-command 'dmj-map)
(global-set-key (kbd "C-1") 'dmj-map)

(define-key dmj-map (kbd "r") 'recompile)

(defun dmj-test-foo()
  "This is a silly test"
  (interactive)
  (message "foo"))

(define-key dmj-map (kbd "[") 'dmj-test-foo)
(define-key dmj-map (kbd "l") 'delete-trailing-whitespace)
(define-key dmj-map (kbd "w") 'whitespace-mode)


;;;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  :custom
  ;; (undo-tree-visualizer-relative-timestamps t)
  (undo-tree-auto-save-history nil)
  (undo-tree-visualizer-timestamps t)
  )

(use-package move-text
  :config
  (global-set-key [\M-\S-up] 'move-text-up)
  (global-set-key [\M-\S-down] 'move-text-down))

;;;; open-with
(when my-is-mac
  ;; Copied from emacs-prelude
  (defun prelude-open-with ()
    "Simple function that allows us to open the underlying
file of a buffer in an external program."
    (interactive)
    (when buffer-file-name
      (shell-command (concat
                      (if (eq system-type 'darwin)
                          "open"
                        (read-shell-command "Open current file with: "))
                      " "
                      buffer-file-name))))

  (global-set-key (kbd "C-c o") 'prelude-open-with))


(use-package apropos
  :disabled t
  :config
  (apropos-sort-by-scores t)
  )

(provide '03-ui)
;;; 03-ui.el ends here
