





;;; 15-general -- Summary
;;; Commentary:
;;; Code:
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(use-package wgrep)

;; wgrep replacement :
;; C-x C-q - wgrep
;;

;; http://stackoverflow.com/questions/20967818/emacs-function-to-case-insensitive-sort-lines
(defun sort-lines-nocase ()
  "Sort lines, case insensitive."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(use-package lacarte
  :bind ([?\M-`] . lacarte-execute-command))


(global-set-key (kbd "M-i") 'imenu)


;; https://github.com/abo-abo/hydra
(use-package hydra
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  )
(provide '15-general)
;;; 15-general.el ends here
