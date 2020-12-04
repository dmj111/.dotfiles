
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(use-package wgrep
  :ensure t)

;; wgrep replacement :
;; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
;; counsel-ag,
;; C-c C-o ivy occur
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

;;(set-frame-font "Cousine-14" nil t)

;; https://github.com/abo-abo/hydra
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  )
