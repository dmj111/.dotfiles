
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(use-package wgrep
  :ensure t)

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

; (set-frame-font "Cousine-14" nil t)
; (set-frame-font "source code pro-13")
(set-frame-font "fira code-14")


;; https://github.com/abo-abo/hydra
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  )
