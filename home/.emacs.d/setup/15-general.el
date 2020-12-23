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


(defvar *my-frame-fonts*
  '(
    "fira code-14"
    "source code pro-13"
    "cousine-14"
    )
  "List of fonts to try. Add to this list in init-local and
the font will be set after init is loaded")


(when (display-graphic-p)
  (eval-after-load 'init
    '(reduce (lambda (acc font)
               (or acc (ignore-errors
                         (set-frame-font font) font)))
             *my-frame-fonts*
             :initial-value nil)))



;; https://github.com/abo-abo/hydra
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  )
