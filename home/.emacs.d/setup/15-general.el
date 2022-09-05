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


(defvar my-frame-fonts
  '(
    "input mono-14"
    "fira code-14"
    "source code pro-14"
    )
  "List of fonts to try. Add to this list in init-local and
the font will be set after init is loaded")


(defun try-set-frame-font (fonts)
  "Try setting frame-font to the values of fonts."
  (if (null fonts)
      (message "no more fonts to try")
    (let ((font (cl-first fonts))
          (rest (cl-rest fonts)))
      (if (ignore-errors (set-frame-font font) t)
          (message "set font to : %s" font)
        (message "failed to set font: %s" font)
        (try-set-frame-font rest)))))


(when (display-graphic-p)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (try-set-frame-font my-frame-fonts))))


;; https://github.com/abo-abo/hydra
(use-package hydra
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  )
