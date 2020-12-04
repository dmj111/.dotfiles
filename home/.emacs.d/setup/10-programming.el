
;;;; programming mode
;; TODO: flyspell on mac!!
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))


;; http://stackoverflow.com/a/13408008
(use-package ansi-color
  :ensure t
  :init
  (defun colorize-compilation-buffer ()
    "Colorize compiler output."
    (read-only-mode -1)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (read-only-mode 1))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))



;; Used this at previous job - not sure if still want it.
;; (add-hook 'prog-mode-hook
;;           (lambda () (add-hook 'before-save-hook
;;                                'whitespace-cleanup nil t)))
