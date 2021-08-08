
(use-package recentf
  :ensure t
  :bind
  (("C-x C-r" . recentf-ido-find-file))
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15)
  (recentf-mode 1)
  ;; (move to another file to setup an autoload?)
  (defun recentf-ido-find-file ()
    "Find recent file with ido."
    (interactive)
    (let ((file (ido-completing-read
                 "Chose recent file:"
                 (-map 'abbreviate-file-name recentf-list) nil t)))
      (when file (find-file file)))))
