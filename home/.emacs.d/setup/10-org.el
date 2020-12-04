(use-package org
  :bind (([f6] . org-capture))
  :config
  (message "first use-package-org")

  (add-hook 'org-mode-hook (lambda ()
                             (auto-fill-mode 1)))

  ;; http://endlessparentheses.com/inserting-the-kbd-tag-in-org-mode.html?source=rss

  (define-key org-mode-map "\C-ck" #'endless/insert-key)
  (defun endless/insert-key (key)
    "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
    (interactive "kType key sequence: ")
    (let* ((is-org-mode (derived-mode-p 'org-mode))
           (tag (if is-org-mode
                    "@@html:<kbd>%s</kbd>@@"
                  "<kbd>%s</kbd>")))
      (if (null (equal key "
"))
          (insert
           (format tag (help-key-description key nil)))
        (insert (format tag ""))
        (forward-char (if is-org-mode -8 -6)))))

  (setq org-export-htmlize-output-type 'css)
  (setq org-src-fontify-natively t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-directory "~/code/private")
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)

  (setq org-default-notes-file "log.org")
  (define-key global-map "\C-cc" 'org-capture)


  (setq org-capture-templates dmj-org-capture-templates)
  ;; http://doc.norang.ca/org-mode.html#Refiling
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets (quote ((nil :maxlevel . 3))))


  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; (sh . t)
     (emacs-lisp . t)
     (python . t)
     )))


;; (use-package org-plus-contrib-autoloads
;;   :commands org-drill)

;; (use-package org-drill
;;   :config
;;   (setq org-drill-add-random-noise-to-intervals-p t))
