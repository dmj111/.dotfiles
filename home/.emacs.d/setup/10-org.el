(use-package org
  :bind (([f6] . org-capture))
  :config
  (message "first use-package-org")

  ;; note - C-c C-, instead of <s for newer org

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
     ))


  ;; Make it look nicer
  ;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  ;;
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (let* ((variable-tuple
          (cond ((x-list-fonts "Avenir")         '(:font "Avenir"))
                ((x-list-fonts "Charter")         '(:font "Charter"))
                ((x-list-fonts "Palatino")         '(:font "Palatino"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Charter" :height 180 :weight thin))))
   '(fixed-pitch ((t ( :family "Fira Code" :height 160)))))

  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))


  (setq org-hide-emphasis-markers t))




(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))



;; (use-package org-plus-contrib-autoloads
;;   :commands org-drill)

;; (use-package org-drill
;;   :config
;;   (setq org-drill-add-random-noise-to-intervals-p t))
