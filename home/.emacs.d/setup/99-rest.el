;; Add a local lisp directory to the load path.
(add-to-list 'load-path *local-dir*)

;; Delete soon
;; Taken from http://milkbox.net/note/single-file-master-emacs-configuration/
;;
;; Make eval-after-load a little nicer.  For elpa autoloads, call it
;; with a string name.  For require's, call it with a symbol.
;; (defmacro after (mode &rest body)
;;   "`eval-after-load' MODE evaluate BODY."
;;   (declare (indent defun))
;;   `(eval-after-load ,mode
;;      '(progn
;;         (message "after loading %s" ,mode)
;;         ,@body)))

;;;; Utilities
;;;; package stuff..
(require 'cl)

(use-package dash :ensure t)
(use-package markdown-mode :ensure t)

;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode))))

;;;; Theme


;;;; Hippie-expand
;; From emacs-prelude
;; hippie expand is dabbrev expand on steroids

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))


;;;; magit
(use-package magit
  :ensure t
  :bind (("\C-xg" . magit-status))
  :config

  ;; (eval-after-load 'swiper
  ;;   (setq magit-completing-read-function 'ivy-completing-read))

  ;; full-scrreen magit-status
  ;; from magnars --
  ;;    https://github.com/magnars/.emacs.d/blob/master/setup-magit.el
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))


(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (projectile-mode t))


;;;; python
(use-package python
  :ensure t
  :init
  (defun my-python-mode-hook ()
    "Stuff to run when python-mode loads"
    (setq-default py-indent-offset 4)
    (setq indent-tabs-mode nil)
    ;;(setq py-python-command nil)
    ;;(require 'virtualenv)
    (transient-mark-mode t)
    (message "ran my-python-mode-hook"))

  (add-hook 'python-mode-hook 'my-python-mode-hook)
  ;; (use-package flymake-python-pyflakes-autoloads
  ;;   :init
  ;;   (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

  ;; (use-package jedi-autoloads
  ;;   :init
  ;;   (add-hook 'python-mode-hook 'jedi:setup)
  ;;   (setq jedi:setup-keys t)
  ;;   (setq jedi:complete-on-dot t))
  )

;;;; Shell-script mode

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;;;; elisp

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defun imenu-elisp-sections ()
  "Might as well have a doc  string."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)


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




;;; Custom file
(when (file-exists-p custom-file)
  (load custom-file))

;;;; programming mode
;; TODO: flyspell on mac!!
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))


;; Used this at previous job - not sure if still want it.
;; (add-hook 'prog-mode-hook
;;           (lambda () (add-hook 'before-save-hook
;;                                'whitespace-cleanup nil t)))

;;;; uniquify

;; (use-package uniquify
;;   :config
(setq uniquify-buffer-name-style 'forward)

;;;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer--timestamps t))


;;;; smex
(use-package smex
  :ensure t
  :config
  (smex-initialize))


;;;; js2
(use-package js2-mode
  :ensure t
  :mode (("\\.json\\'" . js-mode)
         ("\\.js\\'" . js2-mode))
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-highlight-level 3))

(use-package ac-js2
  :hook js2-mode-hook)

;;;; auto-complete
(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories
               (expand-file-name "ac-dict" *config-dir*))
  (ac-config-default)
  ;; Trigger key
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))


(defun add-to-list-if-file-exists (list path)
  (when (file-exists-p path) (add-to-list list path))
  )
;;;; yasnippet
(use-package yasnippet
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'snippet-mode-hook 'yas-minor-mode)

  :config
  ;; yas-reload-all
  (add-to-list-if-file-exists
   'yas-snippet-dirs (expand-file-name "snippets" *config-dir*))
  (add-to-list-if-file-exists
   'yas-snippet-dirs (expand-file-name "snippets" *local-dir*))
  (add-to-list-if-file-exists
   'yas-snippet-dirs (expand-file-name "~/code/templates/snippets"))
  (yas-reload-all)
  (yas-global-mode -1)
  (setq yas-prompt-functions
        '(yas-ido-prompt
          yas-x-prompt
          yas-dropdown-prompt
          yas-completing-prompt
          yas-no-prompt)))

;;;; c++

(use-package c++-mode
  :mode "\\.h\\'")


;;;; cpputils-cmake
(use-package cpputils-cmake
  :ensure t)


(use-package google-c-style
  :ensure t
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style))

(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)

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
     ))

)

;; (use-package org-plus-contrib-autoloads
;;   :commands org-drill)

;; (use-package org-drill
;;   :config
;;   (setq org-drill-add-random-noise-to-intervals-p t))


(use-package ace-window
  :bind ([(f12)] . ace-window))

;; http://irreal.org/blog/?p=760
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode)
  :config
  (add-hook 'ace-jump-mode-before-jump-hook
            (lambda () (push-mark (point) t))))


(use-package lacarte
  :bind ([?\M-`] . lacarte-execute-command))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;; [ ] https://github.com/abo-abo/swiper
;; [ ] http://oremacs.com/swiper/
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer)
   :map ivy-mode-map
   ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order)))
  ;;(setq ivy-count-format "(%d/%d) ")
  )



(use-package counsel :ensure t)
;; commands
;; counsel-ag grep
;; counsel-recentf
;; counsel-git-grep
;; counsel-git find file
;; counsel-describbindings
;; counsel-yank-pop
;; counsel-projectile

(use-package swiper
  :ensure t
  ;; C-j to select current
  ;; C-M-j to select current value (creat new file)
  ;; M-j to select word at point.
  :bind (("C-s" . swiper)
         ("C-x C-r" . counsel-recentf)
         ("M-x" . counsel-M-x)
         ("\C-x\C-m" . counsel-M-x)
         ("\C-xm" . counsel-M-x)
         ("\C-c\C-m" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox)
         ("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  )

(use-package avy
  :ensure t
  :bind (
         ("C-:" . avy-goto-char)
         :map dmj-map
         ("w" . avy-goto-word-1)
         ("1" . avy-goto-char-timer)))

;;;; winner
(winner-mode t)

;;;; open-with

(when *is-mac*
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

(defun clear-kill-ring ()
  "Clear all entries from the kill ring."
  (interactive)
  (setq kill-ring nil)
  (garbage-collect))

(defalias 'list-buffers 'ibuffer)

;; server
(require 'server)
(unless (server-running-p)
  (server-start))


(eval-after-load 'apropos '(setq apropos-sort-by-scores t))



;; http://stackoverflow.com/questions/20967818/emacs-function-to-case-insensitive-sort-lines
(defun sort-lines-nocase ()
  "Sort lines, case insensitive."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))


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



(global-set-key (kbd "M-i") 'imenu)

;;(set-frame-font "Cousine-14" nil t)


(unless t
  (set-frame-font "inconsolata-14")
  (set-frame-font "monaco-12")
  (set-frame-font "menlo-12")
  (set-frame-font "-*-Andale Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
  (set-frame-font "Cousine-14")
  (set-frame-font "source code pro-12")
  ;; Test string for fonts:
  ;; abcdefghijklmnopqrstuvqxyz
  ;; ABCDEFGHIJKLMNOPQRSTUVQXYZ
  ;; 0123456789
  ;; O, o, 0
  ;; 1 I i L l
  ;; 5 S s
  ;; 2 Z z
  ;; ( { [ ] } )
)

;; google-this
(use-package google-this
  :ensure t
  :config
  (google-this-mode 1))

(use-package wttrin
  :commands (wttrin)
  :config
  (setq wttrin-default-cities '("State College, Pennsylvania")))


;; https://github.com/abo-abo/hydra
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  )


(use-package conda
  :ensure t
  :config
  ;; Make sure pylint is installed.
  ;; make sure a default environment is set
  ;;   conda env export > environment.yml
  ;; https://github.com/necaris/conda.el
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  (conda-env-autoactivate-mode t)
  ;; If your Anaconda installation is anywhere other than the default (~/.anaconda3) then set the conda-anaconda-home custom variable to the installation path. For instance, in my configuration I have:

  (custom-set-variables
   '(conda-anaconda-home *anaconda-directory*)))


;; http://jblevins.org/log/mmm
(defun my-mmm-markdown-auto-class (lang &optional submode)
  "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "^```" lang "[\n\r]+"))
        (back "^```"))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)))


(use-package mmm-mode
  :ensure t
  :config
  (setq mmm-global-mode 'maybe)

  ;; Note: rememb
  ;; Mode names that derive directly from the language name
  (mapc 'my-mmm-markdown-auto-class
        '("c" "cpp" "css" "html" "latex" "lisp" "makefile"
          "markdown" "python" "r" "xml"))
  (global-set-key (kbd "C-c m") 'mmm-parse-buffer)
  )



(add-to-list 'exec-path "/usr/local/opt/llvm/bin/" t)

;; https://nilsdeppe.com/posts/emacs-c++-ide

(use-package flycheck
  :ensure t
  :init
  ;; TODO [ ] https://github.com/abo-abo/hydra/wiki/Flycheck
  ;; Force flycheck to always use c++11 support. We use
  ;; the clang language backend so this is set to clang

    ;; Turn flycheck on everywhere
  ;; (global-flycheck-mode)
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-clang-language-standard "c++14")))

  ;; Requires pylint and flake8 to be installed.
  (flycheck-add-next-checker `python-pylint '(warning . python-flake8))
  (setq  flycheck-python-flake8-executable "flake8"))

;; Make sure clang-tidy is on exec path
(use-package flycheck-clang-tidy
  :ensure t
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup)))
;; (use-package heml-rtags
;;   :ensure t
;;   :config
;;   (setq rtags-use-helm t))

(use-package company
  :disabled t
  :config
  (global-company-mode 1))


;; To get tabs in c++ code
;; // L o c a l Variables:
;; // *cpp-auto-clang-format*: nil
;; // indent-tabs-mode: t
;; // c-basic-offset: 4
;; // tab-width: 4
;; // whitespace-mode: t
;; // End:


(use-package clang-format
  :ensure t
  :config
  (defvar *cpp-auto-clang-format* t  "Auto-clang format on save hook.")
  (defun my-add-clang-format-hook ()
    (add-hook 'before-save-hook
              (lambda ()
                (when *cpp-auto-clang-format* (clang-format-buffer)))
              nil t))
  (add-hook 'c++-mode-hook #'my-add-clang-format-hook)
  (global-set-key [C-M-tab] 'clang-format-region))


(use-package ledger-mode
  :mode "\\.ledger$")


(use-package move-text
  :ensure t
  :config
  (global-set-key [\M-\S-up] 'move-text-up)
  (global-set-key [\M-\S-down] 'move-text-down))

(use-package wgrep
  :ensure t)

;; wgrep replacement :
;; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
;; counsel-ag,
;; C-c C-o ivy occur
;; C-x C-q - wgrep
;;


;; http://syamajala.github.io/c-ide.html

;; Load the local file, if it exists.
(require 'init-local nil t)


(define-coding-system-alias 'UTF-8 'utf-8)

;; TODO require-after-load is using this...
(provide 'init)


;; Consider something like this in local:
;;;; Example init-local.el
;;(add-to-list 'exec-path "/usr/local/bin" t)
;;(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;;(setq my-default-theme 'zenburn)
;; (provide 'init-local)

;;; init.el ends here

;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary
