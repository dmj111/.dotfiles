;;; init.el --- Emacs configuration file
;;; Commentary:

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary


;; stuff to practice:
;; - C-/ to undo
;; M-0 (to 9) instead of C-u 8
;; M-a, M-e
;; C-M (move by expression) f,b,d,u
;; C-M-s regexp forward search
;; M-m -- move back to indentation
;; C-u M-g M-g -- goto line in prev buffer (matches line number at point)
;; ace-jump

;;;; TO LEARN
;; TODO: ace-jump
;; http://www.emacswiki.org/emacs/ImenuMode#toc10 -ido powered imenu
;; http://masteringemacs.org/article/effective-editing-movement ETAGS
;; C-x C-n / C-u C-x C-n  set /unset goal column
;; subword mode for Camel Case
;;
;; - kmacro-set-counter and kmacro-insert-counter
;;   looks like my C-x C-k keybindings interfere...
;; - try avy mode instead of acejump
;; -
;; TODO
;; - js2 setup
;; -  http://emacs.stackexchange.com/questions/2867/how-should-i-change-my-workflow-when-moving-from-ido-to-helm

;;; Code:

;;;; Debugging settings.

;; Debug if there is an error
;; (setq debug-on-error t)

;;; TODO:
;;; Initial setup

;; Don't limit the print out of a variable
(setq eval-expression-print-length nil)
(setq inhibit-startup-screen t)
(setq visible-bell t)

;; Turn off mouse interface early in startup to avoid momentary display
;; The macro is for non-windowed emacs.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; make yes/no shorter, and a frequent alias.
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp)


(setq-default indent-tabs-mode nil)
(column-number-mode 1)
(show-paren-mode 1)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;;; enable features

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (quote [M-down])  'scroll-up-line)
(global-set-key (quote [M-up])  'scroll-down-line)



;;;; Keybindings

(global-set-key [(f5)] 'call-last-kbd-macro)
(global-set-key "\C-x\C-n" 'next-error)
(global-set-key (quote [S-M-down])  'next-error)
(global-set-key (quote [S-M-up])  'prev-error)

(global-set-key "\M-?" 'help)
(global-set-key "\C-cx" 'compile)
(global-set-key [(f9)] 'recompile)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;; Hmm... isearch.  and regexp.
;; (global-set-key "\M-s" 'isearch-forward-regexp)
;; (global-set-key "\M-r" 'isearch-backward-regexp)

;; backward-kill-word is a fast way to delete
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Sometimes M-x is just too hard to type.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; TODO - detect electric indent, and don't bother if it exists
(define-key global-map (kbd "RET") 'newline-and-indent)


;; backward-kill-word is a fast way to delete
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)




(defconst my-is-mac (eq system-type 'darwin))

;; This is for when alt is not meta.   I need my meta.
;; (setq x-alt-keysym 'meta)
(when my-is-mac
  (setq mac-command-modifier 'meta))

(setq uniquify-buffer-name-style 'forward)



;;; hippie-expand
;; From emacs-prelude
;; hippie expand is dabbrev expand on steroids
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;;; winner
(winner-mode t)

(define-coding-system-alias 'UTF-8 'utf-8)


(defun clear-kill-ring ()
  "Clear all entries from the kill ring."
  (interactive)
  (setq kill-ring nil)
  (garbage-collect))

(defalias 'list-buffers 'ibuffer)

;;; Fonts for windowed emacs


(defvar my-frame-fonts
  '(
    "input mono-14"
    "fira code-14"
    "source code pro-14"
    )
  "A list of fonts to try.
Add to this list in init-local and the font will be set after
init is loaded.")


(defun try-set-frame-font (fonts)
  "Try setting frame-font using first good value of FONTS."
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


;;; theme

(defvar my-default-theme 'ef-night
  "Default theme to load at startup.")

(add-hook 'emacs-startup-hook
            (lambda ()
              (when my-default-theme
                (message "loading the theme...")
                (load-theme my-default-theme t))))


;;; configuration settings
;; (error "error")

;; Give names to some config directories.
(defconst my-config-dir (file-name-directory load-file-name)
  "Root directory for the configuration.")

(defconst my-config-setup (file-name-as-directory (concat my-config-dir "setup"))
  "Root directory for the configuration.")

(add-to-list 'load-path (concat my-config-dir "lisp"))

(setq custom-file (expand-file-name "custom.el" my-config-dir))

(when (file-exists-p custom-file)
  (add-hook 'after-init-hook
            (lambda ()
              (load custom-file))))



(require 'cl-lib)

;; (defun my-el-files (dir)
;;   "Return list of el files in DIR, except auto-saves and custom.el."
;;   (cl-delete-if
;;    (lambda (f) nil
;;      (or  (string-match-p "/custom.el\\'" f)
;;           (string-match-p "~\\|#" f))
;;      )
;;    (file-expand-wildcards (file-name-concat  dir "*.el")))
;;   )

;; (defun my-load-el-files (dir)
;;   "Load .el files in DIR, except auto-saves and custom.el."
;;   (mapc 'load (my-el-files dir)))

;; (mapc 'load (file-expand-wildcards (concat  my-local-dir "*.el")))
;; (mapc 'load (file-expand-wildcards my-config-dir))



(defvar my-packages
  '(
    ansi-color
    avy
    clang-format
    company
    conda
    counsel
    cpputils-cmake
    crux
    dash
    diminish
    ef-themes
    elpy
    flycheck
    flycheck-clang-tidy
    google-c-style
    hydra
    ivy
    js2-mode
    magit
    markdown-mode
    mmm-mode
    move-text
    org-bullets
    paredit
    projectile
    python
    pyvenv
    rainbow-mode
    recentf
    smex
    swiper
    undo-tree
    use-package
    wgrep
    yasnippet
    )
  "Packages to install."
  )

;; my-packages can be modified in init-local-preload.el to add/remove
;; packages for a specific deployment.


;; START LOCAL CUSTOMIZATION
(require 'init-local-preload nil t)
;; END LOCAL CUSTOMIZATION


(require 'package)
(package-initialize)
(message "loading package stuff")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Force this call for errors like use-package- not found
;; (package-refresh-contents)
(unless package-archive-contents  (package-refresh-contents))

(defun my-load-packages (packages &optional refreshed)
  (when (not (null packages))
    (let ((pkg (car packages))
          (rest (cdr packages)))
      (if (package-installed-p pkg)
          (my-load-packages rest refreshed)
        (when (not refreshed)
          (message "refreshing package contents")
          (package-refresh-contents))
        (message "Installing ~a" pkg)
        (package-install pkg)
        (my-load-packages rest t)))))

(my-load-packages my-packages)

(require 'use-package)

(use-package does-not-exist
  :disabled)

(use-package dash)
(use-package diminish)

;; use-package quick notes:
;; :init  - before load
;; :config - after load
;; :bind - creates autoload
;; :commands - creates autoload
;; :bind-keymap
;; :mode, :keymap
;; :magic
;; :hook - add to hook
;; :if, :after, :requires (stops if not loaded)

;; Load local settings files

;; (my-load-el-files my-config-setup)

;;; ui setup
;; http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/
;; unset C- and M- digit keys
(dotimes (n 9)
  (global-unset-key (kbd (format "C-%d" (+ 1 n))))
  (global-unset-key (kbd (format "M-%d" (+ 1 n)))))

(defvar my-map nil "Hold user key map.")
(define-prefix-command 'my-map)
(global-set-key (kbd "C-1") 'my-map)

(define-key my-map (kbd "r") 'recompile)
(define-key my-map (kbd "l") 'delete-trailing-whitespace)
(define-key my-map (kbd "w") 'whitespace-mode)




;;; avy
(use-package avy
  :bind (
         ("C-:" . avy-goto-char)
         :map my-map
         ("w" . avy-goto-word-1)
         ("1" . avy-goto-char-timer)))


;;;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  :custom
  ;; (undo-tree-visualizer-relative-timestamps t)
  (undo-tree-auto-save-history nil)
  (undo-tree-visualizer-timestamps t)
  )

;;; move-text
(use-package move-text
  :bind (("M-S-<up>" . move-text-up)
         ("M-S-<down>" . move-text-down)))


;;; crux
(use-package crux
  :bind (("C-c o" . crux-open-with)))

;;; ivy
;; interesting for later:
;; - ivy-rich
;; - ivy-hydra
;; - ivy-pass https://www.passwordstore.org/ the standard unix password manager

;; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
(use-package ivy
  :defer 0.1
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
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
  ;; (setq ivy-count-format "")
  (setq ivy-count-format "(%d/%d) ")

  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)

  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

;;; counsel
;; install smex for counsel-M-x
(use-package counsel
  :after ivy
  :bind
  (
   ("C-x C-r" . counsel-recentf)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-load-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate))
  :config (counsel-mode))


;;; swiper
;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
(use-package swiper
  ;; C-j to select current
  ;; C-M-j to select current value (creat new file)
  ;; M-j to select word at point.
  :bind (
         ;; -isearch is word instead of line based,
         ("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ;; ("C-r" . swiper-backward)
         ))

;;; recentf
(use-package recentf
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 15))


;;; magit
(use-package magit
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

;;; paredit
(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode))))


(use-package flycheck
  :config
  ;; TODO [ ] https://github.com/abo-abo/hydra/wiki/Flycheck
  ;; Force flycheck to always use c++11 support. We use
  ;; the clang language backend so this is set to clang
  ;; Turn flycheck on everywhere
  (global-flycheck-mode 1)

  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-clang-language-standard "c++14")))

  ;; Requires pylint and flake8 to be installed.
  ;; (flycheck-add-next-checker `python-pylint '(warning . python-flake8))
  (setq  flycheck-python-flake8-executable "flake8")
  )


;;; org-mode

(defvar my-org-font-heading
  (cond
   ((not (display-graphic-p)) nil)
   ((x-list-fonts "Avenir")         '(:font "Avenir"))
   ((x-list-fonts "Charter")         '(:font "Charter"))
   ((x-list-fonts "Palatino")         '(:font "Palatino"))
   ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
   (t    '(:family "Sans Serif")))
  "Which font to use.  Can be set by the local init scripts.")

(defvar my-org-font-variable
  (cond
   ((not (display-graphic-p)) nil)
   ((x-list-fonts "Charter")         '(:font "Charter"))
   ((x-list-fonts "Palatino")         '(:font "Palatino"))
   (t    '(:family "Serif")))
  "Which font to use."
  )

(defvar my-org-font-fixed
  (cond
   ((not (display-graphic-p)) nil)
   ((x-list-fonts "Fira Code")         '(:font "Fira Code"))
   (t    '(:family "Monospace"))))

(defun my-org-fonts-setup ()
  "Set up the org mode fonts."
  (when (display-graphic-p)
    (add-hook 'org-mode-hook 'variable-pitch-mode)

    (let* ((heading-font my-org-font-heading)
           (headline           `(:weight bold )))
      (custom-theme-set-faces
       'user
       `(org-level-8 ((t (,@headline ,@heading-font))))
       `(org-level-7 ((t (,@headline ,@heading-font))))
       `(org-level-6 ((t (,@headline ,@heading-font))))
       `(org-level-5 ((t (,@headline ,@heading-font))))
       `(org-level-4 ((t (,@headline ,@heading-font :height 1.1))))
       `(org-level-3 ((t (,@headline ,@heading-font :height 1.25))))
       `(org-level-2 ((t (,@headline ,@heading-font :height 1.5))))
       `(org-level-1 ((t (,@headline ,@heading-font :height 1.75))))
       `(org-document-title ((t (,@headline ,@heading-font :height 2.0 :underline nil))))
       `(variable-pitch ((t (:height 180 :weight thin ,@my-org-font-variable))))
       `(fixed-pitch ((t (:height 160 ,@my-org-font-fixed))))
       '(org-block ((t (:inherit fixed-pitch))))
       '(org-table ((t (:inherit fixed-pitch))))
       '(org-code ((t (:inherit (shadow fixed-pitch)))))
       ;; '(org-document-info ((t (:foreground "dark orange"))))
       '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
       '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
       ;;   '(org-link ((t (:foreground "royal blue" :underline t))))
       '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
       '(org-property-value ((t (:inherit fixed-pitch))) t)
       '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
       ;;   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
       '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
       '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))))

;; http://endlessparentheses.com/inserting-the-kbd-tag-in-org-mode.html?source=rss
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


(use-package org
  :defer t
  :custom
  (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  ;; this can be modified in local config
  (org-directory "~/code/private")
  (org-log-done t)
  (org-default-notes-file "log.org")

  :config

  ;; note - C-c C-, instead of <s for newer org

  (add-hook 'org-mode-hook (lambda ()
                             (auto-fill-mode 1)))


  (define-key org-mode-map "\C-ck" #'endless/insert-key)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)

  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)
     (python . t)
     ))

  ;; Make it look nicer
  ;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  ;;
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


  (my-org-fonts-setup))

;; (use-package ox-html
;;   :custom
;; (org-html-htmlize-output-type 'css)
;; )


;; define additional use-package sections in local config
(use-package org-capture
  :defer t
  :bind (([f6] . org-capture)
         ("\C-cc" . org-capture))
  :custom
  (org-capture-templates
   '(("r" "Reference" entry (file "reference.org")
      "* %? %^g" :prepend t)
     ;; ("t" "Todo Inbox" entry (file+headline "" "Todos")
     ;;  "* TODO %?\n  Added %u\n  %i" :prepend t)
     ("b" "Bookmark" entry (file "bookmarks.org")
      "* [[%^{link}][%^{description}]] %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
     ;; ("b" "Bookmark" entry (file "bookmarks.org")
     ;;  "* %?\n %x")
     ;; http://members.optusnet.com.au/~charles57/GTD/datetree.html
     ("n" "Notes inbox" entry (file+headline "log.org" "Inbox")
      "* %^{Description} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?\n" :empty-lines 1)
     ("t" "make a task" entry (file+olp+datetree "log.org")
      "* TODO %^{Description} %^g\n\n:PROPERTIES:\n:CREATED: %U\n:END:\n"))))

(use-package org-refile
  :defer t
  :custom
  ;; http://doc.norang.ca/org-mode.html#Refiling
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets (quote ((nil :maxlevel . 3)))))

(use-package org-bullets
  :defer t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))



;;; projectile
(use-package projectile
  :after ivy
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq
   projectile-completion-system 'ivy
   projectile-enable-caching t
   ;; projectile-project-search-path '("~/code" "~/.dotfiles")
   )
  (projectile-mode t))

(use-package
  counsel-projectile
  :ensure t)

;;; Mode specific

;;; ledger
(use-package ledger-mode
  :mode "\\.ledger$")


;;; shell
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;;; elisp
;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defun imenu-elisp-sections ()
  "Might as well have a doc  string."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)



;;; programming


(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)


(defun show-trailing-ws ()
  "Turn on the mode to show trailing whitespace."
  (setq show-trailing-whitespace 1))

(add-hook 'prog-mode-hook 'show-trailing-ws)

;; http://stackoverflow.com/a/13408008
(use-package ansi-color
  :commands ansi-color-apply-on-region
  :init
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (defun colorize-compilation-buffer ()
    "Colorize compiler output."
    (ansi-color-apply-on-region compilation-filter-start (point))))

(use-package company
  :config
  (global-company-mode 1))


;;; cpp
(use-package c++-mode
  :mode "\\.h\\'")

(use-package google-c-style
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style))

;; http://syamajala.github.io/c-ide.html
;; cpputils-cmake
(use-package cpputils-cmake)

;; Make sure clang-tidy is on exec path
(use-package flycheck-clang-tidy
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))



(defvar *cpp-auto-clang-format* t  "Auto-clang format on save hook.")

(defun my-add-clang-format-hook ()
  "Run clang format on save."
  (add-hook 'before-save-hook
            (lambda ()
              (when *cpp-auto-clang-format* (clang-format-buffer)))
            nil t))

(use-package clang-format
  :commands clang-format-region
  :bind  (([C-M-tab] . clang-format-region))
  :config
  (add-hook 'c++-mode  #'my-add-clang-format-hook))

;; To get tabs in c++ code
;; // L o c a l Variables:
;; // *cpp-auto-clang-format*: nil
;; // indent-tabs-mode: t
;; // c-basic-offset: 4
;; // tab-width: 4
;; // whitespace-mode: t
;; // End:


;;; python
(defun my-python-mode-hook ()
  "Stuff to set for python mode."
  (setq-default py-indent-offset 4)
  (setq indent-tabs-mode nil)
  ;;(setq py-python-command nil)
  ;;(require 'virtualenv)
  (transient-mark-mode t)
  (message "ran my-python-mode-hook"))

(use-package python
  :init
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


(use-package pyvenv
  :config
  ;; Set this configuration in local customizations if needed
  (setenv "WORKON_HOME"   (expand-file-name "~/miniconda3/envs")))

;; M-x elpy-rpc-reinstall-virtualenv to fix "peculiar error" message
(use-package elpy
  :defer t
  ;; use pyvenv-workon manually to switch projects.  it might be nice
  ;; to auto-switch, but at the cost of doing more work when switching
  ;; buffers.
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;;; js
(use-package js2-mode
  :mode (("\\.json\\'" . js-mode)
         ("\\.js\\'" . js2-mode))
  :init
  (add-hook 'js-mode-hook 'js2-minor-mode)
  :custom
  (js2-highlight-level 3))

(use-package ac-js2
  :hook js2-mode-hook)


;;; markdown
(use-package markdown-mode)

;; http://jblevins.org/log/mmm


(use-package mmm-mode
  :bind (("C-c m" . mmm-parse-buffer))
  :config
  (setq mmm-global-mode 'maybe)
  (defun my-mmm-markdown-auto-class (lang &optional submode)
    "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
If SUBMODE is not provided, use `LANG-mode' by default."
    (let ((class (intern (concat "markdown-" lang)))
          (submode (or submode (intern (concat lang "-mode"))))
          (front (concat "^```" lang "[\n\r]+"))
          (back "^```"))
      (mmm-add-classes (list (list class :submode submode :front front :back back)))
      (mmm-add-mode-ext-class 'markdown-mode nil class)))
  ;; Note: rememb
  ;; Mode names that derive directly from the language name
  (mapc 'my-mmm-markdown-auto-class
        '("c" "cpp" "css" "html" "latex" "lisp" "makefile"
          "markdown" "python" "r" "xml")))


;; Consider something like this in local:
;;(add-to-list 'exec-path "/usr/local/bin" t)
;;(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;;(setq my-default-theme 'zenburn)
;; modus
;; ef-themes

(use-package epg
  :defer t
  :config
  ;; ?? 
  (setq epg-pinentry-mode 'loopback)
  ;; make saving work better in org mode
  (fset 'epg-wait-for-status 'ignore))

;; START LOCAL CUSTOMIZATION
(require 'init-local nil t)
;; END LOCAL CUSTOMIZATION


;; Make epg work with newer gpgs
(fset 'epg-wait-for-status 'ignore)

(provide 'init)
;;; init.el ends here


;;  (outline-hide-subtree))
;; Local Variables:
;; eval: (outline-minor-mode 1)
;; eval: (while (re-search-forward outline-regexp nil t)  (outline-hide-subtree)))
;; outline-regexp: "^;;; "
;; End:
