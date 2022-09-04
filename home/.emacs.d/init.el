;;; init.el --- Emacs configuration file

;;; Commentary:

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




;; Don't limit the print out of a variable
(setq eval-expression-print-length nil)

(setq inhibit-startup-screen t)
(setq visible-bell t)

;; Turn off mouse interface early in startup to avoid momentary display
;; The macro is for non-windowed emacs.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Give names to some config directories.
(defconst *config-dir* (file-name-directory load-file-name)
  "Root directory for the configuration.")
(defconst *local-dir* (expand-file-name "~/.dotfiles/local/.emacs.d")
  "Root directory for local configuation.")

(defconst *config-d* (file-name-as-directory (concat *config-dir* "setup"))
  "Root directory for the configuration.")

;; Keep the custom file in local-dir so it can be tracked in the
;; local config git file if desired.
(setq custom-file (expand-file-name "custom.el" *local-dir*))


(defconst *is-mac* (eq system-type 'darwin))

(defvar *anaconda-directory* (expand-file-name "~/miniconda3")
  "Anaconda installation directory.")

(defvar *font-name* "fira code-14"
  "Which font to use.  Can be set by the local init scripts.")

(message *config-d*)

(require 'cl-lib)

(add-hook 'emacs-startup-hook
          (lambda ()
            (set-frame-font *font-name*)))


;; (mapc 'load (file-expand-wildcards (concat  *local-dir* "*.el")))
;; (mapc 'load (file-expand-wildcards *config-dir*))



;; Add a local lisp directory to the load path.
(add-to-list 'load-path *local-dir*)

(defvar my-packages
  '(
    ansi-color
    avy
    clang-format
    company
    conda
    counsel
    cpputils-cmake
    dash
    diminish
    ef-themes
    flycheck
    flycheck-clang-tidy
    google-c-style
    hydra
    ivy
    js2-mode
    lsp-ivy
    lsp-jedi
    lsp-mode
    lsp-ui
    magit
    markdown-mode
    mmm-mode
    move-text
    org-bullets
    paredit
    projectile
    python
    rainbow-mode
    recentf
    smex
    swiper
    undo-tree
    wgrep
    yasnippet
    )
  "packages to make sure are installed"
  )

;; my-packages can be modified in init-local-preload.el to add/remove
;; packages for a specific deployment.

;; Try to load local settings ahead of time
(require 'init-local-preload nil t)


(require 'package)
(package-initialize)
(message "loading package stuff")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Force this call for errors like use-package- not found
;; (package-refresh-contents)
(unless package-archive-contents  (package-refresh-contents))

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; make sure use-package is loaded
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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
(mapc 'load (file-expand-wildcards (concat  *config-d* "[a-zA-Z0-9]*.el")))


;; Load the local file, if it exists.
(require 'init-local nil t)


;; TODO require-after-load is using this...
(provide 'init)


;; Consider something like this in local:
;;;; Example init-local.el
;;(add-to-list 'exec-path "/usr/local/bin" t)
;;(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;;(setq my-default-theme 'zenburn)
;; (provide 'init-local)


;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary
