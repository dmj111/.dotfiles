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
(defconst my-local-dir (expand-file-name "~/.dotfiles/local/.emacs.d")
  "Root directory for local configuation.")

(defconst my-config-setup (file-name-as-directory (concat my-config-dir "setup"))
  "Root directory for the configuration.")

;; Keep the custom file in local-dir so it can be tracked in the
;; local config git file if desired.
(setq custom-file (expand-file-name "custom.el" my-local-dir))




(require 'cl-lib)

(defun my-el-files (dir)
  "Return list of el files in DIR, except auto-saves and custom.el."
  (cl-delete-if
   (lambda (f) nil
     (or  (string-match-p "/custom.el\\'" f)
          (string-match-p "~\\|#" f))
     )
   (file-expand-wildcards (file-name-concat  dir "*.el")))
  )

(defun my-load-el-files (dir)
  "Load .el files in DIR, except auto-saves and custom.el."
  (mapc 'load (my-el-files dir)))

;; (mapc 'load (file-expand-wildcards (concat  my-local-dir "*.el")))
;; (mapc 'load (file-expand-wildcards my-config-dir))



;; Add a local lisp directory to the load path.
(add-to-list 'load-path my-local-dir)

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
  "Packages to ensure are installed."
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

(my-load-el-files my-config-setup)



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


;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary

(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)
;;; init.el ends here


;; Local Variables:
;; eval: (outline-minor-mode 1)
;; eval: (while (re-search-forward outline-regexp nil t) (outline-hide-subtree))
;; outline-regexp: "^;;; "
;; End:
