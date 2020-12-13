(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :ensure t
  :hook (
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration)
         ((python-mode c++-mode js2-mode) . lsp))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)


;; maybe
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)


;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;; (use-package which-key
;;   :config
;;   (which-key-mode))

;;;; python
;; pipx install jedi-language-server

(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))


;;; C++

;; install clangd on system

;; TODO: move to local, or add to path
;; (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")

(require 'lsp-clangd)
(add-to-list 'lsp-enabled-clients 'clangd)
(add-to-list 'lsp-enabled-clients 'eslint)

(defun remove-from (xs ms)
  (seq-filter (lambda (x) (not (memq x ms))) xs))



(defun my/lsp-cpp-hook ()
  ;; using lsp for completion means we don't want to use the company
  (set (make-local-variable  'company-backends)
       ;; TODO: filter out these values instead of copy/update
       (remove-from company-backends '(company-xcode company-clang))))

(use-package company
  :config
  (add-hook 'c++-mode-hook 'my/lsp-cpp-hook))

; (add-to-list 'lsp-enabled-clients 'eslint)
