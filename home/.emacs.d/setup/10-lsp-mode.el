;;; 10-lsp-mode -- Summary
;;; Commentary:
;;; Code:
;;; Note:
;;; not using this right now.

(use-package lsp-mode
  :disabled
  :hook (
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration)
         ((python-mode c++-mode js2-mode) . lsp))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-keymap-prefix "C-c l"))

(eval-after-load "lsp-mode-3" (lambda () (error "shouldn't load this now")))

(defun remove-from (xs ms)
  (seq-filter (lambda (x) (not (memq x ms))) xs))


(defun my/lsp-cpp-hook ()
  ;; using lsp for completion means we don't want to use the company
  (set (make-local-variable  'company-backends)
       (remove-from company-backends '(company-xcode company-clang))))


(use-package lsp-clangd
  :after (lsp-mode)
  :config
  (add-to-list 'lsp-enabled-clients 'clangd)
  (add-to-list 'lsp-enabled-clients 'eslint)
  (add-hook 'c++-mode-hook 'my/lsp-cpp-hook))


(use-package lsp-ui
  :after (lsp-mode)
  :commands lsp-ui-mode)

;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; if you are ivy user
(use-package lsp-ivy
  :after (lsp-mode)
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
  :after (lsp-mode)
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))


;;; C++

;; install clangd on system

;; TODO: move to local, or add to path
;; (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")


(use-package company
  :config
  (global-company-mode 1))

; (add-to-list 'lsp-enabled-clients 'eslint)
(provide '10-lsp-mode)
;;; 10-lsp-mode.el ends here
