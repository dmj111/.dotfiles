;;; 10-cpp -- Summary
;;; Commentary:
;;; Code:
;; http://syamajala.github.io/c-ide.html

(use-package c++-mode
  :mode "\\.h\\'")

;;;; cpputils-cmake
(use-package cpputils-cmake)

(use-package google-c-style
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style))


;; Make sure clang-tidy is on exec path
(use-package flycheck-clang-tidy
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup)))


(use-package clang-format
  :config
  (defvar *cpp-auto-clang-format* t  "Auto-clang format on save hook.")
  (defun my-add-clang-format-hook ()
    (add-hook 'before-save-hook
              (lambda ()
                (when *cpp-auto-clang-format* (clang-format-buffer)))
              nil t))
  (add-hook 'c++-mode-hook #'my-add-clang-format-hook)
  (global-set-key [C-M-tab] 'clang-format-region))


;; To get tabs in c++ code
;; // L o c a l Variables:
;; // *cpp-auto-clang-format*: nil
;; // indent-tabs-mode: t
;; // c-basic-offset: 4
;; // tab-width: 4
;; // whitespace-mode: t
;; // End:
(provide '10-cpp)
;;; 10-cpp.el ends here
