(use-package markdown-mode :ensure t)


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
  (global-set-key (kbd "C-c m") 'mmm-parse-buffer))
