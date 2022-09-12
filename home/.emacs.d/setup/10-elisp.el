;;; 10-elisp -- Summary
;;; Commentary:
;;; Code:
;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defun imenu-elisp-sections ()
  "Might as well have a doc  string."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
(provide '10-elisp)
;;; 10-elisp.el ends here
