;;; 10-conda -- Summary
;;; Commentary:
;;; Code:

(defvar my-anaconda-directory (expand-file-name "~/miniconda3")
  "Anaconda installation directory.")

(use-package conda
  :config
  ;; Make sure pylint is installed.
  ;; make sure a default environment is set
  ;;   conda env export > environment.yml
  ;; https://github.com/necaris/conda.el
  ;; if you want interactive shell support, include:
  ;; (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  ;; (conda-env-initialize-eshell)

  ;; if you want auto-activation (see below for details), include:
  ;; - Consider this for dirs.el
  ;; (conda-env-autoactivate-mode t)

  :custom
  (conda-anaconda-home my-anaconda-directory))
(provide '10-conda)
;;; 10-conda.el ends here
