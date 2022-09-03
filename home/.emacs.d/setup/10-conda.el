
(use-package conda
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
