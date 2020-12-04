(use-package zenburn-theme
  :ensure t)


;; Set this var in local config
(defvar my-default-theme 'zenburn
  "Default theme to use at startup.")

;; After init.el is loaded, set the theme.
(eval-after-load 'init
  '(progn
     (message "loading the theme...")
     (load-theme my-default-theme t)))
