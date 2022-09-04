;; (use-package zenburn-theme)


;; (use-package zenburn-theme
;;   :defer t)

;; ;; Set this var in local config to override
;; (defvar my-default-theme 'zenburn
;;   "Default theme to use at startup.")

(defvar my-default-theme 'ef-spring
  "*default theme to load at startup")

;; After init.el is loaded, set the theme.
(eval-after-load 'init
  '(progn
     (when my-default-theme
       (message "loading the theme...")
       (load-theme my-default-theme t))))
