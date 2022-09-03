(use-package magit
  :bind (("\C-xg" . magit-status))
  :config

  ;; (eval-after-load 'swiper
  ;;   (setq magit-completing-read-function 'ivy-completing-read))

  ;; full-scrreen magit-status
  ;; from magnars --
  ;;    https://github.com/magnars/.emacs.d/blob/master/setup-magit.el
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))
