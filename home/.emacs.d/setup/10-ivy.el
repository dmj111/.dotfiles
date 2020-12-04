
;; [ ] https://github.com/abo-abo/swiper
;; [ ] http://oremacs.com/swiper/
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer)
   :map ivy-mode-map
   ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order)))
  ;;(setq ivy-count-format "(%d/%d) ")
  )



(use-package counsel :ensure t)
;; commands
;; counsel-ag grep
;; counsel-recentf
;; counsel-git-grep
;; counsel-git find file
;; counsel-describbindings
;; counsel-yank-pop
;; counsel-projectile

(use-package swiper
  :ensure t
  ;; C-j to select current
  ;; C-M-j to select current value (creat new file)
  ;; M-j to select word at point.
  :bind (("C-s" . swiper)
         ("C-x C-r" . counsel-recentf)
         ("M-x" . counsel-M-x)
         ("\C-x\C-m" . counsel-M-x)
         ("\C-xm" . counsel-M-x)
         ("\C-c\C-m" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox)
         ("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  )
