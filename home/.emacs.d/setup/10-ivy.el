(use-package ivy
  :defer 0.1
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
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
  ;; (setq ivy-count-format "")
  (setq ivy-count-format "(%d/%d) ")

  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)

  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

(use-package counsel
  :after ivy
  :bind
  (
   ("C-x C-r" . counsel-recentf)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-load-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate))
  :config (counsel-mode))


;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/

(use-package swiper
  ;; C-j to select current
  ;; C-M-j to select current value (creat new file)
  ;; M-j to select word at point.
  :bind (
         ;; -isearch is word instead of line based,
         ("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ;; ("C-r" . swiper-backward)
         ))


;; interesting for later:
;; - ivy-rich
;; - ivy-hydra
;; - ivy-pass https://www.passwordstore.org/ the standard unix password manager

;; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
