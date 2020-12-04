(require 'package)
(package-initialize)

;; Also use Melpa for most packages

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("org" . "http://orgmode.org/elpa/") t)
;; (when *is-mac*
  ;; (add-to-list 'package-archives
  ;;              '("marmalade" . "http://marmalade-repo.org/packages/") t))
(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/") t)

(when (not package-archive-contents)
    (package-refresh-contents))

;; make sure use-package is loaded
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package does-not-exist
  :disabled)
