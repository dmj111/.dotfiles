;; make yes/no shorter, and a frequent alias.
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp)


(setq-default indent-tabs-mode nil)
(column-number-mode 1)
(show-paren-mode 1)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;;; enable features

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (quote [M-down])  'scroll-up-line)
(global-set-key (quote [M-up])  'scroll-down-line)

(require 'cl)

;; http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/
;; unset C- and M- digit keys
(dotimes (n 9)
  (global-unset-key (kbd (format "C-%d" (+ 1 n))))
  (global-unset-key (kbd (format "M-%d" (+ 1 n)))))

(define-prefix-command 'dmj-map)
(global-set-key (kbd "C-1") 'dmj-map)

(define-key dmj-map (kbd "r") 'recompile)

(defun dmj-test-foo()
  "This is a silly test"
  (interactive)
  (message "foo"))
(define-key dmj-map (kbd "[") 'dmj-test-foo)
(define-key dmj-map (kbd "l") 'delete-trailing-whitespace)
(define-key dmj-map (kbd "w") 'whitespace-mode)




;;;; Keybindings

(global-set-key [(f5)] 'call-last-kbd-macro)
(global-set-key "\C-x\C-n" 'next-error)
(global-set-key (quote [S-M-down])  'next-error)
(global-set-key (quote [S-M-up])  'prev-error)

(global-set-key "\M-?" 'help)
(global-set-key "\C-cx" 'compile)
(global-set-key [(f9)] 'recompile)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;; Hmm... isearch.  and regexp.
;; (global-set-key "\M-s" 'isearch-forward-regexp)
;; (global-set-key "\M-r" 'isearch-backward-regexp)

;; backward-kill-word is a fast way to delete
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Sometimes M-x is just too hard to type.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; TODO - detect electric indent, and don't bother if it exists
(define-key global-map (kbd "RET") 'newline-and-indent)


;; backward-kill-word is a fast way to delete
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; This is for when alt is not meta.   I need my meta.
(setq x-alt-keysym 'meta)

(when my-is-mac
  (setq mac-command-modifier 'meta))

(setq uniquify-buffer-name-style 'forward)

;;;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  :custom
  ;; (undo-tree-visualizer-relative-timestamps t)
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-timestamps t)
  )

;;;; winner
(winner-mode t)


(define-coding-system-alias 'UTF-8 'utf-8)

(use-package move-text
  :config
  (global-set-key [\M-\S-up] 'move-text-up)
  (global-set-key [\M-\S-down] 'move-text-down))

;;;; open-with
(when my-is-mac
  ;; Copied from emacs-prelude
  (defun prelude-open-with ()
    "Simple function that allows us to open the underlying
file of a buffer in an external program."
    (interactive)
    (when buffer-file-name
      (shell-command (concat
                      (if (eq system-type 'darwin)
                          "open"
                        (read-shell-command "Open current file with: "))
                      " "
                      buffer-file-name))))

  (global-set-key (kbd "C-c o") 'prelude-open-with))


(defun clear-kill-ring ()
  "Clear all entries from the kill ring."
  (interactive)
  (setq kill-ring nil)
  (garbage-collect))

(defalias 'list-buffers 'ibuffer)


(eval-after-load 'apropos '(setq apropos-sort-by-scores t))
