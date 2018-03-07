;;; Configure emacs' looks

;; window
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines nil)

;; editor
(setq custom-safe-themes t)
(load-theme 'gruvbox-dark-hard t)
(show-paren-mode t)

(setq fill-column 80)
(add-hook 'text-mode-hook #'turn-on-auto-fill) 
(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; modeline
(line-number-mode t)
(column-number-mode t)

(provide 'looks)
