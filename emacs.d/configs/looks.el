;;; Configure emacs' looks

;; editor
(setq custom-safe-themes t)
(load-theme 'gruvbox-dark-hard t)


(show-paren-mode t)
(global-linum-mode t)

(setq fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill) 
;; sentences end with single space
(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

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

;; modeline
(line-number-mode t)
(column-number-mode t)

;; jump into this some other time
;(setq-default mode-line-format
;              (list
;               " " mode-line-modified
;               " %[" mode-line-buffer-identification "%] %l %6 "
;               mode-line-misc-info
;               mode-line-end-spaces))
;
;(setq global-mode-string '("" display-time-string appt-mode-string))

(provide 'looks)
