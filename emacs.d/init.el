;; init.el --- Emacs Configuration
;;; Commentary:
;;; iteration: 2019-1-7
;;; Code:

;;; Initialization
;; Personal settngs
(setq user-full-name "Niels Eigenraam"
      user-mail-address "nielseigenraam@gmail.com")

;; garbage collection
(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

;; package.el
(require 'package)
(setq package-enable-at-startup nil
      load-prefer-newer t
      package-user-dir "~/.emacs.d/elpa"
      package--init-file-ensured t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(unless package--initialized (package-initialize t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-verbose t)

(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
(load custom-file)

;;; personal lisp collection
(add-to-list 'load-path "/home/niels/dotfiles/emacs.d")
(add-to-list 'load-path "/home/niels/dotfiles/emacs.d/etc/lisp")

;;; Settings
(setq custom-safe-themes t)
(use-package doom-themes
  ;; colors

  ;; (red        '("#ff6c6b" "#ff6655" "red"          ))
  ;; (orange     '("#da8548" "#dd8844" "brightred"    ))
  ;; (green      '("#98be65" "#99bb66" "green"        ))
  ;; (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
  ;; (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
  ;; (blue       '("#51afef" "#51afef" "brightblue"   ))
  ;; (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
  ;; (magenta    '("#c678dd" "#c678dd" "brightmagenta"))
  ;; (violet     '("#a9a1e1" "#a9a1e1" "magenta"      ))
  ;; (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
  ;; (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))
  :init
  (load-theme 'doom-vibrant 't)
  :config
  ;; (setq-default doom-neotree-file-icons t)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-org-config))

(set-face-attribute 'default nil :font "Iosevka 11")
(set-face-attribute 'line-number nil :background 'unspecified)
(set-face-attribute 'fringe nil :inherit 'line-number)

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-input-method "latin-postfix")

(require 'recentf)
(setq-default abbrev-mode 1
              recentf-mode 1
              show-paren-mode 1)

(dolist (table abbrev-table-name-list)
  (abbrev-table-put (symbol-value table) :case-fixed t))

(run-at-time nil (* 5 60)
             (lambda ()
               (let ((inhibit-message t))
                 (recentf-save-list))))


(setq confirm-kill-emacs 'yes-or-no-p)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default x-select-enable-clipboard t)
(setq-default vc-follow-symlinks t)

(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              scroll-margin 10
              scroll-conservatively most-positive-fixnum
              auto-fill-function 'do-auto-fill)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode 1)

(fringe-mode '(8 . 8))

(setq display-line-numbers-width 4
      display-line-numbers-width-start 3
      display-line-numbers-widen nil)

(setq ispell-silently-savep t
      ispell-dictionary "english"
      ispell-extra-args '("-a" "utf-8"))

(setq-default bookmark-save-flag 1
              bookmark-default-file "~/dotfiles/emacs.d/var/bookmarks")

(setq TeX-engine 'xelatex)
(setq latex-run-command "xelatex")

(setq-default tramp-default-method "ssh")

(setq frame-title-format "%b")

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; (setq compilation-read-command nil)

(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "2 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No Compilation Errors!")))))


;;; load lisp files
(require 'pkgs)
(require 'functions)
(require 'keys)
(require 'table)


;;; python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")


(add-hook 'focus-out-hook 'garbage-collect)
(add-hook 'sh-mode-hook 'aggressive-indent-mode)

(defun pfn-setup-prog-mode ()
  "Load 'prog-mode' minor modes."
  (eldoc-mode 1)
  (auto-fill-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
  (delete-trailing-whitespace)
  (flycheck-mode 1)
  (outline-minor-mode 1)
  (display-line-numbers-mode 1)
  (smartparens-mode 1))
(add-hook 'prog-mode-hook 'pfn-setup-prog-mode)


(defun pfn-setup-text-mode ()
  "Load 'text-mode' hooks."
  (delete-trailing-whitespace)
  (turn-on-auto-fill)
  (rainbow-delimiters-mode 1)
  (flyspell-mode 1)
  (electric-pair-mode 1))
(add-hook 'text-mode-hook 'pfn-setup-text-mode)

;; company - yasnippet garbage
(defun company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil))
    (unless (yas-expand)
      (call-interactively #'company-complete-common))))
(add-hook 'company-mode-hook (lambda ()
                               (substitute-key-definition 'company-complete-common
                                                          'company-yasnippet-or-completion
                                                          company-active-map)))

;; lower garbage collection threshold

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
;;; init.el ends here
