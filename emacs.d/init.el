; Emacs configuration

(setq user-full-name "Niels Eigenraam"
      user-mail-address "nielseigenraam@gmail.com")

(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package-settings)
(require 'evil-settings)
(require 'org-settings)
(require 'prog-settings)
(require 'modeline-settings)
(require 'utils) ; functions and keybindings

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))
  ;(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

(use-package magit
  :commands (magit-status magit-blame magit-log-buffer-file magit-log-all))

(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  (progn
    (setq sml/theme 'respectful
          sml/shorten-directory t
          sml/name-width 40
          sml/mode-width 'right)
    (add-hook 'after-init-hook #'sml/setup)))

(line-number-mode t)
(column-number-mode t)

(use-package base16-theme
  :ensure t
  :init
  (setq custom-safe-themes t)
  (setq base16-distinct-fringe-background nil)
  :demand t
  :config
  (load-theme 'base16-gruvbox-dark-soft))

(use-package rainbow-delimiters
  :ensure t
  :demand t)

(use-package eyebrowse
  :ensure t
  :demand t
  :init
  :config
  (setq eyebrowse-new-workspace t)
  (eyebrowse-setup-opinionated-keys)
  (define-key evil-motion-state-map "gc" nil)
  (define-key evil-motion-state-map "gC" 'eyebrowse-close-window-config)
  (eyebrowse-mode t))

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))

(use-package helm
  :ensure t
  :config
  (setq helm-split-window-inside-p t)
  (helm-mode 1))

(use-package olivetti
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode
  ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc")
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode)
  :config
  (font-lock-add-keywords 'markdown-mode
                        '(("@[[:alnum:]]+" . font-lock-keyword-face))))

(use-package guess-language
  :ensure t
  :defer t
  :config
  (setq guess-language-langcodes '((en . ("english" "English"))
                                   (nl . ("dutch" "Dutch")))
        guess-language-languages '(en nl)
        guess-language-min-paragraph-length 45))

(set-face-attribute 'default nil :font "Hack-10" )

(show-paren-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq ispell-silently-savep t)

(setq visible-bell nil
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines nil)
(setq sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq fill-column 80)

(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(setq initial-buffer-choice "~/org/todo.org")

(put 'narrow-to-region 'disabled nil)

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'guess-language-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)

; lower garbace collection threshold
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
