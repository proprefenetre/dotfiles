(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)

;; directories in .emacs.d
(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/home/niels/bin")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'load-repos) 

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; packages

; helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :config 
  (setq helm-split-window-in-side-p t)
  (helm-mode 1))

(use-package smart-mode-line :ensure t)

(use-package markdown-mode
  :ensure t
  :commands
  (markdown-mode)
  :mode
  (("README\\.md\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc"))

; evil mode
(require 'evil-settings)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (add-hook 'evil-mode-hook 'pfn--config-evil)
  (evil-mode 1)

    (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (pfn--config-evil-leader))

    (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

    (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))
)
  
(use-package gruvbox-theme
  :ensure t)

(use-package org
  :ensure t)

;; settings
(load-theme 'gruvbox-dark-hard t)
(show-paren-mode t)
(global-linum-mode t)
(line-number-mode t)
(column-number-mode t)

; Make yes/no options y/n
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(menu-bar-mode -1)
(tool-bar-mode -1)

; smart-mode-line
(setq sml/mode-width 10)
(setq sml/name-width 20)
(rich-minority-mode 1)
(setf rm-blacklist "")
(sml/setup)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines nil)

(setq vc-follow-symlinks t) 
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)

(setq custom-safe-themes t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
