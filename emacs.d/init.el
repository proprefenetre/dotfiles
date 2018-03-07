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

(setq use-package-verbose t)

;; packages

; helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :config 
  (setq helm-split-window-in-side-p t)
  (helm-mode 1))

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/theme 'dark)
    (rich-minority-mode 1)
    (setf rm-blacklist "")
    (add-hook 'after-init-hook #'sml/setup)))

(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t))

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

(use-package key-chord
  :ensure t
  :config
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1))

(use-package gruvbox-theme
  :ensure t)

(use-package org
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode t))

;; settings
(require 'looks)

(setq vc-follow-symlinks t) 
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)


(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
