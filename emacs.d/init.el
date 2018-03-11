(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/")) 
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)

;; directories in .emacs.d
(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/home/niels/bin")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'utils)

; keys
(global-set-key (kbd "C-c o") 'pfn/open-config-file)

; packages

;; org 
(require 'org-settings)
(require 'evil-settings)

;; helm
(use-package helm
  :ensure t
  :config 
  (setq helm-split-window-in-side-p t)
  (helm-mode 1))

;; look and feel
(use-package eyebrowse
  :ensure t
  :init
  :config
  (setq eyebrowse-new-workspace t)
  (define-key evil-motion-state-map "gc" nil)
  (define-key evil-motion-state-map "gC" 'eyebrowse-close-window-config)
  (eyebrowse-mode t))

(use-package olivetti
  :ensure t
  :defer t
  :config
  (linum-mode -1))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . markdown-mode))

(setq custom-safe-themes t)
(use-package base16-theme
  :ensure t
  :init (setq base16-distinct-fringe-backgeround nil)
  :demand t
  :config
  (load-theme 'base16-gruvbox-dark-soft))

(use-package rainbow-delimiters
  :ensure t)

(set-face-attribute 'default nil :font "Hack-10" )

(show-paren-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq visible-bell nil
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines nil)

(setq fill-column 80)
(setq sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)

;; mode-line
(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/override-theme nil)
    (rich-minority-mode 1)
    (setq rm-whitelist "eyebrowse")
    (add-hook 'after-init-hook #'sml/setup)))

(line-number-mode t)
(column-number-mode t)

;; text-mode
; Automatically detect language for Flyspell
(use-package guess-language
  :ensure t
  :defer t
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_GB" "English"))
                                   (nl . ("nl_NL" "Dutch")))
        guess-language-languages '(en nl)
        guess-language-min-paragraph-length 45))

(add-hook 'text-mode-hook #'turn-on-auto-fill) 
(add-hook 'text-mode-hook #'rainbow-delimiters-mode)
(add-hook 'text-mode-hook #'turn-on-flyspell)

;; prog-mode hooks
(add-hook 'prog-mode-hook #'turn-on-auto-fill) 
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'linum-mode t)

;; files
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;; code
(use-package flycheck
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package python-mode
  :ensure t)
