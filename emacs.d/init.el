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
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/home/niels/bin")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'utils) ; functions and keybindings
(require 'modeline-settings)
(require 'prog-settings)
(require 'org-settings)
(require 'evil-settings)

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil))

(use-package helm
  :ensure t
  :config 
  (setq helm-split-window-in-side-p t)
  (helm-mode 1))

(use-package eyebrowse
  :ensure t
  :init
  :config
  (setq eyebrowse-new-workspace t)
  (eyebrowse-setup-opinionated-keys)
  (define-key evil-motion-state-map "gc" nil)
  (define-key evil-motion-state-map "gC" 'eyebrowse-close-window-config)
  (eyebrowse-mode t))

(use-package olivetti
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . markdown-mode))

(setq custom-safe-themes t)
(use-package base16-theme
  :ensure t
  :init (setq base16-distinct-fringe-background nil)
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
(setq fill-column 120)
(setq sentence-end-double-space nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)


;; text-mode

; Automatically detect language for Flyspell
(use-package guess-language
  :ensure t
  :defer t
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("english" "English"))
                                   (nl . ("dutch" "Dutch")))
        guess-language-languages '(en nl)
        guess-language-min-paragraph-length 45))

(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'rainbow-delimiters-mode)
(add-hook 'text-mode-hook #'turn-on-flyspell)

;; files
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

(add-hook 'ielm-mode-hook (lambda () (eldoc-mode 1)))
