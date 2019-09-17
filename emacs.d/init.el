;;; init.el --- guess what
;;; Commentary:
;;; this beast keeps getting longer.
;;; Code:

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)

(dolist (archive '(("elpa" . "https://elpa.gnu.org/packages/")
                   ("melpa" . "https://melpa.org/packages/")
                   ("melpa-stable" . "https://stable.melpa.org/packages/")
                   ("org" . "https://orgmode.org/elpa/")))
  (add-to-list 'package-archives archive))

(setq package-enable-at-startup nil)
(package-initialize)

(setq-default use-package-always-ensure t
              use-package-always-defer t
              use-package-verbose t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
(load custom-file)

(set-face-attribute 'default nil :font "Fantasque Sans Mono 15")

(fset 'yes-or-no-p 'y-or-n-p)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(fringe-mode '(8 . 8))
(recentf-mode)
(auto-fill-mode -1)
(abbrev-mode 1)

(setq-default default-input-method "latin-postfix"
              initial-scratch-message ""
              indent-tabs-mode nil
              tab-width 4
              fill-column 120
              scroll-margin 10
              scroll-conservatively most-positive-fixnum
              confirm-kill-emacs 'yes-or-no-p
              x-select-enable-clipboard t
              vc-follow-symlinks t
              display-line-numbers-width 4
              display-line-numbers-width-start 3
              display-line-numbers-widen nil
              bookmark-save-flag 1
              bookmark-default-file "~/.emacs.d/var/bookmarks"
              TeX-engine 'xelatex
              latex-run-command "xelatex"
              tramp-default-method "ssh"
              save-abbrevs 'silent
              desktop-restore-frames nil
              inhibit-startup-screen t
              auto-fill-function 'do-auto-fill
              browse-url-firefox-program "firefox-developer-edition"
              browse-url-browser-function 'browse-url-firefox)

(dolist (table abbrev-table-name-list)
  (abbrev-table-put (symbol-value table) :case-fixed t))

(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

;; Packages

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'pfn-functions)
(require 'pfn-completion)
(require 'pfn-evil)
(require 'pfn-org)
(require 'pfn-python)
(require 'pfn-rust)
(require 'pfn-stats)

(use-package all-the-icons)

(use-package doom-themes
  :init
  (setq custom-safe-themes t)
  (load-theme 'doom-one t)
  :config
  (setq doom-one-brighter-comments nil)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; doom-modeline-height 11
  ;; doom-modeline-bar-width 3
  (setq column-number-mode t
        doom-modeline-icon t))

(use-package key-chord
  :demand t
  :config (key-chord-mode 1))

(use-package which-key
  :demand t
  :config (which-key-mode 1))

(use-package smex
  :demand t)

(use-package general
  :demand t
  :config
  (general-evil-setup)
  (general-override-mode)

  ;; general keybindings
  (general-create-definer evil-leader
    :prefix ",")

  (evil-leader
    :states '(normal visual emacs)
    :keymaps 'override
    ;; "b" '
    "c" 'capitalize-dwim
    "d" 'dired-jump
    "e" 'eval-last-sexp
    "g" 'evil-commentary-yank-line
    "f" 'flycheck-list-errors
    "i" '(lambda () (interactive)
           (find-file user-init-file))
    "o" 'olivetti-mode
    "p" 'counsel-yank-pop
    "q" 'evil-window-delete
    "r" '(lambda () (interactive)
           (revert-buffer :ignore-auto :noconfirm))
    "n" 'symbol-overlay-rename
    "s" 'magit-status
    "t" 'treemacs-select-window
    "w" 'ace-window)

  (general-def
    :prefix "C-c"
    "b"   'counsel-bookmark
    "c"   '(lambda () (interactive)
             (org-capture nil "c"))
    ;; "d"
    "C-d" 'dired-jump-other-window
    ;; "e"
    "f"   'ffap-other-window
    ;; "g" "h"
    "i"   'ibuffer
    ;; "j"
    "k"   'counsel-ag
    "l"   'org-store-link
    ;; "m" "n" "o"
    "p"   'projectile-command-map
    ;; "q"
    "R"   '(lambda () (interactive)
             (revert-buffer :ignore-auto :noconfirm))
    "s"   'counsel-rg
    "t"   'treemacs
    ;; "u"
    ;; "v"
    "w"   'ace-window
    ;;"x"
    "C-l" 'comint-clear-buffer)

  (general-def
    :prefix "C-x"
    "ESC ESC" 'keyboard-quit
    "C-b" 'counsel-ibuffer
    "2" '(lambda () (interactive) 
           (split-window-below)
           (other-window 1))
    "3" '(lambda () (interactive) 
           (split-window-right)
           (other-window 1)))

  (general-def
    "M-x" 'counsel-M-x
    "M-/" 'hippie-expand
    "C-)" 'sp-forward-slurp-sexp
    "C-(" 'sp-add-to-previous-sexp
    "C-s-s" 'query-replace
    "C-s" 'swiper)

  (general-mmap
    "j"   'evil-next-visual-line
    "k"   'evil-previous-visual-line
    "C-e" 'evil-end-of-line
    "[ p" 'evil-paste-before
    "] p" 'evil-paste-after
    "`"   'evil-avy-goto-char
    "C-b" 'mode-line-other-buffer)

  (general-def
    :keymaps 'evil-insert-state-map
    (general-chord "jj") 'evil-normal-state
    (general-chord "ww") 'evil-window-next)

  (general-def
    :keymaps 'evil-normal-state-map
    (general-chord "bi") 'ibuffer
    "s-q" 'kill-this-buffer)

  (general-def
    :keymaps 'evil-visual-state-map
    ")" 'er/expand-region)

  (general-def
    :keymaps 'goto-map
    "f" 'avy-goto-char
    "t" 'avy-goto-word-1)

  ;; package specific
  (general-def
    :keymaps 'company-active-map
    "C-w" 'evil-delete-backward-word
    "C-n"  'company-select-next
    "C-p"  'company-select-next
    "<tab>" 'company-complete-common
    "<esc>" 'company-cancel)

  (general-def
    :keymaps 'rust-mode-map
    "C-c <tab>" 'rust-format-buffer)

  (general-def
    :keymaps 'org-mode-map
    :prefix "C-c"
    "a"   'org-agenda-list
    "C-a" 'org-archive-subtree
    "r"   'org-refile
    "!"   'org-time-stamp-inactive)

  (general-def
    :keymaps 'org-mode-map
    :states 'normal
    "<return>" 'org-return)

  (general-def
    :keymaps 'treemacs-mode-map
    :states 'treemacs
    "C-w s" 'treemacs-switch-workspace))

(use-package ivy
  :demand t
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :demand t)

(use-package prescient
  :demand t
  :config
  (prescient-persist-mode 1)
  (setq ivy-prescient-excluded-commands '(swiper evil-search-function)))

(use-package ivy-prescient
  :after prescient
  :demand t
  :config
  (ivy-prescient-mode 1))

(use-package edit-indirect)

(use-package expand-region)

(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n)
        avy-style 'pre
        avy-all-windows nil))

(use-package shackle
  :demand t
  :config
  (setq shackle-rules '(("*Flycheck error messages*" :noselect t :align 'below
                         :ignore t)
                        ("*Python*" :noselect t :size .25 :align 'below)
                        ("COMMIT_EDITMSG" :select t)
                        (magit-status-mode :regexp t :same t :inhibit-window-quit t)))
  (setq shackle-default-rule '(:select t :align 'below))
  (shackle-mode 1))

(use-package magit
  :demand t)

(use-package rainbow-delimiters
  :demand t
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inverse-video nil
                      :weight 'normal))

(use-package smartparens
  :demand t
  :config
  (require 'smartparens-config)
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "/" "/")
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (add-to-list 'sp-sexp-suffix (list 'rust-mode 'regexp ";"))
  (set-face-attribute 'sp-show-pair-match-face nil :foreground "#51afef")
  (set-face-attribute 'sp-show-pair-mismatch-face nil :weight 'unspecified :foreground 'unspecified :background 'unspecified)
  (smartparens-global-mode))

(use-package rainbow-mode
  :demand t
  :config
  (rainbow-mode 1))

(use-package aggressive-indent
  :demand t
  :config
  (dolist (mode '(html-mode python-mode dockerfile-mode))
    (add-to-list 'aggressive-indent-excluded-modes mode))
  (global-aggressive-indent-mode))

(use-package yasnippet
  :demand t
  :config
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (setq yas-triggers-in-field t
        yas-snippet-revival t
        yas-indent-line 'nil
        yas-wrap-around-region t
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  ;; (add-to-list 'company-backends 'company-yasnippet)
  (yas-global-mode 1))

(use-package projectile
  :demand t
  :config
  (projectile-mode))

(use-package counsel-projectile
  :demand t
  :config
  (counsel-projectile-mode))

(use-package treemacs
  :demand t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (setq treemacs-width 25
        treemacs-position 'right
        treemacs-no-png-images t
        treemacs-python-executable "/usr/local/bin/python")
  (treemacs-git-mode 'deferred))

(use-package treemacs-evil
  :demand t)

(use-package treemacs-projectile
  :demand t)

(use-package treemacs-magit
  :demand t)

(use-package eyebrowse
  :demand t
  :config
  (setq eyebrowse-new-workspace t
        eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t)
  (eyebrowse-setup-opinionated-keys)
  (general-def 'motion
    "gc" 'evil-commentary)
  (eyebrowse-mode))

(use-package flycheck
  :commands (projectile-switch-project)
  :config
  (setq flycheck-idle-change-delay 2)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package symbol-overlay
  :demand t
  :config
  (setq symbol-overlay-displayed-window t))

(use-package highlight-indent-guides
  :demand t
  :hook (python-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top))

(use-package ace-window
  :demand t
  :config
  (setq aw-scope 'frame))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-tramp)

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :config
  (display-line-numbers-mode))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (display-line-numbers-mode))

(use-package hydra)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.spec\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package olivetti
  :config
  (setq olivetti-body-width 120)
  (display-line-numbers-mode -1))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; (use-package pdf-tools
;;   :pin manual
;;   :mode "\\.pdf\\'"
;;   :config
;;   (pdf-tools-install)
;;   (setq-default pdf-view-display-size 'fit-page)
;;   (add-hook 'pdf-view-mode-hook '(blink-cursor-mode -1)))

(use-package highlight-numbers)

(use-package highlight-escape-sequences)

(use-package persistent-scratch
  :hook (emacs-startup . persistent-scratch-restore)
  :config
  (persistent-scratch-setup-default))

(use-package feature-mode
  ;; Cucumber/gherkin mode
  :mode "\\.feature\\'")

(use-package realgud)

(add-hook 'focus-out-hook 'garbage-collect)

(global-hl-line-mode)
(global-auto-revert-mode)
(global-eldoc-mode)
(global-prettify-symbols-mode)

;; prog-mode hooks
(dolist (hook '(hes-mode
                hs-minor-mode
                highlight-numbers-mode
                rainbow-delimiters-mode
                display-line-numbers-mode
                flycheck-mode))
  (add-hook 'prog-mode-hook hook))

;; text-mode hooks
(dolist (hook '(hes-mode
                hs-minor-mode
                rainbow-delimiters-mode
                delete-trailing-whitespace
                turn-on-auto-fill))
  (add-hook 'text-mode-hook hook))

(setq gc-cons-threshold 20000000
      gc-cons-percentage 0.1)
;;; Init.el ends here
