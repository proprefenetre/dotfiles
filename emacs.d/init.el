;;; init.el --- Emacs Configuration
;;; Commentary:
;;; Tuesday 09 iteration: a new hope
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

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

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
(add-to-list 'load-path "/home/niels/dotfiles/emacs.d/var/lisp")

(require 'custom-functions)

;;; Settings
(setq custom-safe-themes t)

(use-package challenger-deep-theme
  :demand t
  :config
  (load-theme 'challenger-deep t))

(set-face-attribute 'default nil :font "Iosevka 11")
(set-face-attribute 'line-number nil :background 'unspecified)
(set-face-attribute 'fringe nil :inherit 'line-number)

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-input-method "latin-postfix")

(setq-default abbrev-mode 1
	      recentf-mode 1
	      show-paren-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

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

(setq tramp-default-method "ssh")

;;; Packages
(use-package aggressive-indent
  :demand t
  :config
  (add-hook 'TeX-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package company
  :demand t
  :config
  (setq company-idle-delay 0
        company-selection-wrap-around t)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package counsel
  :demand t)

;; (use-package counsel-projectile
;;   :demand t
;;   :config
;;   (counsel-projectile-mode))

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t)
  :config
  (setq evil-search-wrap t
        evil-regexp-search t
        evil-complete-next-func 'hippie-expand
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-cross-lines t)
  (evil-mode 1))

(use-package evil-collection
  :demand t
  :after evil
  :init (setq evil-collection-outline-bind-tab-p nil)
  :config (evil-collection-init))

(use-package evil-surround
  :demand t
  :config
  (push '(?* . ("*" . "*")) evil-surround-pairs-alist)
  (push '(?_ . ("_" . "_")) evil-surround-pairs-alist)
  (push '(?/ . ("/" . "/")) evil-surround-pairs-alist)
  (global-evil-surround-mode))

(use-package evil-embrace
  :after evil-surround
  :demand t
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-commentary
  :after evil
  :demand t
  :config (evil-commentary-mode))

(use-package evil-org
  :after org
  :demand t
  :config
  (setq evil-org-set-key-theme '(textobjects insert navigation))
  (add-hook 'org-mode-hook 'evil-org-mode))

(use-package eyebrowse
  :demand t
  :config
  (setq eyebrowse-new-workspace t
        eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t)
  (eyebrowse-mode t))

(use-package flycheck
  :delight " Fly"
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package general
  :demand t)

(use-package key-chord
  :demand t
  :config (key-chord-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package magit
  :config (setq magit-completing-read-function 'ivy-completing-read))

(use-package markdown-mode
  :mode
  ("\\.md" . markdown-mode)
  ("\\.mdpp" . markdown-mode)
  :init
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode)
  :config
  (font-lock-add-keywords 'markdown-mode
                          '(("@[[:alnum:]]+\\(-[[:alnum:]]+\\)?" . font-lock-keyword-face))))

;; (use-package projectile
;;   :demand t
;;   :config
;;   (setq projectile-mode-line '(:eval (format " PRJ:%s" (projectile-project-name)))
;;         projectile-completion-system 'ivy)
;;   (projectile-mode))

(use-package olivetti
  :config (setq-default olivetti-body-width 90))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :demand t
  :commands (org-capture)
  :init
  (require 'cl)
  (setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
  :config
  (setq org-directory "~/org"
        org-default-notes-file "~/org/todo.org"
        org-agenda-files '("~/org/todo.org" "~/org/notes.org")
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-archive-location "~/org/archief::datetree/"
	org-cycle-separator-lines -1))

(use-package paredit
  :demand t
  :delight " ()"
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package rainbow-delimiters
  :demand t)

(use-package rainbow-mode)

(use-package smart-mode-line
  :demand t
  :config
  (line-number-mode t)
  (column-number-mode t)
  (setq sml/theme 'respectful)
  (setq sml/modified-char "+")
  (setq sml/name-width 40)
  (setq sml/shorten-modes nil)
  (setq sml/mode-width 0)
  (setq rm-whitelist
        (format "^ \\(%s\\)$"
                (mapconcat #'identity
                           '("()" "Fly" "PRJ.*" "=>")
                           "\\|")))
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/thesis" ":TH:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/" ":PRJ:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/dotfiles" ":DF:"))
  (sml/setup))

(use-package which-key
  :demand t
  :config (which-key-mode 1))

(use-package yaml-mode
  :mode
  ("\\.yml" . yaml-mode)
  ("\\.yaml" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'delete-trailing-whitespace))

(use-package yasnippet
  :demand t
  :config
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))
  (setq yas-triggers-in-field t
        yas-snippet-revival t
        yas-indent-line nil)
  (yas-global-mode 1))

(use-package dashboard
  :demand t
  :config
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 10))
	initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-setup-startup-hook))

;;; Hooks
(setq-default auto-fill-function 'do-auto-fill)

(add-hook 'focus-out-hook 'garbage-collect)

(defun pfn-setup-prog-mode ()
  "Load 'prog-mode' minor modes."
  (eldoc-mode 1)
  (rainbow-delimiters-mode)
  (display-line-numbers-mode)
  (delete-trailing-whitespace)
  (flycheck-mode 1)
  (outline-minor-mode))
(add-hook 'prog-mode-hook 'pfn-setup-prog-mode)

(defun pfn-setup-text-mode ()
  "Load 'text-mode' hooks."
  (delete-trailing-whitespace)
  (rainbow-delimiters-mode 1)
  (flyspell-mode 1))
(add-hook 'text-mode-hook 'pfn-setup-text-mode)

;;; Keys
(general-evil-setup)

(general-mmap
  "j"   'evil-next-visual-line
  "k"   'evil-previous-visual-line
  "C-e" 'evil-end-of-line
  "[ p" 'evil-paste-before
  "] p" 'evil-paste-after)

(general-def
  :keymaps 'evil-insert-state-map
  (general-chord "jj") 'evil-normal-state)

;; (general-create-definer evil-leader
;;   :states '(normal visual insert emacs)
;;   :prefix ","
;;   :non-normal-prefix "M-,")

(general-def
  :states '(normal visual insert emacs)
  :prefix ","
  :non-normal-prefix "M-,"
  "b" 'mode-line-other-buffer
  "d" 'dired-jump
  "e" 'eval-last-sexp
  "i" '(lambda () (interactive)
	 (find-file user-init-file))
  "o" 'olivetti-mode
  "p" 'counsel-yank-pop
  "q" 'kill-buffer-and-window
  "r" '(lambda () (interactive)
	 (revert-buffer :ignore-auto :noconfirm))
  "R" '(lambda () (interactive)
	 (load-file user-init-file)
	 (message "buffer reloaded"))
  "s" 'magit-status
  "," 'other-window
  "-" 'counsel-find-file)

(general-def
  ;; "C-c a"
  ;; "C-c b"
  "C-c c"   'org-capture
  ;; "C-c d"
  ;; "C-c e"
  "C-c f"   'ffap
  ;; "C-c g"
  ;; "C-c h"
  ;; "C-c i"
  ;; "C-c j"
  "C-c k"   'counsel-ag
  "C-c l"   'org-store-link
  ;; "C-c m"
  ;; "C-c n"
  ;; "C-c o"
  ;; "C-c p"
  ;; "C-c q"
  "C-c R"   '(lambda () (interactive)
               (load-file user-init-file))
  ;; "C-c s"
  ;; "C-c t"
  ;; "C-c u"
  ;; "C-c v"
  ;; "C-c w"
  "C-c x"   'org-archive-subtree
  ;; "C-c y"
  ;; "C-c z"
  "C-s"     'swiper)

;; lower garbace collection threshold


(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;;; init.el ends here
