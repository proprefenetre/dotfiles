;;; init.el -- a fresh shart

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))

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

(set-face-attribute 'default nil :font "Hack 10")
(set-face-attribute 'line-number nil :background 'unspecified)
(set-face-attribute 'fringe nil :inherit 'line-number)

(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

(setq-default locale-coding-system 'utf-8
	      default-input-method "latin-postfix"
	      indent-tabs-mode nil
	      tab-width 4
	      fill-column 80
	      scroll-margin 10
	      scroll-conservatively most-positive-fixnum
	      auto-fill-function 'do-auto-fill
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
	      tramp-default-method "ssh")

(dolist (table abbrev-table-name-list)
  (abbrev-table-put (symbol-value table) :case-fixed t))

(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(abbrev-mode 1)
(recentf-mode 1)
(show-paren-mode 1)
(fringe-mode '(8 . 8))

(setq ispell-silently-savep t
      ispell-dictionary "dutch"
      ispell-extra-args '("-a" "utf-8"))

;; Packages
(setq custom-safe-themes t)

(use-package all-the-icons)
(use-package doom-themes
  :init
  (load-theme 'doom-vibrant t)
  :config
  ;; (setq-default doom-neotree-file-icons t)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 11
        doom-modeline-bar-width 3
        column-number-mode t
        doom-modeline-icon t))

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-want-C-w-delete t)
  :config
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "W" 'evil-write))

  (setq evil-search-wrap t
        evil-regexp-search t
        evil-complete-next-func 'hippie-expand
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-cross-lines t)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :demand t
  :init (setq evil-collection-outline-bind-tab-p nil
              evil-collection-setup-minibuffer t)
  :config
  (setq evil-collection-mode-list (delete 'company evil-collection-mode-list))
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :demand t
  :config (evil-commentary-mode))

(use-package evil-surround
  :after evil
  :demand t
  :config
  (global-evil-surround-mode))

(use-package general
  :demand t)

(use-package key-chord
  :demand t
  :config (key-chord-mode 1))

(use-package which-key
  :demand t
  :config (which-key-mode 1))

(use-package smex
  :demand t)

(use-package ivy
  :demand t
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :demand t)

(use-package company
  :demand t
  :config
  (setq company-idle-delay 0
        company-selection-wrap-around t
        company-require-match 'never)
  (add-to-list 'company-frontends 'company-tng-frontend)
  (global-company-mode))

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

(use-package company-prescient
  :after prescient
  :demand t
  :config
  (company-prescient-mode 1))

(use-package edit-indirect
  :demand t)

(use-package shackle
  :demand t
  :config
  (setq shackle-rules '((compilation-mode :noselect t)
                        ("*Flycheck error messages*" :noselect t :align below :ignore t)))
  (setq shackle-default-rule '(:select t :align 'below))
  (shackle-mode 1))

(use-package magit
  :demand t)

(use-package rainbow-delimiters
  :demand t)

(use-package aggressive-indent
  :demand t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package yasnippet
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
  (yas-global-mode 1))


;;; keybinding
(setq tab-always-indent t)

(general-evil-setup)

                                        ; movement, pasting
(general-mmap
  "j"   'evil-next-visual-line
  "k"   'evil-previous-visual-line
  "C-e" 'evil-end-of-line
  "[ p" 'evil-paste-before
  "] p" 'evil-paste-after)

                                        ; bind esc
(general-def
  :keymaps 'evil-insert-state-map
  (general-chord "jj") 'evil-normal-state)

                                        ; leader key
(general-def '(normal visual emacs)
  :prefix ","
  :non-normal-prefix "M-,"
  :keymaps '(override inferior-ess-r-mode-map)
  ;; "b" 'switch-to-prev-buffer
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
  "s" 'magit-status)

                                        ; C-c binds
(general-def
  :prefix "C-c"
  "a"   'org-agenda
  "b"   'counsel-bookmark
  "c"   'compile
  ;; "d"
  "C-d" 'dired-jump-other-window
  ;; "e"
  "f"   'ffap-other-window
  ;; "g" "h"
  "i"   'ibuffer
  ;; "j"
  "k"   'counsel-ag
  "l"   'org-store-link
  "L"   '(lambda () (interactive)
           (load-file buffer-file-name))
  ;; "m" "n" "o"
  "p"   'projectile-command-map
  ;; "q"
  "r"   'org-refile
  "R"   '(lambda () (interactive)
           (load-file user-init-file))
  "s"   'cycle-ispell-languages
  ;; "t" "u" "v" "w" "x"
  "C-l" 'comint-clear-buffer
  )

(general-def
  :prefix "C-x"
  "ESC ESC" nil
  "t t" 'treemacs
  "t 0" 'treemacs-select-window
  "t 1" 'treemacs-delete-other-windows
  "t B" 'treemacs-bookmark
  "t C-t" 'treemacs-find-file
  "t M-t" 'treemacs-find-tag)

(general-def
  "M-/" 'hippie-expand
  "C-)" 'sp-forward-slurp-sexp)

(general-def 'normal racket-repl-mode-map
  :prefix "C-w"
  "C-w" 'other-window)

(general-def company-active-map
  "C-w" 'evil-delete-backward-word
  "TAB" 'company-select-next
  "<tab>" 'company-select-next
  "<backtab>" 'company-select-previous
  "RET" nil)

(general-def 'insert yas-keymap
  "TAB" 'yas-next-field-or-maybe-expand
  "<tab>" 'yas-next-field-or-maybe-expand
  "<backtab>" 'yas-prev)

;;; other stuff
(add-to-list 'load-path "~/.emacs.d/etc/lisp/")
(require 'functions)
(require 'packages)

(add-hook 'focus-out-hook 'garbage-collect)
(add-hook 'sh-mode-hook 'aggressive-indent-mode)

(defun pfn-setup-prog-mode ()
  "Load 'prog-mode' minor modes."
  (eldoc-mode 1)
  (auto-fill-mode 1)
  (rainbow-delimiters-mode 1)
  (rainbow-mode 1)
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
;;; init.el ends here
