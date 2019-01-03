;; init.el --- Emacs Configuration
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
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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
;; (add-to-list 'load-path "/home/niels/dotfiles/emacs.d/var/lisp")
(add-to-list 'load-path "/home/niels/dotfiles/emacs.d")

(require 'custom-functions)

;;; Settings
(setq custom-safe-themes t)

(use-package challenger-deep-theme
  :demand t
  :config
  (load-theme 'challenger-deep t))

;; theme colors
;; "#906cff"
;; "#91ddff"
;; "#65b2ff"
;; "#ffd75f"
;; "#62d196"
;; "#ff5458"
;; "#ffe9aa"
;; "#95ffa4"
;; "#aaffe4"
;; "#ff8080"

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
	          show-paren-mode 1
              electric-pair-mode 1)

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
              scroll-margin 20
              scroll-conservatively most-positive-fixnum)

(setq-default auto-fill-function 'do-auto-fill)

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

;; (setq compilation-read-command nil)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;;; Packages
(use-package s)

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

(use-package projectile
  :demand t
  :config
  (projectile-mode))

(use-package counsel-projectile
  :demand t
  :config
  (counsel-projectile-mode))

(use-package smex
  :demand t)

(use-package poly-markdown)

(use-package polymode
  :mode
  ("\\.Rnw" . poly-noweb+r-mode)
  ("\\.Rmd" . poly-markdown+r-mode))

(use-package ess
  :config
  (setq ess-eval-visibly 'nowait))

(use-package eyebrowse
  :demand t
  :config
  (setq eyebrowse-new-workspace t
	    eyebrowse-wrap-around t
	    eyebrowse-switch-back-and-forth t))

(use-package edit-indirect
  :demand t)

(use-package shackle
  :demand t
  :config
  (setq shackle-rules '((compilation-mode :noselect t))
        shackle-default-rule '(:select t))
  ;; (setq shackle-select-reused-windows nil) ; default nil
  ;; (setq shackle-default-alignment 'below) ; default below
  ;; (setq shackle-default-size 0.3) ; default 0.5
  ;; (setq shackle-default-rule '(:select t :align 'below))
  (shackle-mode 1))

(use-package aggressive-indent
  :demand t
  :config
  (add-hook 'TeX-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package company
  :demand t
  :config
  (setq company-idle-delay 0
        company-selection-wrap-around t
        company-require-match 'never)
  (setq company-backends
        '(company-files
          (company-yasnippet
           company-keywords
           company-capf)
          (company-dabbrev-code
           company-dabbrev
           company-abbrev))))

(use-package counsel
  :demand t)

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
  (defadvice evil-inner-word (around underscore-as-word activate)
    (let ((table (copy-syntax-table (syntax-table))))
      (modify-syntax-entry ?_ "w" table)
      (with-syntax-table table
        ad-do-it)))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :demand t
  :init (setq evil-collection-outline-bind-tab-p nil
	          evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package evil-numbers
  :demand t)

(use-package evil-surround
  :demand t
  :config
  (push '(?* . ("**" . "**")) evil-surround-pairs-alist)
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

(use-package evil-magit
  :after '(evil magit)
  :config
  (setq evil-magit-state 'normal))

(use-package markdown-mode
  :mode
  ;; ("\\.md" . markdown-mode)
  ("\\.mdpp" . markdown-mode)
  :init
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode)
  :config
  (font-lock-add-keywords 'markdown-mode
                          '(("@[^ ]+"
                             . font-lock-variable-name-face)))
  (font-lock-add-keywords 'markdown-mode
                          '(("\\(@fig:\\|@tbl:\\)\\([^ ]+\\)"
                             (1 font-lock-keyword-face) (2 font-lock-variable-name-face))))
  (font-lock-add-keywords 'markdown-mode
                          '(("(b?red .*)" . font-lock-keyword-face)))
  )

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
        org-refile-allow-creating-parent-nodes t
        org-refile-use-outline-path t
        org-archive-location "~/org/archief::datetree/"
	    org-cycle-separator-lines -1
	    org-blank-before-new-entry '((heading . nil)
				                     (plain-list-item . nil))
	    org-M-RET-may-split-line '((default . nil))
        org-return-follows-link t
	    org-reverse-note-order t
        org-outline-path-complete-in-steps nil)

  (setq org-todo-keyword-faces
        '(("READ" . "#65b2ff")
          ("GOOGLE" . "#65b2ff")
          ("AFSPRAAK" . "#906cff")
          ("WEEKLY" . "#aaffe4")))

  (setq org-capture-templates
	    '(("d" "dict" entry (file+headline "~/org/dict.org" "Words") "* %? :: "))))

(use-package paredit
  :demand t
  :delight " ()"
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package rainbow-delimiters
  :demand t)

(use-package rainbow-mode)

;; (setq mode-line-format
;;       (list
;;        " "
;;        mode-line-mule-info
;;        'mode-line-modified
;;        "-  "
;;        'mode-line-buffer-identification
;;        ;; 'mode-line-modes
;;        " -- "
;;        `(vc-mode vc-mode)

;;        ;; Fill until the end of line but 10 characters
;;        (mode-line-fill 'mode-line 10)
;;        "(%l, %c)"))

(use-package smart-mode-line
  :demand t
  :config
  (line-number-mode t)
  (column-number-mode t)
  (setq sml/theme 'respectful)
  (setq sml/modified-char "+")
  (setq sml/name-width 40)
  (setq sml/mode-width 'full)
  (setq rm-whitelist
        (format "^ \\(%s\\)$" (mapconcat #'identity
                                         '("()" "Fly" "PRJ.*" "=>") "\\|")))
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/thesis" ":thesis:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/" ":projects:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/dotfiles" ":df:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/org" ":org:"))
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
        yas-indent-line 'nil
        yas-wrap-around-region t
        yas-indent-line 'auto
        yas-also-auto-indent-first-line t)
  (yas-global-mode 1))

;;; Hooks & Builtins
(require 'table)
(add-hook 'focus-out-hook 'garbage-collect)
(add-hook 'sh-mode-hook 'aggressive-indent-mode)

(defun pfn-setup-prog-mode ()
  "Load 'prog-mode' minor modes."
  (eldoc-mode 1)
  (auto-fill-mode)
  (rainbow-delimiters-mode)
  (delete-trailing-whitespace)
  (flycheck-mode 1)
  (outline-minor-mode)
  (company-mode-on)
  (display-line-numbers-mode))
(add-hook 'prog-mode-hook 'pfn-setup-prog-mode)

(defun pfn-setup-text-mode ()
  "Load 'text-mode' hooks."
  (delete-trailing-whitespace)
  (turn-on-auto-fill)
  (rainbow-delimiters-mode 1)
  (flyspell-mode 1))
(add-hook 'text-mode-hook 'pfn-setup-text-mode)

;;; Keys
(setq tab-always-indent 'complete)

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

(general-def 'normal (ibuffer-mode-map image-mode-map inferior-ess-r-mode-map)
  "," nil
  "C-c C-w" 'eyebrowse-last-window-config)

(general-def 'insert markdown-mode-map
  "TAB"   'yas-expand
  "<tab>" 'yas-expand)

(dolist (key '("<return>" "RET"))
  (general-def company-active-map key
    `(menu-item nil company-complete
		        :filter ,(lambda (cmd)
			               (when (company-explicit-action-p)
			                 cmd)))))

(general-def 'insert company-active-map
  "TAB" 'company-complete-common-or-cycle
  "SPC" nil
  "C-w" 'evil-delete-backward-word)

(general-def '(normal visual emacs)
  :prefix ","
  :non-normal-prefix "M-,"
  :keymaps '(override inferior-ess-r-mode-map)
  "b" 'switch-to-previous-buffer
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

(general-def
  :prefix "C-c"
  "a"   'org-agenda
  "b"   'counsel-bookmark
  "c"   'compile
  "d"   'quick-calc
  ;; "e"
  "f"   'ffap
  ;; "g" "h"
  "i"   'ibuffer
  ;; "j"
  "k"   'counsel-ag
  "l"   'org-store-link
  ;; "m" "n" "o"
  "p"   'projectile-command-map
  ;; "q"

  "r"   'counsel-recentf
  "R"   '(lambda () (interactive)
           (load-file user-init-file))
  "s"   'cycle-ispell-languages
  ;; "t" "u" "v" "w" "x"
  )


(general-def
  :prefix "C-h"
  "f"   'counsel-describe-function
  "v"   'counsel-describe-variable)

(general-def
  :prefix "C-x"
  "f"   'counsel-find-file
  "C-f" 'counsel-find-file
  "ESC ESC" 'nil)

(general-def
  "C-;"     'evil-repeat-find-char-reverse
  "C-s"     'swiper
  "M-/"     'hippie-expand
  "<M-tab>" 'company-complete
  "M-x"     'counsel-M-x)

;; lower garbage collection threshold
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;;; init.el ends here
