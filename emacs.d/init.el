;;; init.el -- a fresh shart
;;; Commentary:
;;; Code:

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

;; (set-face-attribute 'default nil :font "Hack 13")
(set-face-attribute 'default nil :font "Fantasque Sans Mono 13")
(set-face-attribute 'line-number nil :background 'unspecified)
(set-face-attribute 'fringe nil :inherit 'line-number)

(fset 'yes-or-no-p 'y-or-n-p)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; builtins
(setq default-frame-alist
      '((width . 120)
        (height . 38)))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode 1)
(fringe-mode '(8 . 8))
(recentf-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(global-display-line-numbers-mode)

(setq-default locale-coding-system 'utf-8
              default-input-method "latin-postfix"
              indent-tabs-mode nil
              tab-width 4
              fill-column 120
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
              tramp-default-method "ssh"
              abbrev-mode t
              save-abbrevs 'silent
              desktop-restore-frames nil
              inhibit-startup-screen t)


(dolist (table abbrev-table-name-list)
  (abbrev-table-put (symbol-value table) :case-fixed t))

(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

;; (setq ispell-silently-savep t
;;       ispell-dictionary "dutch"
;;       ispell-extra-args '("-a" "utf-8"))

;; (if (string-equal "darwin" (symbol-name system-type))
;;     (setenv "PATH" (concat "/usr/local/bin:/usr/local/sbin:" (getenv "PATH"))))

;; Packages
(setq custom-safe-themes t)

(use-package all-the-icons)

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  ;; (setq doom-challenger-deep-brighter-comments t)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; doom-modeline-height 11
  ;; doom-modeline-bar-width 3
  (setq column-number-mode t
        doom-modeline-icon t))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :init
  (require 'cl)
  (setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
  :config
  (symbol-overlay-mode -1)
  (setq initial-major-mode 'org-mode
        initial-scratch-message "")
  (set-face-attribute 'org-level-1 nil :height 1.0 :box nil)
  (setq org-directory "~/org"
        org-default-notes-file "~/org/todo.org"
        org-agenda-files '("~/org/todo.org")
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-allow-creating-parent-nodes t
        org-refile-use-outline-path 'file
        org-archive-location "~/org/archief::datetree/"
        org-cycle-separator-lines -1
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil))
        org-M-RET-may-split-line '((default . nil))
        org-return-follows-link t
        org-reverse-note-order t
        org-outline-path-complete-in-steps nil
        org-use-speed-commands t
        org-pretty-entities t
        org-log-done nil
        org-startup-indented t)

  (setq org-capture-templates
        '(("c" "Capture" entry (file+headline "~/org/todo.org" "Inbox")
           "* %?\n")))

  (setq org-todo-keywords '((type "AFSPRAAK(a)" "GOOGLE(g)" "READ(r)" "NB(n)" "IDEE(i)" "|"
                                  "DONE(d)")
                            (sequence "TODO(t)" "STARTED(s)" "AFWACHTEN(w)" "BEZIG(b)" "|" "DONE(d)" "CANCELED(c)")))

  (setq org-todo-keyword-faces
        '(("TODO" . "yellow")
          ("BEZIG" . "SpringGreen")
          ("AFWACHTEN" . "OliveDrab" )
          ("READ" . "cyan")
          ("GOOGLE" . "cyan")
          ("AFSPRAAK" . "magenta")
          ("CANCELED" . "red")
          ("IDEE" . "orange")
          ("NB". "orange")))

  (defun org-keyword-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'org-keyword-backend))
      (prefix (and (eq major-mode 'org-mode)
                   (cons (company-grab-line "^#\\+\\(\\w*\\)" 1)
                         t)))
      (candidates (mapcar #'upcase
                          (cl-remove-if-not
                           (lambda (c) (string-prefix-p arg c))
                           (pcomplete-completions))))
      (ignore-case t)
      (duplicates t)))
  (add-to-list 'company-backends 'org-keyword-backend)
  )


(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-want-C-w-delete t)
  :config
  (add-to-list 'desktop-locals-to-save 'evil-markers-alist)
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "W" 'evil-write))
  (evil-set-initial-state 'treemacs-mode 'normal)
  (setq evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-search-wrap t
        evil-regexp-search t
        evil-complete-next-func 'hippie-expand
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-cross-lines t
        evil-ex-substitute-global t)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :demand t
  :init
  (setq evil-collection-outline-bind-tab-p nil
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

(use-package evil-org
  :after org
  :demand t
  :config
  (setq evil-org-set-key-theme '(textobjects insert navigation))
  (add-hook 'org-mode-hook 'evil-org-mode))

(use-package evil-magit
  :demand t
  :config
  (setq evil-magit-state 'normal))

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

  ;; movement, pasting
  (general-mmap
    "j"   'evil-next-visual-line
    "k"   'evil-previous-visual-line
    "C-e" 'evil-end-of-line
    "[ p" 'evil-paste-before
    "] p" 'evil-paste-after
    "M-s" 'avy-goto-word-1)

  ;; chords
  (general-def
    :keymaps 'evil-insert-state-map
    (general-chord "jj") 'evil-normal-state
    (general-chord "ww") 'evil-window-next)

  ;; leader key
  (general-override-mode)

  (general-def '(normal visual emacs treemacs) override
    :prefix ","
    :non-normal-prefix "M-,"
    "b" 'mode-line-other-buffer
    "d" 'dired-jump
    "e" 'eval-last-sexp
    "g" 'evil-commentary-yank-line
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
    "t" 'treemacs
    "w" 'ace-window)

  (general-def org-mode-map
    :prefix "C-c"
    "a"   'org-agenda-list
    "C-a" 'org-archive-subtree
    "r"   'org-refile)                          ; C-c binds

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
    "L"   '(lambda () (interactive)
             (load-file buffer-file-name))
    ;; "m" "n" "o"
    "p"   'projectile-command-map
    ;; "q"
    "R"   '(lambda () (interactive)
             (load-file user-init-file))
    "s"   'cycle-ispell-languages
    ;; "t"   'treemacs
    ;; "u"
    ;; "v"
    "w"   'ace-window
    ;;"x"
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
    "t M-t" 'treemacs-find-tag
    "C-b" 'counsel-ibuffer)

  (general-def
    "M-/" 'hippie-expand
    "C-)" 'sp-forward-slurp-sexp
    "M-s" 'avy-goto-word-1)

  (general-def 'visual
    ")" 'er/expand-region)

  (general-def company-active-map
    "C-w" 'evil-delete-backward-word
    "C-n"  'company-select-next
    "C-p"  'company-select-previous
    "<tab>"  'company-complete-common-or-cycle
    )
  )

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
        company-echo-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-require-match 'never)
  (setq company-backends
        '((company-files
           company-keywords
           company-capf
           company-yasnippet)
          (company-abbrev company-dabbrev)
          ))
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

(use-package edit-indirect)

(use-package expand-region)

(use-package avy)

(use-package shackle
  :demand t
  :config
  (setq shackle-rules '(("*Flycheck error messages*" :noselect t :align 'below
                         :ignore t)
                        ("*Python*" :noselect t :size .25 :align :'below)
                        ("COMMIT_EDITMSG" :select t)
                        (magit-status-mode :regexp t :same t :inhibit-window-quit t)))
  (setq shackle-default-rule '(:select t :align 'below))
  (shackle-mode 1))

(use-package magit
  :demand t)

(use-package rainbow-delimiters
  :demand t)

(use-package smartparens
  :demand t
  :config
  (require 'smartparens-config)
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "+" "+")
  (sp-local-pair 'org-mode "/" "/")
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (add-to-list 'sp-sexp-suffix (list #'rust-mode 'regexp ";"))
  (electric-pair-mode 0)
  (smartparens-global-mode))

(use-package rainbow-mode
  :demand t
  :config
  (rainbow-mode 1))

(use-package aggressive-indent
  :demand t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

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
  (yas-global-mode 1))

(use-package projectile
  :demand t
  :config
  (projectile-mode))

(use-package counsel-projectile
  :demand t
  :config
  (counsel-projectile-mode))


;; Python

;; (use-package eglot
;;   :demand t
;;   :config
;;   (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
;;   (add-to-list 'eglot-server-programs '(python-mode . ("/usr/local/bin/pyls")))
;;   (add-hook 'python-mode-hook 'eglot-ensure))

;; (use-package lsp-mode
;;   :commands (lsp)
;;   :config
;;   (add-hook 'python-mode-hook 'lsp)
;;   )
(use-package python
  :ensure nil
  :mode ("\\.py" . python-mode)
  :config
  (flymake-mode-off)
  (setq flycheck-python-flake8-executable "/usr/local/bin/flake8"
        flycheck-flake8rc "~/.config/flake8")
  (setq python-shell-interpreter "/usr/local/bin/ipython"
        python-shell-interpreter-args "--simple-prompt")
  (setq python-indent-offset 4)
  (flycheck-mode))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top)
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode))

(use-package pyvenv)

;; (use-package elpy
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;;   :custom
;;   (elpy-rpc-backend "jedi"))

;; (use-package company-jedi
;;   :init
;;   (add-to-list 'company-backends 'company-jedi)
;;     (add-hook 'python-mode-hook 'enable-jedi)))

(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :init
  (add-to-list 'company-backends 'company-anaconda))


(use-package ace-window
  :demand t
  :config
  ;; setq aw-keys '(?a ?o ?e ?u ?i ?d ?t ?n ?s)
  (setq aw-scope 'frame))

(use-package treemacs
  :demand t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (setq treemacs-width 28
        treemacs-python-executable "/usr/local/bin/python")
  (treemacs-git-mode 'deferred)
  )

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
  :delight " Fly"
  :commands (projectile-switch-project)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (global-flycheck-mode))

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package docker-tramp)

(use-package symbol-overlay
  :demand t
  :config
  (symbol-overlay-mode))


;;; other stuff
(add-to-list 'load-path "~/.emacs.d/etc/lisp/")
(require 'my-functions)

(add-hook 'focus-out-hook 'garbage-collect)
(add-hook 'sh-mode-hook 'aggressive-indent-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

(defun pfn-setup-prog-mode ()
  "Load 'prog-mode' minor modes."
  (eldoc-mode 1)
  (auto-fill-mode 1)
  (outline-minor-mode 1)
  (smartparens-mode 1)
  (rainbow-delimiters-mode 1))
(add-hook 'prog-mode-hook 'pfn-setup-prog-mode)

(defun pfn-setup-text-mode ()
  "Load 'text-mode' hooks."
  (delete-trailing-whitespace)
  (turn-on-auto-fill)
  (rainbow-delimiters-mode 1)
  ;; (flyspell-mode 1)
  (electric-pair-mode 1))
(add-hook 'text-mode-hook 'pfn-setup-text-mode)

(message (getenv "PATH"))
;;; init.el ends here
