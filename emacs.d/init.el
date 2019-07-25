; ;;init.el -- not so fresh anymore!
;;; Commentary:
;;; Code:
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

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

(use-package exec-path-from-shell
  :demand t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (setenv "PKG_CONFIG_PATH" "/usr/local/opt/libffi/lib/pkgconfig")
    (message (getenv "PATH"))))

(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
(load custom-file)

(set-face-attribute 'default nil :font "Fantasque Sans Mono 15")
;; (set-face-attribute 'line-number nil :background 'unspecified)
;; (set-face-attribute 'fringe nil :inherit 'line-number)

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
;; (global-hl-line-mode 1)
(global-auto-revert-mode 1)
(global-display-line-numbers-mode)
(global-eldoc-mode)

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
              ;; tramp-default-method "ssh"
              abbrev-mode t
              save-abbrevs 'silent
              desktop-restore-frames nil
              inhibit-startup-screen t
              auto-fill-function 'do-auto-fill
              auto-fill-mode -1)

(dolist (table abbrev-table-name-list)
  (abbrev-table-put (symbol-value table) :case-fixed t))

(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

;; Packages
(use-package all-the-icons)

(setq custom-safe-themes t)

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  (setq doom-one-brighter-comments t)
  (doom-themes-org-config))


(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; doom-modeline-height 11
  ;; doom-modeline-bar-width 3
  (setq column-number-mode t
        doom-modeline-icon t))

;; (use-package mood-line
;;   :hook (after-init . mood-line-mode))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :init
  (require 'cl)
  (setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
  :config
  (rainbow-delimiters-mode 1)
  (display-line-numbers-mode -1)
  (set-face-attribute 'org-level-1 nil :height 1.0 :box nil)
  (setq org-directory "~/Dropbox/org"
        org-default-notes-file "~/Dropbox/org/todo.org"
        org-agenda-files '("~/Dropbox/org/todo.org" "~/Dropbox/org/notes.org" "~/Dropbox/org/inbox.org")
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-allow-creating-parent-nodes t
        org-refile-use-outline-path 'file
        org-archive-location "~/Dropbox/org/archief::datetree/"
        org-cycle-separator-lines -1
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil))
        ;; org-M-RET-may-split-line '((default . nil))
        org-return-follows-link t
        org-reverse-note-order t
        org-outline-path-complete-in-steps nil
        org-use-speed-commands t
        org-pretty-entities t
        org-log-done nil
        org-startup-indented t)

  (setq org-latex-toc-command "\\tableofcontents \\clearpage")

  (setq org-capture-templates
        '(("c" "Capture" entry (file "~/Dropbox/org/inbox.org")
           "* TODO %?\n")))

  (setq org-todo-keywords '((type "AFSPRAAK(a)" "GOOGLE(g)" "READ(r)" "NB(n)" "IDEE(i)" "|"
                                  "DONE(d)")
                            (sequence "FIXME(f)" "TODO(t)" "STARTED(s)" "AFWACHTEN(w)" "BEZIG(b)" "|" "DONE(d)" "CANCELED(c)")))

  (setq org-todo-keyword-faces
        '(("TODO" . "yellow")
          ("FIXME" . "red")
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
  (add-to-list 'company-backends 'org-keyword-backend))

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

(use-package evil-embrace
  :after evil
  :demand t
  :hook ((python-mode . (lambda () (embrace-add-pair ?a "\"\"\"" "\"\"\"" )))
         (org-mode (lambda () (embrace-add-pair ?a "_" "_")))
         (org-mode (lambda () (embrace-add-pair ?a "_" "_")))
         (org-mode (lambda () (embrace-add-pair ?a "*" "*")))
         (org-mode (lambda () (embrace-add-pair ?a "**" "**"))))
  ;; (add-hook 'python-mode-hook (lambda () (embrace-add-pair ?a "\"\"\"" "\"\"\"" )))
  ;; (add-hook 'org-mode-hook (lambda () (embrace-add-pair ?a "_" "_")))
  ;; (add-hook 'org-mode-hook (lambda () (embrace-add-pair ?a "*" "*")))
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-org
  :after org
  :demand t
  :hook (org-mode . evil-org-mode)
  :config
  (setq evil-org-set-key-theme '(textobjects insert navigation))
  ;; (add-hook 'org-mode-hook 'evil-org-mode)
  )

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
  (general-override-mode)

  ;; movement, pasting
  (general-mmap
    "j"   'evil-next-visual-line
    "k"   'evil-previous-visual-line
    "C-e" 'evil-end-of-line
    "[ p" 'evil-paste-before
    "] p" 'evil-paste-after
    "] f" 'flycheck-next-error
    "[ f" 'flycheck-previous-error
    "`"   'evil-avy-goto-char)

  ;; chords
  (general-def
    :keymaps 'evil-insert-state-map
    (general-chord "jj") 'evil-normal-state
    (general-chord "ww") 'evil-window-next)

  (general-def
    :keymaps 'evil-normal-state-map
    (general-chord "ib") 'ibuffer)

  ;; leader key
  (general-create-definer evil-leader
    :prefix ",")

  (evil-leader
    :states '(normal visual emacs treemacs)
    :keymaps 'override
    ;; "b" 'mode-line-other-buffer
    "b" 'frog-jump-buffer
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
    "R" '(lambda () (interactive)
           (load-file user-init-file)
           (message "buffer reloaded"))
    "n" 'symbol-overlay-rename
    "s" 'magit-status
    "t" 'treemacs-select-window
    "w" 'ace-window)

  (general-def org-mode-map
    :prefix "C-c"
    "a"   'org-agenda-list
    "C-a" 'org-archive-subtree
    "r"   'org-refile
    "!"   'org-time-stamp-inactive)                          ; C-c binds

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
    "s"   'counsel-rg
    "t"   'treemacs
    ;; "u"
    ;; "v"
    "w"   'ace-window
    ;;"x"
    "C-l" 'comint-clear-buffer
    )

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
    "M-/" 'hippie-expand
    "C-)" 'sp-forward-slurp-sexp
    "C-(" 'sp-add-to-previous-sexp
    "M-s" 'avy-goto-word-1)


  (general-def 'visual
    ")" 'er/expand-region)

  (general-def company-active-map
    "C-w" 'evil-delete-backward-word
    "C-n"  'company-select-next
    "C-p"  'company-select-next
    "<tab>" 'company-complete
    "<esc>" 'company-cancel)
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
        '((company-capf
           company-yasnippet
           company-files
           company-keywords)
          (company-abbrev company-dabbrev)))
  (global-company-mode))

;; (use-package company-posframe
;;   :hook company-mode
;;   ;; (add-hook 'company-mode-hook 'company-posframe-mode)
;;   )

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
  (sp-local-pair 'org-mode "/" "/")
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (add-to-list 'sp-sexp-suffix (list #'rust-mode 'regexp ";"))
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
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'dockerfile-mode)
  )

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

(use-package treemacs
  :demand t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (setq treemacs-width 28
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
  :delight " Fly"
  :commands (projectile-switch-project)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (global-flycheck-mode))

;; Python
(use-package python
  :ensure nil
  :mode ("\\.py" . python-mode)
  :init
  (setq flycheck-python-flake8-executable "/usr/local/bin/flake8"
        flycheck-flake8rc "~/.config/flake8")
  (setq python-shell-interpreter "/usr/local/bin/ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  (setq-default python-indent-offset 4))

(use-package anaconda-mode
  :demand t
  :hook python-mode
  :init
  (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode)
  )

(use-package company-anaconda
  :hook (python-mode . (lambda () (add-to-list 'company-backends 'company-anaconda)))
  :init
  ;; (add-hook 'python-mode-hook '(lambda () (add-to-list 'company-backends 'company-anaconda)))
  ;; :config
  ;; (add-to-list 'company-backends 'company-anaconda)
 )

(use-package symbol-overlay
  :demand t
  :hook (python-mode . symbol-overlay-mode)
  :config
  (setq symbol-overlay-displayed-window t)
  ;; (add-hook 'python-mode-hook 'symbol-overlay-toggle-in-scope)
  ;; (add-hook 'python-mode-hook 'symbol-overlay-mode))
  )

(use-package highlight-indent-guides
  :demand t
  :hook (python-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top)
  ;; (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  )

(use-package auto-virtualenv
  :demand t
  :hook ((python-mode window-configuration-change) . auto-virtualenv-set-virtualenv)
  ;; :config
  ;; (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
  ;; (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  )

(use-package ace-window
  :demand t
  :config
  (setq aw-scope 'frame))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-tramp)

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode))

(use-package yaml-mode
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode)
  :hook ((yaml-mode . display-line-numbers-mode)
         (yaml-mode . delete-trailing-whitespace))
  ;; :config
  ;; (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  ;; (add-hook 'yaml-mode-hook 'delete-trailing-whitespace)
  )


(use-package json-mode
  :mode
  ("\\.json\\'" . json-mode)
  :hook ((json-mode . display-line-numbers-mode)
         (json-mode . delete-trailing-whitespace))
  ;; :config
  ;; (add-hook 'json-mode-hook 'display-line-numbers-mode)
  ;; (add-hook 'json-mode-hook 'delete-trailing-whitespace)
  )

(use-package hydra
  :demand t)

(use-package markdown-mode
  :mode
  ("\\.md" . markdown-mode)
  ("\\.mdpp\\'" . markdown-mode))

(use-package olivetti
  :config
  (setq olivetti-body-width 120)
  (display-line-numbers-mode -1))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-multiframe))

(use-package pdf-tools)

;; (use-package posframe)

(use-package frog-jump-buffer)

(use-package highlight-numbers)

(use-package highlight-operators)

(use-package highlight-escape-sequences)

(use-package rust-mode
  :mode ("\\.rs\\'". rust-mode)
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
  :hook ((rust-mode . racer-mode)
         (racer-mode . company-mode))
  ;; :init
  ;; (add-hook 'rust-mode-hook 'racer-mode)
  ;; (add-hook 'racer-mode-hook 'company-mode)
  )

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package toml-mode)

;;; other stuff
(add-to-list 'load-path "~/.emacs.d/etc/lisp/")
(require 'my-functions)

;; hooks
(add-hook 'focus-out-hook 'garbage-collect)
(add-hook 'sh-mode-hook 'aggressive-indent-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

(defun pfn-setup-prog-mode ()
  "Load 'prog-mode' minor modes."
 (highlight-numbers-mode)
 (hes-mode)
 (rainbow-delimiters-mode 1))    ;; highlight escape sequences (rainbow-delimiters-mode 1))
(add-hook 'prog-mode-hook 'pfn-setup-prog-mode)

(defun pfn-setup-text-mode ()
  "Load 'text-mode' hooks."
  (delete-trailing-whitespace)
  (turn-on-auto-fill)
  (aggressive-indent-mode -1))
(add-hook 'text-mode-hook 'pfn-setup-text-mode)

(setq gc-cons-threshold 20000000
      gc-cons-percentage 0.1)
;;; Init.el ends here
