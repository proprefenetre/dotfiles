;;; pkgs.el -- emacs packages
;;; commentary:
;;; code:

;;; Packages
(use-package racer
  :hook (rust-mode . racer-mode))

(use-package rust-mode
  :mode ("\\.rs" . rust-mode))

(use-package pyvenv
  :hook (python-mode . pyvenv-mode))

(use-package anaconda-mode
  :hook (python-mode . anaconda-mode)
  :init
  (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :init
  (add-to-list 'company-backends 'company-anaconda))

(require 'lsp-clients)
(use-package lsp-mode
  :commands lsp
  :init
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  :config
  (setq lsp-auto-guess-root t
        lsp-prefer-flymake nil
        lsp-auto-configure t))

(use-package company-lsp
  :after company
  :commands company-lsp
  :init
  (add-to-list 'company-backends 'company-lsp)
  :config
  (setq company-lsp-async t
        company-lsp-cache-candidates 'auto
        company-lsp-enable-recompletion t))

(use-package js2-mode
  :mode ("\\.js" . js2-mode)
  :hook (js2-mode . lsp))

(use-package all-the-icons)

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
        eyebrowse-switch-back-and-forth t)
  (eyebrowse-mode))

(use-package edit-indirect
  :demand t)

(use-package shackle
  :demand t
  :config
  (setq shackle-rules '((compilation-mode :noselect t)
                        ("*Flycheck error messages*" :noselect t :align below :ignore t))
        shackle-default-rule '(:select t))
  ;; (setq shackle-select-reused-windows nil) ; default nil
  ;; (setq shackle-default-alignment 'below) ; default below
  ;; (setq shackle-default-size 0.3) ; default 0.5
  (setq shackle-default-rule '(:select t :align 'below))
  (shackle-mode 1))

(use-package aggressive-indent
  :demand t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package company
  :demand t
  :config
  (setq company-idle-delay 0
        company-selection-wrap-around t
        company-require-match 'never)
  (global-company-mode))

(use-package counsel
  :demand t)

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
  ;; (defadvice evil-inner-word (around underscore-as-word activate)
  ;;   (let ((table (copy-syntax-table (syntax-table))))
  ;;     (modify-syntax-entry ?_ "w" table)
  ;;     (with-syntax-table table
  ;;       ad-do-it)))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :demand t
  :init (setq evil-collection-outline-bind-tab-p nil
              evil-collection-setup-minibuffer t)
  :config
  ;; (setq evil-collection-mode-list (delete 'company evil-collection-mode-list))
  (evil-collection-init))

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
  :commands (projectile-switch-project)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (setq-default flycheck-disabled-checker 'javascript-jshint)
  (setq-default flycheck-disabled-checker 'json-jsonlist)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setq flycheck-flake8rc "~/.flake8"))

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

  (setq org-todo-keywords '((type "AFSPRAAK(a)" "GOOGLE(g)" "READ(r)" "NB(n)" "|"
                                  "DONE(d)")
                            (sequence "TODO(t)" "STARTED(s)" "AFWACHTEN(w)" "BEZIG(b)" "|" "DONE(d)" "CANCELED(c)")))


  (setq org-todo-keyword-faces
        '(("TODO" . "yellow")
          ("BEZIG" . "SpringGreen")
          ("AFWACHTEN" . "SpringGreen" )
          ("READ" . "cyan")
          ("GOOGLE" . "cyan")
          ("AFSPRAAK" . "magenta")
          ("CANCELED" . "red")
          ("NB". "orange")))

  (setq org-capture-templates
        '(("d" "dict" entry (file+headline "~/org/dict.org" "Words") "* %? :: "))))

(use-package smartparens
  :demand t
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "<" nil :actions nil)
  (add-to-list 'sp-sexp-suffix (list #'rust-mode 'regexp ";"))
  (electric-pair-mode 0)
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :demand t)

(use-package rainbow-mode)

(use-package doom-modeline
  :config
  (setq doom-modeline-height 10
        doom-modeline-bar-width 3
        column-number-mode t
        doom-modeline-icon t)
  :hook (after-init . doom-modeline-mode))

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

(use-package yasnippet-snippets
  :after yasnippet)

(use-package racket-mode)

(provide 'pkgs)
;;; pkgs.el ends here
