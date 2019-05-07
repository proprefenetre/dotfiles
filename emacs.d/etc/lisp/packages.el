;;; packages.el -- emacs packages
;;; commentary:
;;; code:

;; Python
(use-package pyvenv
  :init
  (add-hook 'anaconda-mode-hook pyvenv-mode))

(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :init
  (add-to-list 'company-backends 'company-anaconda))

;; Language Server Protocol
(use-package lsp-mode
  :commands lsp
  :init
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  :config
  (require 'lsp-clients)
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

;; Javascript
(use-package js2-mode
  :mode ("\\.js" . js2-mode)
  :hook (js2-mode . lsp))

;; string manipulation
(use-package s)

;; Projectile
(use-package projectile
  :demand t
  :config
  (projectile-mode))

(use-package counsel-projectile
  :demand t
  :config
  (counsel-projectile-mode))

(use-package treemacs
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-evil
  :demand t)

(use-package treemacs-projectile
  :demand t)

(use-package treemacs-magit)

;; Science stuff
;; (use-package poly-markdown)

;; (use-package polymode
;;   :mode
;;   ("\\.Rnw" . poly-noweb+r-mode)
;;   ("\\.Rmd" . poly-markdown+r-mode))

;; (use-package ess
;;   :config
;;   (setq ess-eval-visibly 'nowait))

(use-package markdown-mode
  :mode
  ("\\.md" . markdown-mode)
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

;; evil stuff
(use-package evil-org
  :after org
  :demand t
  :config
  (setq evil-org-set-key-theme '(textobjects insert navigation))
  (add-hook 'org-mode-hook 'evil-org-mode))

(use-package evil-magit
  :after '(evil magit)
  :config
  (setq evil-magit-state 'normal))

(use-package flycheck
  :delight " Fly"
  :commands (projectile-switch-project)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (setq-default flycheck-disabled-checker 'javascript-jshint)
  (setq-default flycheck-disabled-checker 'json-jsonlist)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setq flycheck-flake8rc "~/.flake8"))

(use-package olivetti
  :config (setq-default olivetti-body-width 90))

(use-package smartparens
  :demand t
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "<" nil :actions nil)
  (add-to-list 'sp-sexp-suffix (list #'rust-mode 'regexp ";"))
  (electric-pair-mode 0)
  (require 'smartparens-config))

(use-package rainbow-mode)

;; org
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
          ("NB". "orange"))))

(use-package yaml-mode
  :mode
  ("\\.yml" . yaml-mode)
  ("\\.yaml" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'delete-trailing-whitespace))

(use-package yasnippet-snippets
  :after yasnippet)

;; Racket
(use-package racket-mode)

(provide 'packages)
;;; packages.el ends here
