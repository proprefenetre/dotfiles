;;; pfn-completion.el --- Eglot & company configuration
;;; Commentary:
;;; Code:
(defun pfn-eglot-python-setup () 
  (interactive)
  (setq eglot-workspace-configuration
        '((pyls . ((configurationSources . ["flake8"])
                   (plugins . ((jedi_completion . ((enabled . t)))
                               (jedi_definition . ((enabled . t)
                                                   (follow_imports . t)
                                                   (follow_builtin_imports . t)))
                               (jedi_hover . ((enabled . t)))
                               (jedi_signature_help . ((enabled . t)))
                               (jedi_symbols . ((enabled . t)
                                                (all_scopes . t)))
                               (mypy . ((enabled . t)))
                               (pylint . ((enabled . nil)))
                               (pycodestyle . ((enabled . nil)))
                               (pydocstyle . ((enabled . nil)))
                               (pyflakes . ((enabled . nil)))
                               ))))))

  (eglot-signal-didChangeConfiguration (eglot--current-server-or-lose)))

(use-package eglot
  :demand t
  :commands (eglot-ensure eglot-server)
  :hook (python-mode . eglot-ensure)
  :config
  (setq eglot-workspace-configuration
        '((pyls . ((configurationSources . ["flake8"])
                   (plugins . ((jedi_completion . ((enabled . t)))
                               (jedi_definition . ((enabled . t)
                                                   (follow_imports . t)
                                                   (follow_builtin_imports . t)))
                               (jedi_hover . ((enabled . t)))
                               (jedi_signature_help . ((enabled . t)))
                               (jedi_symbols . ((enabled . t)
                                                (all_scopes . t)))
                               (mypy . ((enabled . t)))
                               (pylint . ((enabled . nil)))
                               (pycodestyle . ((enabled . nil)))
                               (pydocstyle . ((enabled . nil)))
                               (pyflakes . ((enabled . nil)))
                               ))))))

  (setq eglot-put-doc-in-help-buffer t
        eglot-auto-display-help-buffer nil
        eglot-ignored-server-capabilities :documentHighlightProvider))

(use-package company
  :demand t
  :config
  (setq company-idle-delay 0
        company-echo-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-require-match 'never)
  (setq company-backends
        '(company-capf
          company-files
          company-yasnippet
          company-dabbrev-code
          (company-abbrev company-dabbrev)))
  (global-company-mode 1))

(use-package company-prescient
:after prescient
:demand t
:config
(company-prescient-mode 1))

(use-package compdef
  :after company
  :demand t
  :config

  (compdef
   :modes 'org-mode
   :company '(company-dabbrev company-capf)
   :capf 'pcomplete-completions-at-point))


(provide 'pfn-completion)
;;; pfn-completion ends here
