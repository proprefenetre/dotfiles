;;; pfn-python.el --- python setup
;;; Commentary:
;;; Code:

;; (use-package anaconda-mode
;;   :commands (anaconda-eldoc-mode)
;;   :hook ((python-mode . anaconda-mode)
;;          (python-mode . anaconda-eldoc-mode)))

;; (use-package company-anaconda
;;   :hook (python-mode . (lambda ()
;;                          (pfn-add-company-backend-local
;;                           'company-anaconda))))

(use-package python-mode
  :ensure nil
  :mode "\\.py\\'"
  :config
  ;; eglot uses flymake
  ;; (setq flycheck-python-flake8-executable "/usr/bin/flake8"
  ;;       flycheck-flake8rc "~/.config/flake8")
  (setq python-flymake-command '("flake8" "-"))
  (setq python-shell-interpreter "/usr/bin/ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  (setq python-indent-offset 4)

  (eglot-workspace-configuration
   .
   ((pyls.configurationSources . ["flake8"] )))
  (embrace-add-pair ?a "\"\"\"" "\"\"\"" ))

(use-package pyvenv)

(use-package blacken)

(provide 'pfn-python)
;;; pfn-python.el ends here
