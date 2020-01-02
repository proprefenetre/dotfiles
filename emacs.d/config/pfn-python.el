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

(use-package eglot
  :hook (python-mode . eglot-ensure)
  :config
  (setq eglot-put-doc-in-help-buffer t
        eglot-auto-display-help-buffer nil
        eglot-ignored-server-capabilities :documentHighlightProvider))

(use-package python
  :ensure nil
  :mode ("\\.py" . python-mode)
  :init
  ;; (setq flycheck-python-flake8-executable "/usr/bin/flake8"
  ;;       flycheck-flake8rc "~/.config/flake8")
  (setq python-shell-interpreter "/usr/bin/ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  (setq python-indent-offset 4))

(use-package blacken)

(provide 'pfn-python)
;;; pfn-python.el ends here
