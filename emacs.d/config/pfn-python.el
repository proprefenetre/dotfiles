;;; pfn-python.el --- python setup
;;; Commentary:
;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py" . python-mode)
  :init

  (setq flycheck-python-flake8-executable (pfn-make-bin-path "flake8")
        flycheck-flake8rc "~/.config/flake8")
  (setq python-shell-interpreter (pfn-make-bin-path "ipython")
        python-shell-interpreter-args "--simple-prompt -i")
  (setq python-indent-offset 4))

(use-package anaconda-mode
  :commands (anaconda-eldoc-mode)
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda
  :hook (python-mode . (lambda ()
                         (pfn-add-company-backend-local
                          '(company-anaconda :with company-yasnippet)))))

(provide 'pfn-python)
;;; pfn-python.el ends here
