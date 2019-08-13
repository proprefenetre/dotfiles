;;; pfn-python.el --- python setup
;;; Commentary:
;;; Code:
(use-package python
  :ensure nil
  :mode ("\\.py" . python-mode)
  :init
  (setq flycheck-python-flake8-executable "/usr/local/bin/flake8"
        flycheck-flake8rc "~/.config/flake8")
  (setq python-shell-interpreter "/usr/local/bin/ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  (setq python-indent-offset 4))

(use-package anaconda-mode
  :commands (anaconda-eldoc-mode)
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))


(use-package company-anaconda
  :commands (company-anaconda)
  :hook (python-mode . (lambda ()
                         (jojo/company-push-backend-local '(company-anaconda :wth company-capf)))))

(provide 'pfn-python)
;;; pfn-python.el ends here
