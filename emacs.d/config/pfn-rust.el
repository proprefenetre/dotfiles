;;; pfn-rust.el --- rust setup
;;; Commentary:
;;; Code:
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package company-racer
  :hook (rust-mode . (lambda () (pfn-add-company-backend-local '(company-racer company-yasnippet)))))

(provide 'pfn-rust)
;;; pfn-rust.el ends here
