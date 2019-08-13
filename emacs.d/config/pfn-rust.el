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
  :hook (rust-mode . (jojo/company-push-backend-local 'company-racer))
  :init
  (unless (getenv "RUST_SRC_PATH")
    (setenv "RUST_SRC_PATH" (expand-file-name "/Users/niels/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src"))))

(provide 'pfn-rust)
;;; pfn-rust.el ends here
