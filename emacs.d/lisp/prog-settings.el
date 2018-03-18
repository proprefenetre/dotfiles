(use-package flycheck
  :ensure t)

(use-package yaml-mode
  :ensure t
  :defer t
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode))

(use-package rust-mode
  :ensure t)

(use-package python-mode
  :ensure t
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

(use-package elpy
  :ensure t
  :init
  (with-eval-after-load 'python (elpy-enable))
  :config
  (delete 'elpy-module-highlight-indentation elpy-modules))

(use-package racket-mode
  :ensure t)

(use-package paredit
  :ensure t)

(add-hook 'ielm-mode-hook (lambda () (eldoc-mode 1)))

(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'linum-mode t)
(add-hook 'prog-mode-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'paredit-mode)

(provide 'prog-settings)
