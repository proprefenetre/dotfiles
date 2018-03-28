(use-package paredit
  :ensure t)

(defun pfn/setup-prog-mode ()
  (auto-fill-mode)
  (rainbow-delimiters-mode)
  (display-line-numbers-mode)
  (delete-trailing-whitespace))

(defun pfn/setup-lisp-mode ()
  (paredit-mode)
  (aggressive-indent-mode))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode)
  :config
  (display-line-numbers-mode)
  (delete-trailing-whitespace))

(use-package rust-mode
  :ensure t)

(use-package python-mode
  :ensure t
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook 'pfn/setup-lisp-mode))

(add-hook 'ielm-mode-hook (lambda () (eldoc-mode 1)))


(add-hook 'prog-mode-hook 'pfn/setup-prog-mode)


(provide 'prog-settings)
