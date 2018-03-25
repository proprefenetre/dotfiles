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
(use-package racket-mode)


;; :config
;; (font-lock-add-keywords 'racket-mode
;; '(("define-\\w+" . font-lock-keyword-face))))

(use-package paredit
  :ensure t)

(add-hook 'ielm-mode-hook (lambda () (eldoc-mode 1)))

(defun pfn/prog-mode-hooks ()
  (auto-fill-mode)
  (rainbow-delimiters-mode)
  (paredit-mode)
  (display-line-numbers-mode)
  (delete-trailing-whitespace)
  (aggressive-indent-mode))

(add-hook 'prog-mode-hook 'pfn/prog-mode-hooks)
;; (set-face-attribute 'line-number nil :background 'unspecified)

(provide 'prog-settings)
