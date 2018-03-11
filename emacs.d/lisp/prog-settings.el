; modes etc

;; prog-mode hooks
(add-hook 'prog-mode-hook #'turn-on-auto-fill) 
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'linum-mode t)

(use-package flycheck
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package python-mode
  :ensure t)

(provide 'prog-settings)
