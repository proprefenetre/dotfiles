;;; evil-settings.el -- evil config
;;; Commentary:
;;; Code:

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration nil
        evil-want-Y-yank-to-eol t)
  ;; (setq evil-disable-insert-state-bindings t)
  :config
  (setq evil-search-wrap t
        evil-regexp-search t)

  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :demand t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :demand t
  :config
  (global-evil-surround-mode))

(use-package evil-embrace
  :ensure t
  :after evil-surround
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-commentary
  :ensure t
  :demand t
  :config
  (evil-commentary-mode))

(provide 'evil-settings)
