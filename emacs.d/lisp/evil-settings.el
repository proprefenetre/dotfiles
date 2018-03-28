;;; evil-settings.el -- evil config
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration nil)
  :config
  (setq evil-disable-insert-state-bindings t) ; use emacs bindings in insert mode
  (setq evil-search-wrap t
        evil-regexp-search t)

  ;; Use Emacs state in these additional modes.
  ;; (dolist (mode '(dired-mode
  ;;                 docview-mode
  ;;                 eshell-mode
  ;;                 term-mode))
  ;;   (add-to-list 'evil-emacs-state-modes mode))

  (evil-mode))

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

(use-package evil-commentary
  :ensure t
  :demand t
  :config
  (evil-commentary-mode))

(provide 'evil-settings)
