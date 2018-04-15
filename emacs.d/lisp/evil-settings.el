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
  (setq evil-emacs-state-cursor '("#906cff" box)
        evil-normal-state-cursor '("#91ddff" box)
        evil-motion-state-cursor '("#65b2ff" hollow)
        evil-operator-state-cursor '("#65b2ff" hollow)
        evil-visual-state-cursor '("#ffd75f" box)
        evil-insert-state-cursor '("#62d196" hbar)
        evil-replace-state-cursor '("#ff5458" hbar))
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
