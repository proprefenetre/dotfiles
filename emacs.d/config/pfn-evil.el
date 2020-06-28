;;; pfn-evil.el --- evil setup
;;; Commentary:
;;; Code:
(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-want-C-w-delete t)
  :config
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "W" 'evil-write))
  (setq evil-insert-state-modes nil
        evil-motion-state-modes nil
        evil-search-wrap t
        evil-regexp-search t
        evil-complete-next-func 'hippie-expand
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-cross-lines t
        evil-ex-substitute-global t)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :demand t
  :init
  (setq evil-collection-outline-bind-tab-p nil
        evil-collection-setup-minibuffer t)
  :config
  (setq evil-collection-mode-list (delete 'company evil-collection-mode-list))
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :demand t
  :config (evil-commentary-mode))

(use-package evil-surround
  :after evil
  :demand t
  :config
  (global-evil-surround-mode))

;; (use-package evil-embrace
;;   :after evil
;;   :demand t
;;   :hook ((python-mode . (lambda () (embrace-add-pair ?a "\"\"\"" "\"\"\"" )))
;;          (org-mode (lambda () (embrace-add-pair ?_ "_" "_")))
;;          (org-mode (lambda () (embrace-add-pair ?* "*" "*")))
;;          (org-mode (lambda () (embrace-add-pair ?a "**" "**"))))
;;   (evil-embrace-enable-evil-surround-integration))

(use-package evil-org
  :after org
  :demand t
  :hook (org-mode . evil-org-mode)
  :config
  (setq evil-org-set-key-theme '(textobjects insert navigation)))

(use-package evil-magit
  :demand t
  :config
  (setq evil-magit-state 'normal))


(provide 'pfn-evil)
;;; pfn-evil.el ends here
