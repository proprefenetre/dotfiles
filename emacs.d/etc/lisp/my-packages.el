;;; packages.el -- emacs packages
;;; commentary:
;;; code:

;; Python
;; Language Server Protocol TODO: try eglot

;; (use-package lsp-mode
;;   :commands lsp
;;   :init
;;   (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
;;   :config
;;   (require 'lsp-clients)
;;   (setq lsp-auto-guess-root t
;;         lsp-prefer-flymake nil
;;         lsp-auto-configure t))

;; (use-package company-lsp
;;   :after company
;;   :commands company-lsp
;;   :init
;;   (add-to-list 'company-backends 'company-lsp)
;;   :config
;;   (setq company-lsp-async t
;;         company-lsp-cache-candidates 'auto
;;         company-lsp-enable-recompletion t))

;; Javascript
(use-package js2-mode
  :mode ("\\.js" . js2-mode)
  :hook (js2-mode . lsp))

;; Sciency stuff
;; (use-package poly-markdown)

;; (use-package polymode
;;   :mode
;;   ("\\.Rnw" . poly-noweb+r-mode)
;;   ("\\.Rmd" . poly-markdown+r-mode))

;; (use-package ess
;;   :config
;;   (setq ess-eval-visibly 'nowait))

(use-package markdown-mode
  :mode
  ("\\.md" . markdown-mode)
  ("\\.mdpp" . markdown-mode)
  :init
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode)
  :config
  (font-lock-add-keywords 'markdown-mode
                          '(("@[^ ]+"
                             . font-lock-variable-name-face)))
  (font-lock-add-keywords 'markdown-mode
                          '(("\\(@fig:\\|@tbl:\\)\\([^ ]+\\)"
                             (1 font-lock-keyword-face) (2 font-lock-variable-name-face))))
  (font-lock-add-keywords 'markdown-mode
                          '(("(b?red .*)" . font-lock-keyword-face)))
  )

(provide 'my-packages)
;;; my-packages.el ends here
