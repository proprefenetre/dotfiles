;;; pfn-stats.el --- ess &c
;;; commentary:
;;; code:

(use-package ess)

(use-package poly-markdown)
(use-package poly-R)

(use-package polymode
  :init
  (require 'poly-markdown)
  (require 'poly-R)
  :config
  (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode)))

(provide 'pfn-stats)
;;; pfn-functions.el ends here
