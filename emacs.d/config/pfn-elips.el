;;; pfn-elips.el --- emacs-lisp-mode config
;;; Commentary: 
;;; Code:

(use-package emacs-lisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . (lambda ()
                             (pfn-add-company-backend-local '(company-capf :with company-yasnippet)))))

(provide 'pfn-elips)
;;; pfn-elips.el ends here
