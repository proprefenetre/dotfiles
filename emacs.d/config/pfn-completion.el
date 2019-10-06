;;; pfn-completion.el --- setup company-mode
;;; Commentary:
;;; Code:

(use-package company
  :demand t
  :config
  (setq company-idle-delay 0
        company-echo-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-require-match 'never)
  (setq company-backends
        '(company-files
          company-capf
          company-keywords
          company-dict
          (company-abbrev company-dabbrev company-dabbrev-code)))
  (global-company-mode))

  (use-package company-prescient
    :after prescient
    :demand t
    :config
    (company-prescient-mode 1))

(use-package company-dict
  :after company
  :demand t
  :config
  (setq company-dict-dir (concat user-emacs-directory "dict/")))

;; (use-package compdef
;;   :after company
;;   :demand t
;; (compdef
;;  :modes '(emacs-lisp-mode lisp-interaction-mode ielm-mode)
;;  :company '(company-capf company-yasnippet company-files company-keywords company-dict
;;                          (company-abbrev company-dabbrev company-dabbrev-code)))
;; )

(provide 'pfn-completion)
;;; pfn-completion ends here
