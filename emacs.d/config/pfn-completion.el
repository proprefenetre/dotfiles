;;; pfn-completion.el --- setup company-mode
;;; Commentary:
;;; code borrowed from https://github.com/jojojames/.dotfiles/blob/master/emacs/.emacs.d/config/jojo-autocomplete.el
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

(defun org-keyword-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'org-keyword-backend))
    (prefix (and (eq major-mode 'org-mode)
                 (cons (company-grab-line "^#\\+\\(\\w*\\)" 1)
                       t)))
    (candidates (mapcar 'downcase
                        (cl-remove-if-not
                         (lambda (c) (string-prefix-p arg c))
                         (pcomplete-completions))))
    (ignore-case t)
    (duplicates t)))

(provide 'pfn-completion)
;;; pfn-completion ends here
