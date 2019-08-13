;;; pfn-completion.el --- setup company-mode
;;; Commentary:
;;; code borrowed from https://github.com/jojojames/.dotfiles/blob/master/emacs/.emacs.d/config/jojo-autocomplete.el
;;; Code:

(use-package company
  :demand t
  :init
  (defun jojo/company-backend-in-backends (b)
    "Check if backend b is already in company-backends.
We need to do this check because each backend has additional symbols attached.
Ex. company-clang :with company-yasnippet."
    (let ((in-backend nil))
      (dolist (backend company-backends)
        (when (member b backend)
          (setq in-backend t)))
      in-backend))

  (defun jojo/company-push-backend (b &optional no-merge)
    "Adds backend b to company mode if it's not already in the list of backends.
If `no-merge' is non-nil, don't merge additional backends."
    (unless (jojo/company-backend-in-backends b)
      (add-to-list 'company-backends b))
    (unless no-merge
      (jojo/company-merge-backends)))

  (defun jojo/company-push-backend-local (b &optional no-merge)
    "Push backend into local backends.
If `no-merge' is non-nil, don't merge additional backends."
    (make-local-variable 'company-backends)
    (jojo/company-push-backend b no-merge))

  :config
  (defun merge-backend-with-company-backends (backend-to-merge)
    "Merges a backend with every backend in company-backends.
The backend will only be merged if it's not already being used in the current backend.
We do this because so that the backend that we're merging
will always be part of the completion candidates.
For example, merging company-yasnippet to company-capf
will yield (company-capf :with company-yasnippet)."

    ;; create a list of backend-to-merge with a count equal to company-backends
    ;; this is so mapcar* can iterate over both lists equally
    ;; ex. if we have (company-capf company-xcode),
    ;; then the list is (company-yasnippet company-yasnippet)
    (setq blist (make-list (cl-list-length company-backends) backend-to-merge))
    ;; b will be backend-to-merge
    ;; backend will be a backend from company-backends
    (setq company-backends (cl-mapcar (lambda (backend b)
                                        (if (and (listp backend) (member b backend))
                                            backend
                                          (append (if (consp backend)
                                                      backend
                                                    (list backend))
                                                  (if (and (listp backend)
                                                           (member :with backend))
                                                      `(,b)
                                                    `(:with ,b)))))
                                      company-backends blist)))

  (defun jojo/company-merge-backends ()
    "Merge common backends."
    (merge-backend-with-company-backends 'company-yasnippet))

  (jojo/company-merge-backends)
  (setq company-idle-delay 0
        company-echo-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-require-match 'never)
  (setq company-backends
        '(company-capf company-files
                       (company-dabbrev company-dabbrev-code)))

  (global-company-mode))

(use-package company-prescient
  :after prescient
  :demand t
  :config
  (company-prescient-mode 1))

(use-package company-dict
  :after company
  :config
  (setq company-dict-dir (concat user-emacs-directory "dict/"))
  (add-to-list 'company-backends 'company-dict)
  )

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
