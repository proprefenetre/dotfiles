;;; pfn-python.el --- python setup
;;; Commentary:
;;; Code:

(defun embrace-python-mode-hook ()
  "Embrace-add-pair for Python-mode."
  (dolist (lst '((?~ "\"\"\"" . "\"\"\"")
                 (?p "Path(" . ")")
                 (?f "{" . ":}")))
    (embrace-add-pair (car lst) (cadr lst) (cddr lst))))

(defun pfn-auto-activate-venv ()
  (interactive)
  (f-traverse-upwards
   (lambda (path)
     (let ((v-path (f-expand ".venv" path)))
       (if (f-exists? v-path)
           (progn
             (message "Found .venv at %s" v-path)
             (pyvenv-activate v-path)
             t)
         nil)))
   default-directory))

(use-package python-mode
  :ensure nil
  :mode "\\.py\\'"
  ;; :company '(company-yasnippet company-files company-dabbrev-code)
  ;; :capf eglot-completion-at-point
  :config
  (setq python-shell-interpreter "/usr/bin/ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  (setq python-indent-offset 4))

(use-package pyvenv
  :ensure t
  :hook ((python-mode . pyvenv-mode)))

(provide 'pfn-python)
;;; pfn-python.el ends here
