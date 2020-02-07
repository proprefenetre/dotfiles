;;; pfn-python.el --- python setup
;;; Commentary:
;;; Code:

(defun embrace-python-mode-hook ()
  "Embrace-add-pair for Python-mode."
  (dolist (lst '((?~ "\"\"\"" . "\"\"\"")
                 (?p "Path(" . ")")
                 (?f "{" . ":}")))
    (embrace-add-pair (car lst) (cadr lst) (cddr lst))))


(use-package python-mode
  :ensure nil
  :mode "\\.py\\'"
  :company '(company-capf company-yasnippet company-files company-dabbrev-code)
  :capf eglot-completion-at-point
  :config
  (setq python-shell-interpreter "/usr/bin/ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  (setq python-indent-offset 4))

(defun pfn-auto-activate-venv ()
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

(use-package pyvenv
  :ensure t
  :hook ((python-mode . pyvenv-mode)
         (python-mode . pfn-auto-activate-venv)))

(provide 'pfn-python)
;;; pfn-python.el ends here
