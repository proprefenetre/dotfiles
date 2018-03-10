;; orgmode settings

(use-package org
  :ensure t
  :defer t
  :commands (org-capture)
  :bind (("C-c c" . org-capture)
         ("C-c t A" . org-agenda))
  :config
  (setq org-todo-keywords
        '((sequence "TODO" "WAITING" "|" "DONE" "CANCELED")
          (sequence "READ" "|" "DONE")))
  (setq org-capture-templates
        '(("t" "taak" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?\n  %i") 
          ("s" "scriptie" entry (file+headline "~/org/todo.org" "Scriptie")
           "* TODO %?\n %^t")
          ("w" "scriptie" entry (file+headline "~/org/todo.org" "Werk")
           "* TODO %?\n %^t")
          ("l" "Link" entry (file+headline "~/org/links.org" "To Read")
           "* READ %? %U"
           :empty-lines 1)
          ("e" "Emacs Facts and Functions" entry (file "~/org/emacs.org")
           "** %? (%a)"
           :empty-lines 1))))
  
(setq org-default-notes-file "~/org/todo.org")
(setq org-directory "~/org")
(setf org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))

(defun pfn/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative
to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :height 1.0)))

(add-hook 'org-mode-hook 'pfn/org-mode-hook)

(provide 'org-settings)
