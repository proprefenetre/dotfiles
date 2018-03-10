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
        '(("t" "todo" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?\n  %i\n" :empty-lines 1)
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a" :empty-lines 1)
          ("l" "Link" entry (file "~/org/links.org")
           "* READ %?\n" :empty-lines 1)
          ("e" "Emacs facts" entry (file "~/org/emacs.org")
           "* %? (%a)\n %i\n" :empty-lines 1))))
  
(setq org-default-notes-file "~/org/todo.org")
(setq org-directory "~/org")
(setf org-blank-before-new-entry '((heading . t) (plain-list-item . t)))

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
