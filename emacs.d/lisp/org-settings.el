(use-package org
  :ensure org-plus-contrib
  :pin org
  :defer t
  :commands (org-capture)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda-list)
         ("C-c l" . org-store-link)
         ("C-c w" . org-refile))
  :config
  (setq org-default-notes-file "~/org/algemeen.org"
        org-directory "~/org"
        org-log-done nil
        org-log-into-drawer t
        org-cycle-separator-lines 1
        org-level-color-stars-only t
        org-clock-persist 'history
        org-return-follows-link 1)
  (org-clock-persistence-insinuate))

;; Ensure ELPA org is prioritized above built-in org.
(require 'cl)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

(setq org-todo-keyword-faces
      '(("bezig" . "orange")))

(setq org-agenda-files
      '("~/org/algemeen.org"
        "~/org/thesis.org"
        "~/org/werk.org"))

(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)))

(setq org-capture-templates
      '(("t" "taak" entry (file+headline "~/org/algemeen.org" "To do")
         "* TODO %?\n")
        ("s" "scriptie" entry (file+headline "~/org/thesis.org" "Algemeen")
         "* TODO %?\n")
        ("w" "werk" entry (file+headline "~/org/werk.org" "Todo")
         "* TODO %?\n %^t")
        ("a" "afspraak" entry (file+headline "~/org/werk.org" "Afspraken")
         "* AFSPRAAK %?\n\t%^T")
        ("l" "Link" entry (file+headline "~/org/notes.org" "To Read")
         "* READ %? %U" :empty-lines 1)
        ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
         "* %?" :empty-lines 1)
        ("e" "Emacs Facts and Functions" entry (file "~/org/emacs.org")
         "** %? (%a)" :empty-lines 1)))

(defun pfn/org-header-settings ()
  "Stop the org-level headers from increasing in height relative
to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :height 1.0)))

(add-hook 'org-mode-hook 'pfn/org-header-settings)

(use-package toc-org
  :after org
  :init (add-hook 'org-mode-hook #'toc-org-enable))

(provide 'org-settings)
