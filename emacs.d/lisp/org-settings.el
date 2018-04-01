;;; org-settings.el --- settings for org
;;; Commentary:
;;; Code:

(use-package org
  :ensure org-plus-contrib
  :pin org
  :defer t
  :commands (org-capture)
  :config
  (setq org-directory "~/org"
        org-default-notes-file "~/org/algemeen.org"
        org-agenda-files '("~/org/algemeen.org"
                           "~/org/thesis.org")
        org-archive-location "~/org/archief.org::"
        org-log-done nil
        org-log-into-drawer t
        org-cycle-separator-lines 1
        outline-blank-line t            ; newlines are not content
        org-level-color-stars-only t
        org-clock-persist 'history
        org-return-follows-link 1)
  (org-clock-persistence-insinuate))


;; Ensure ELPA org is prioritized above built-in org.
(require 'cl)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

(setq org-todo-keyword-faces
      '(("bezig" . "orange")
        ("afspraak" . "steel blue")
        ("read" . "cadet blue")
        ("idee" . "cadet blue")))

(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)))

(setq org-capture-templates
      '(("w" "word" entry (file+headline "~/org/dict.org" "Words")
         "* %? :: ")
        ("w" "usage" entry (file+headline "~/org/dict.org" "Usage")
         "* %? :: ")
        ("t" "todo" entry (file+headline "~/org/algemeen.org" "To do")
         "* todo %?")
        ("e" "emacs" entry (file+headline "~/org/algemeen.org" "Emacs")
         "* todo %?")
        ("s" "scriptie" entry (file+headline "~/org/thesis.org" "Algemeen")
         "* todo %?")
        ("l" "link" entry (file+headline "~/org/algemeen.org" "To Read")
         "* read [[%?][]]")
        ("n" "Note" entry (file+headline "~/org/algemeen.org" "NB")
         "* %?")))

(defun pfn/org-header-settings ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (interactive)
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :height 1.0)))

(add-hook 'org-mode-hook 'pfn/org-header-settings)

(use-package toc-org
  :after org
  :config
  (add-hook 'org-mode-hook 'toc-org-enable))

(provide 'org-settings)
;;; org-settings.el ends here
