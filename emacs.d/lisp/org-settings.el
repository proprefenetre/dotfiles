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
        org-default-notes-file "~/org/todo.org"
        org-agenda-files '("~/org/todo.org"
                           "~/org/agenda.org"
                           "~/org/thesis.org")
        org-archive-location "~/org/archief.org::"
        org-log-done nil
        org-log-into-drawer t
        org-cycle-separator-lines 1
        outline-blank-line t            ; newlines are not content
        org-level-color-stars-only t
        org-return-follows-link t
        org-tags-column -80))

;; Ensure ELPA org is prioritized above built-in org.
(require 'cl)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

(setq org-todo-keyword-faces
      '(("BEZIG" . "orange")
        ("AFSPRAAK" . "steel blue")
        ("READ" . "cadet blue")
        ("IDEE" . "cadet blue")))

(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)))

(setq org-capture-templates
      '(("w" "word" entry (file+headline "~/org/dict.org" "Words")
         "* %? :: ")
        ("w" "usage" entry (file+headline "~/org/dict.org" "Usage")
         "* %? :: ")
        ("t" "todo" entry (file+headline "~/org/todo.org" "To do")
         "* TODO %?")
        ("e" "emacs" entry (file+headline "~/org/notes.org" "Emacs")
         "* TODO %?")
        ("s" "scriptie" entry (file+headline "~/org/thesis.org" "Algemeen")
         "* TODO %?")
        ("l" "link" entry (file+headline "~/org/todo.org" "To Read")
         "* READ [[%?][]]")
        ("n" "note" entry (file+headline "~/org/todo.org" "NB")
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

(provide 'org-settings)
;;; org-settings.el ends here
