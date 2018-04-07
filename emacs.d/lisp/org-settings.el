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
                           "~/org/agenda.org")
        org-archive-location "~/org/archief::"
        org-log-done nil
        org-log-into-drawer nil
        org-cycle-separator-lines 1
        outline-blank-line t            ; newlines are not content
        org-level-color-stars-only t
        org-return-follows-link t
        org-tags-column -80))

(defun pfn-org-level-sizes ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (interactive)
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :height 1.0)))

(defun pfn-org-level-colors ()
  "Taste the rainbow!"
  (interactive)
  (set-face-attribute 'org-level-1 nil :foreground "#ff5458")
  (set-face-attribute 'org-level-2 nil :foreground "#62d196")
  (set-face-attribute 'org-level-3 nil :foreground "#ffd75f")
  (set-face-attribute 'org-level-4 nil :foreground "#65b2ff")
  (set-face-attribute 'org-level-5 nil :foreground "#906cff")
  (set-face-attribute 'org-level-6 nil :foreground "#63f2f1"))

(eval-after-load "org" '(progn (pfn-org-level-colors)
                               (pfn-org-level-sizes)))
;; Ensure ELPA org is prioritized above built-in org.
(require 'cl)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

(setq org-todo-keyword-faces
      '(("TODO" . "#906cff")
        ("AFSPRAAK" . "#91ddff")
        ("BELLEN" . "#91ddff")
        ("INTAKE" . "#91ddff")
        ("CANCELED" . "#ff5458")
        ("READ" . "cadet blue")
        ("IDEE" . "cadet blue")))

(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)))

(setq org-capture-templates
      '(("w" "word" entry (file+headline "~/org/dict.org" "Words")
         "* %? :: ")
        ("W" "usage" entry (file+headline "~/org/dict.org" "Usage")
         "* %? :: ")
        ("t" "todo" entry (file+headline "~/org/todo.org" "To do")
         "* TODO %?")
        ("l" "link" entry (file+headline "~/org/todo.org" "To Read")
         "* READ [[%?][]]")
        ("n" "note" entry (file+headline "~/org/todo.org" "NB")
         "* %?")))

(provide 'org-settings)
;;; org-settings.el ends here
