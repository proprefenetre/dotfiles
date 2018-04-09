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
        org-agenda-files '("~/org/todo.org")
        org-archive-location "~/org/archief::"
        org-log-done nil
        org-log-into-drawer nil
        org-cycle-separator-lines 1
        outline-blank-line t            ; newlines are not content
        org-level-color-stars-only t
        org-return-follows-link t
        org-tags-column -80
        org-reverse-note-order t))      ; enter new note as first item

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
  (set-face-attribute 'org-level-1 nil :foreground "#87ffff")
  (set-face-attribute 'org-level-2 nil :foreground "#87d7ff")
  (set-face-attribute 'org-level-3 nil :foreground "#5fffaf")
  (set-face-attribute 'org-level-4 nil :foreground "#87ffff")
  (set-face-attribute 'org-level-5 nil :foreground "#87d7ff")
  (set-face-attribute 'org-level-6 nil :foreground "#5fffaf"))

(eval-after-load "org" '(progn (pfn-org-level-colors)
                               (pfn-org-level-sizes)))

;; Ensure ELPA org is prioritized above built-in org.
(require 'cl)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

(setq org-todo-keyword-faces
      '(("TODO" . "#c991e1")
        ("AFSPRAAK" . "#aaffe4")
        ("BELLEN" . "#aaffe4")
        ("INTAKE" . "#aaffe4")
        ("CANCELED" . "#ff5458")
        ("READ" . "#65b2ff")
        ("IDEE" . "#65b2ff")))

(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)))

(setq org-capture-templates
      '(("w" "word" entry (file+headline "~/org/dict.org" "Words") "* %? :: ")
        ("W" "usage" entry (file+headline "~/org/dict.org" "Usage") "* %? :: ")
        ("t" "todo" entry (file+headline "~/org/todo.org" "To Do") "* TODO %?")
        ("l" "link" entry (file+headline "~/org/todo.org" "To Do") "* READ [[%?][]]")
        ("n" "note" entry (file+headline "~/org/todo.org" "Notes") "* %?")))

(provide 'org-settings)
;;; org-settings.el ends here
