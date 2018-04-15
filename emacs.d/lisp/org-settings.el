;;; org-settings.el --- settings for org
;;; Commentary:
;;; Code:

;; Ensure ELPA org is prioritized above built-in org.
(require 'cl)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

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

(provide 'org-settings)
;;; org-settings.el ends here
