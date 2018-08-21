;;; custom-functions.el -- transform word or region
;;; commentary:
;;; code:

(require 's)

(defun pfn-eyebrowse-open-init ()
  (interactive)
  (eyebrowse-create-window-config)
  (find-file user-init-file)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) "init.el"))

(defun pfn-eyebrowse-open-package ()
  (interactive)
  (eyebrowse-create-window-config)
  (package-list-packages)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) "package"))

(defun pfn-transform-word-or-region (fmt)
  "Transform word or words in a region according to FMT."
  (interactive "sFormat string: ")
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (mapconcat (function (lambda (x) (format fmt x)))
                         (split-string text) " ")))))

(defun pfn-cycle-themes ()
  "Cycle through available themes."
  (interactive)
  (let* ((themes [challenger-deep nord])
         (idx-before (if (get 'pfn-cycle-themes 'state)
                         (get 'pfn-cycle-themes 'state) 0))
         (idx-after (% (+ idx-before (length themes) 1) (length themes)))
         (prev (aref themes idx-before))
         (next (aref themes idx-after)))
    (put 'pfn-cycle-themes 'state idx-after)
    (disable-theme prev)
    (load-theme next t)
    (setq sml/theme 'respectful)
    (set-face-attribute 'line-number nil :background 'unspecified)
    (set-face-attribute 'fringe nil :inherit 'line-number)))

(defun pfn-edit-indirect-latex (beg end)
  "Edit Latex-snippets in an indirect buffer."
  (interactive "r")
  (switch-to-buffer (edit-indirect-region beg end))
  (LaTeX-mode))

(defun pfn-comment-region (beg end)
  "Comment out the active region."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert (format "%s%s%s\n" comment-start (s-trim text) comment-end))))

(defun pfn-find-file-region (beg end)
  "Open the selected file."
  (interactive "r")
  (find-file (buffer-substring-no-properties beg end)))

(provide 'custom-functions)
;;; custom-functions.el ends here
