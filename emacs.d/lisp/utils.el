; custom functions

(defun pfn/open-config-file ()
  "Open the config file at point"
  (interactive)
  (setq config-dir (expand-file-name (file-name-as-directory "lisp") user-emacs-directory))
  (setq fname (concat (thing-at-point 'symbol) ".el"))
  (find-file (expand-file-name fname config-dir)))

(defun pfn/open-init-file ()
  "edit init file"
  (interactive)
  (find-file user-init-file))

(defun pfn/ispell-toggle-dictionary ()
  "toggle between the dutch and the english dictionaries"
  (interactive)
  (if (get 'pfn/ispell-toggle-dictionary 'state)
      (progn
        (ispell-change-dictionary "nederlands")
        (message "ispell dictionary set to 'nederlands'")
        (put 'pfn/ispell-toggle-dictionary 'state nil))
    (progn
      (ispell-change-dictionary "english")
      (message "ispell dictionary set to 'english'")
      (put 'pfn/ispell-toggle-dictionary 'state t))))

; custom keys

(global-set-key (kbd "C-c o") 'pfn/open-config-file)
(global-set-key (kbd "C-c s") 'pfn/ispell-dictionary-dutch)
(global-set-key (kbd "C-c g") 'magit-status)

(provide 'utils)
