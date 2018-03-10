; custom functions

(defun pfn/open-config-file ()
  "Open the config file at point in another window"
  (interactive)
  (setq config-dir (expand-file-name (file-name-as-directory "configs") user-emacs-directory))
  (setq fname (concat (thing-at-point 'symbol) ".el"))
  (find-file-other-window (expand-file-name fname config-dir)))

(defun pfn/open-init-file ()
  "edit init file"
  (interactive)
  (find-file user-init-file))

(provide 'utils)
