; custom functions

(defun pfn/open-config-file ()
  "Open the config file at point"
  (interactive)
  (setq config-dir "~/.emacs.d/lisp"
        fname (concat (thing-at-point 'symbol) ".el"))
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

(defun pfn/vsplit-new-buffer ()
  "open and move to a new vertically split buffer"
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)
    (funcall initial-major-mode)
    buffer))

(defun pfn/hsplit-new-buffer ()
  "open and move to a new horizontally split buffer"
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)
    (funcall initial-major-mode)
    buffer))

; custom keys

(global-set-key (kbd "C-c o") 'pfn/open-config-file)
(global-set-key (kbd "C-c s") 'pfn/ispell-toggle-dictionary)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-x 2") 'pfn/vsplit-new-buffer)
(global-set-key (kbd "C-x 3") 'pfn/hsplit-new-buffer)

(provide 'utils)
