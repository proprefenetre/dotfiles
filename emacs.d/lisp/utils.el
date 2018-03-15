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

(defun pfn/reload-init ()
  "reload init.el"
  (interactive)
  (load-file user-init-file))

(defun pfn/reload-current-file ()
  "reload the file in this buffer"
  (interactive)
  (load-file buffer-file-name))

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

(defun pfn/cycle-themes ()
  "Cycle through available themes."
  (interactive)
  (let* ((themes [base16-gruvbox-dark-soft base16-gruvbox-light-medium base16-grayscale-dark])
         (idx-before (if (get 'pfn/cycle-themes 'state)
                         (get 'pfn/cycle-themes 'state) 0))
         (idx-after (% (+ idx-before (length themes) 1) (length themes)))
         (next (aref themes idx-after)))
    (put 'pfn/cycle-themes 'state idx-after)
    (load-theme next)))

(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation. 
Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el"
    (interactive)
    (revert-buffer :ignore-auto :noconfirm)
    (message "buffer reloaded"))

; custom keys

(global-set-key (kbd "C-c o") 'pfn/open-config-file)
(global-set-key (kbd "C-c s") 'pfn/ispell-toggle-dictionary)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-x 2") 'pfn/vsplit-new-buffer)
(global-set-key (kbd "C-x 3") 'pfn/hsplit-new-buffer)
(global-set-key (kbd "C-c R") 'pfn/reload-init)
(global-set-key (kbd "C-c t") 'pfn/cycle-themes)
(global-set-key (kbd "C-c r") 'revert-buffer-no-confirm)

(provide 'utils)
