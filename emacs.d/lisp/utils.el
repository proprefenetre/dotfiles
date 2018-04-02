;;; utils.el --- Custom functions
;;; Commentary:
;;; Code:

(defun pfn/open-config-file ()
  "Open the config file at point."
  (interactive)
  (setq config-dir "~/.emacs.d/lisp"
        fname (concat (thing-at-point 'symbol) ".el"))
  (find-file (expand-file-name fname config-dir)))

(defun pfn/open-init-file ()
  "Edit init file."
  (interactive)
  (find-file user-init-file))

(defun pfn/reload-init ()
  "Reload init.el."
  (interactive)
  (load-file user-init-file))

(defun pfn/reload-current-file ()
  "Reload the file in this buffer."
  (interactive)
  (load-file buffer-file-name))

(defun pfn/ispell-toggle-dictionary ()
  "Toggle between the dutch and the english dictionaries."
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
  "Open and move to a new vertically split buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-buffer "*scratch*"))

(defun pfn/hsplit-new-buffer ()
  "Open and move to a new horizontally split buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-buffer "*scratch*"))

(defun pfn/cycle-themes ()
  "Cycle through available themes."
  (interactive)
  (let* ((themes [base16-gruvbox-dark-soft base16-gruvbox-light-medium
                                           base16-grayscale-dark base16-monokai])
         (idx-before (if (get 'pfn/cycle-themes 'state)
                         (get 'pfn/cycle-themes 'state) 0))
         (idx-after (% (+ idx-before (length themes) 1) (length themes)))
         (next (aref themes idx-after)))
    (put 'pfn/cycle-themes 'state idx-after)
    (load-theme next)))

(defun pfn/revert-buffer-no-confirm ()
  "Revert buffer without confirmation.
Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el"
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message "buffer reloaded"))

(defun pfn/backward-delete-word (arg)
  "Delete a word backwards, i.e. ctrl-w in terminals. Supply ARG to repeat."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(provide 'utils)
;;; utils.el ends here
