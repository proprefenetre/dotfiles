;;; utils.el --- Custom functions
;;; Commentary:
;;; Code:

(defun pfn-open-config-file ()
  "Open the config file at point."
  (interactive)
  (setq config-dir "~/.emacs.d/lisp"
        fname (concat (thing-at-point 'symbol) ".el"))
  (find-file (expand-file-name fname config-dir)))

(defun pfn-open-init-file ()
  "Edit init file."
  (interactive)
  (find-file user-init-file))

(defun pfn-reload-init ()
  "Reload init.el."
  (interactive)
  (load-file user-init-file))

(defun pfn-reload-current-file ()
  "Reload the file in this buffer."
  (interactive)
  (load-file buffer-file-name))

(defun pfn-ispell-toggle-dictionary ()
  "Toggle between the dutch and the english dictionaries."
  (interactive)
  (if (get 'pfn-ispell-toggle-dictionary 'state)
      (progn
        (ispell-change-dictionary "nederlands")
        (message "ispell dictionary set to 'nederlands'")
        (put 'pfn-ispell-toggle-dictionary 'state nil))
    (progn
      (ispell-change-dictionary "english")
      (message "ispell dictionary set to 'english'")
      (put 'pfn-ispell-toggle-dictionary 'state t))))

(defun pfn-vsplit-new-buffer ()
  "Open and move to a new vertically split buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-buffer "*scratch*"))

(defun pfn-hsplit-new-buffer ()
  "Open and move to a new horizontally split buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-buffer "*scratch*"))

(defun pfn-cycle-themes ()
  "Cycle through available themes."
  (interactive)
  (let* ((themes [base16-gruvbox-dark-soft base16-gruvbox-light-medium
                                           base16-grayscale-dark base16-monokai])
         (idx-before (if (get 'pfn-cycle-themes 'state)
                         (get 'pfn-cycle-themes 'state) 0))
         (idx-after (% (+ idx-before (length themes) 1) (length themes)))
         (next (aref themes idx-after)))
    (put 'pfn-cycle-themes 'state idx-after)
    (load-theme next)))

(defun pfn-revert-buffer-no-confirm ()
  "Revert buffer without confirmation.
Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el"
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message "buffer reloaded"))

(defun pfn-backward-delete-word (arg)
  "Delete a word backwards, i.e. ctrl-w in terminals. Supply ARG to repeat."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

;; Completion                                        ;
(defun org-keyword-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'org-keyword-backend))
    (prefix (and (eq major-mode 'org-mode)
                 (cons (company-grab-line "^#\\+\\(\\w*\\)" 1)
                       t)))
    (candidates (mapcar #'upcase
                        (cl-remove-if-not
                         (lambda (c) (string-prefix-p arg c))
                         (pcomplete-completions))))
    (ignore-case t)
    (duplicates t)))

(defun pfn-describe-keymap (keymap)
  "Describe a keymap using `substitute-command-keys'."
  (interactive
   (list (completing-read
          "Keymap: " (let (maps)
                       (mapatoms (lambda (sym)
                                   (and (boundp sym)
                                        (keymapp (symbol-value sym))
                                        (push sym maps))))
                       maps)
          nil t)))
  (with-output-to-temp-buffer (format "*keymap: %s*" keymap)
    (princ (format "%s\n\n" keymap))
    (princ (substitute-command-keys (format "\\{%s}" keymap)))
    (with-current-buffer standard-output ;; temp buffer
      (setq help-xref-stack-item (list #'my-describe-keymap keymap)))))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key [tab] 'tab-indent-or-complete)

(provide 'utils)

;;; utils.el ends here
