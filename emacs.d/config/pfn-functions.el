;;; functions.el --- my own lips
;;; commentary:
;;; code:
(use-package s)

(require 's)
(require 'ring)

(let ((langs '("dutch" "english")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun pfn-cycle-ispell-languages ()
  "Default documentation."
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(defun pfn-eyebrowse-open-init ()
  "Default documentation."
  (interactive)
  (eyebrowse-create-window-config)
  (find-file user-init-file)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) "init.el"))

(defun pfn-eyebrowse-open-package ()
  "Default documentation."
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

(defun pfn-edit-indirect-latex (beg end)
  "Edit Latex in an indirect buffer.  BEG END."
  (interactive "r")
  (switch-to-buffer (edit-indirect-region beg end))
  (LaTeX-mode))

(defun pfn-comment-region (beg end)
  "Comment out the active region.  BEG END."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert (format "%s%s%s\n" comment-start (s-trim text) comment-end))))

(defun pfn-find-file-region (beg end)
  "Open the selected file.  BEG END."
  (interactive "r")
  (find-file (buffer-substring-no-properties beg end)))

(defun pfn-unique-words-region (beg end)
  "Collect all of the unique words in the current region.  BEG END."
  (interactive "r")
  (let ((txt (delete-dups (mapcar #'downcase
                                  (split-string (buffer-substring-no-properties beg end)
                                                nil nil
                                                "[^[:alnum:]]+")))))
    (delete-region beg end)
    (cl-dolist (word (sort txt #'string<) txt)
      (insert (concat word "\n")))))

(defun pfn-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun pfn-get-face (pos)
  "Get the font faces at POS.
Source: https://gist.github.com/Wilfred/f7d61b7cdf9fdbb1d11c."
  (interactive "d")
  (remq nil
        (list
         (get-char-property pos 'read-face-name)
         (get-char-property pos 'face)
         (message "%s" (plist-get (text-properties-at pos) 'face)))))

(defun pfn-add-company-backend-local (backend)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends backend))

(defun pfn-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     (get-file-buffer name)
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))
     ;; Buffer name not match below blacklist.
     (string-prefix-p "archief" name)
     (string-prefix-p "*epc" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p "*Ediff" name)
     (string-prefix-p "*anaconda" name)
     (string-prefix-p "*Quail" name)
     (string-prefix-p "*WoMan" name)
     (string-prefix-p "*R" name)
     (string-prefix-p "*ESS" name)
     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
	      (not (file-name-extension name)))
     )))

(provide 'pfn-functions)
;;; pfn-functions.el ends here
