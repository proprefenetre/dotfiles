;; The mode-line -- gejat van het internet
(line-number-mode t)
(column-number-mode t)


(setq mode-line-format '("%e"
                         mode-line-front-space
                         mode-line-mule-info
                         mode-line-client
                         mode-line-modified
                         mode-line-remote
                         mode-line-frame-identification
                         mode-line-buffer-identification
                         sml/pos-id-separator
                         mode-line-position
                         evil-mode-line-tag
                         (vc-mode vc-mode)
                         sml/pre-modes-separator
                         mode-line-modes
                         mode-line-misc-info
                         mode-line-end-spaces))

;; (use-package feebleline)

(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  (setq rm-whitelist
        (format "^ \\(%s\\)$"
                (mapconcat #'identity
                           '("=>" "Paredit")
                           "\\|")))
  (dolist (prop '(("\\` Paredit\\'" 'display " ()" 'face 'font-lock-comment-face)))
    (add-to-list 'rm-text-properties prop))
  (setq sml/theme 'respectful)
  (setq sml/modified-char "+")
  (setq sml/mode-width 'right)
  (sml/setup))

(provide 'modeline-settings)
