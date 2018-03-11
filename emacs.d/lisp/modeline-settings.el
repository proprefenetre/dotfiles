; The mode-line -- gejat van het internet

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/override-theme nil)
    (rich-minority-mode 1)
    (setq rm-whitelist "eyebrowse")
    (add-hook 'after-init-hook #'sml/setup)))

(line-number-mode t)
(column-number-mode t)

(provide 'modeline-settings)
