; The mode-line -- gejat van het internet
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format "%%s %%%ds" available-width) left right)))

;; (setq-default mode-line-format
;;               '((:eval
;;                  (simple-mode-line-render
;;                   ;; left
;;                   (format-mode-line
;;                    '(mode-line-front-space
;;                      "%l:%c"
;;                      mode-line-mule-info
;;                      mode-line-buffer-identification
;;                      mode-line-modified
;;                      " "
;;                      mode-line-position
;;                      evil-mode-line-tag
;;                      (vc-mode vc-mode)))
;;                   ;; right
;;                   (format-mode-line
;;                    '("%m: "
;;                      mode-line-modes
;;                      "%M"
;;                      mode-line-end-spaces))))))

(line-number-mode t)
(column-number-mode t)

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/theme 'respectful
          sml/shorten-directory t
          sml/name-width 40
          sml/mode-width 'right)
    (rich-minority-mode 1)
    (setq rm-whitelist "eyebrowse")
    (add-hook 'after-init-hook #'sml/setup)))

(provide 'modeline-settings)
