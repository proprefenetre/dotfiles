;;; Deprecated snippets of config

;; (use-package challenger-deep-theme
;; theme colors
;; ---------- ----------
;; "#906cff"  "#91ddff"
;; "#65b2ff"  "#ffd75f"
;; "#62d196"  "#ff5458"
;; "#ffe9aa"  "#95ffa4"
;; "#aaffe4"  "#ff8080"
;;   :demand t
;;   :config
;;   (load-theme 'challenger-deep t))


;; (setq org-todo-keyword-faces
;;       '(("READ" . "#65b2ff")
;;         ("GOOGLE" . "#65b2ff")
;;         ("AFSPRAAK" . "#906cff")
;;         ("WEEKLY" . "#ff8080")
;;         ("CANCELED" . "#ff8080")))

;; (use-package smart-mode-line
;;   :demand t
;;   :config
;;   (line-number-mode t)
;;   (column-number-mode t)
;;   (setq sml/theme 'respectful)
;;   (setq sml/modified-char "+")
;;   (setq sml/name-width 40)
;;   (setq sml/mode-width 'full)
;;   (setq rm-whitelist
;;         (format "^ \\(%s\\)$" (mapconcat #'identity
;;                                          '("()" "Fly" "PRJ.*" "=>") "\\|")))
;;   (add-to-list 'sml/replacer-regexp-list '("^~/projects/thesis" ":thesis:") t)
;;   (add-to-list 'sml/replacer-regexp-list '("^~/projects/" ":projects:") t)
;;   (add-to-list 'sml/replacer-regexp-list '("^~/dotfiles" ":df:"))
;;   (add-to-list 'sml/replacer-regexp-list '("^~/org" ":org:"))
;;   (sml/setup))

;; (dolist (key '("<return>" "RET"))
;;   (general-def company-active-map key
;;     `(menu-item nil company-complete
;; 		        :filter ,(lambda (cmd)
;; 			               (when (company-explicit-action-p)
;; 			                 cmd)))))
