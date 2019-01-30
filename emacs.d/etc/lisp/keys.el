;;; keys.el -- emacs keybindings
;;; commentary:
;;; code:

(setq tab-always-indent t)

(general-evil-setup)

(general-mmap
  "j"   'evil-next-visual-line
  "k"   'evil-previous-visual-line
  "C-e" 'evil-end-of-line
  "[ p" 'evil-paste-before
  "] p" 'evil-paste-after)

(general-def
  :keymaps 'evil-insert-state-map
  (general-chord "jj") 'evil-normal-state)

(general-def 'normal (ibuffer-mode-map image-mode-map inferior-ess-r-mode-map)
  "," nil
  "C-c C-w" 'eyebrowse-last-window-config)

(general-def 'insert markdown-mode-map
  "TAB"   'yas-expand
  "<tab>" 'yas-expand)

(general-def company-active-map
  "SPC"   nil
  "C-w"   'evil-delete-backward-word
  "C-h"   nil
  "TAB"   'yas-expand
  "<tab>" 'yas-expand)

(general-def '(normal visual emacs)
  :prefix ","
  :non-normal-prefix "M-,"
  :keymaps '(override inferior-ess-r-mode-map)
  "b" 'switch-to-previous-buffer
  "d" 'dired-jump
  "e" 'eval-last-sexp
  "i" '(lambda () (interactive)
	     (find-file user-init-file))
  "o" 'olivetti-mode
  "p" 'counsel-yank-pop
  "q" 'kill-buffer-and-window
  "r" '(lambda () (interactive)
	     (revert-buffer :ignore-auto :noconfirm))
  "R" '(lambda () (interactive)
	     (load-file user-init-file)
         (message "buffer reloaded"))
  "s" 'magit-status)

(general-def
  :prefix "C-c"
  "a"   'org-agenda
  "b"   'counsel-bookmark
  "c"   'compile
  "d"   'calc-dispatch
  "C-d" 'dired-jump
  ;; "e"
  "f"   'ffap
  ;; "g" "h"
  "i"   'ibuffer
  ;; "j"
  "k"   'counsel-ag
  "l"   'org-store-link
  "L"   '(lambda () (interactive)
           (load-file buffer-file-name))
  ;; "m" "n" "o"
  "p"   'projectile-command-map
  ;; "q"

  "r"   'counsel-recentf
  "R"   '(lambda () (interactive)
           (load-file user-init-file))
  "s"   'cycle-ispell-languages
  ;; "t" "u" "v" "w" "x"
  )


(general-def
  :prefix "C-h"
  "f"   'counsel-describe-function
  "v"   'counsel-describe-variable)

(general-def
  :prefix "C-x"
  "f"   'counsel-find-file
  "C-f" 'counsel-find-file
  "ESC ESC" 'nil)

(general-def
  "C-)"     'sp-forward-slurp-sexp
  "C-;"     'evil-repeat-find-char-reverse
  "C-s"     'swiper
  "M-/"     'hippie-expand
  "M-<tab>" 'company-complete-common
  "M-x"     'counsel-M-x)

(provide 'keys)
;;; keys.el ends here
