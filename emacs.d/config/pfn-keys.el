;;; pfn-keys.el --- keybinding
;;; Commentary: 
;;; Code:

(use-package hydra)

(use-package general
  :demand t
  :config
  (general-override-mode)
  (general-evil-setup)
  ;; general keybindings
  (general-create-definer evil-leader
    :prefix ",")

  (evil-leader
    :states '(normal visual emacs)
    :keymaps 'override
    ;; "b" '
    "c" 'capitalize-dwim
    "d" 'dired-jump
    "e" 'eval-last-sexp
    "g" 'evil-commentary-yank-line
    "f" 'flycheck-list-errors
    "i" '(lambda () (interactive)
           (find-file user-init-file))
    "o" 'olivetti-mode
    "p" 'counsel-yank-pop
    "q" 'evil-window-delete
    "r" '(lambda () (interactive)
           (revert-buffer :ignore-auto :noconfirm))
    "n" 'symbol-overlay-rename
    "s" 'magit-status
    "t" 'treemacs-select-window
    "w" 'ace-window)

  (general-def
    :prefix "C-c"
    "b"   'counsel-bookmark
    "c"   '(lambda () (interactive)
             (org-capture "c"))
    ;; "D"
    "C-d" 'dired-jump-other-window
    ;; "e"
    "f"   'ffap-other-window
    ;; "g" "h"
    "i"   'ibuffer
    ;; "j"
    "k"   'counsel-ag
    "l"   'org-store-link
    ;; "m" "n" "o"
    "p"   'projectile-command-map
    ;; "q"
    "R"   '(lambda () (interactive)
             (load-file buffer-file-name))
    "s"   'counsel-rg
    "t"   'treemacs
    ;; "u"
    ;; "v"
    "w"   'ace-window
    ;;"x"
    "C-l" 'comint-clear-buffer)

  (general-def
    :prefix "C-x"
    "ESC ESC" 'keyboard-quit
    "C-b" 'counsel-ibuffer
    "2" '(lambda () (interactive)
           (split-window-below)
           (other-window 1))
    "3" '(lambda () (interactive)
           (split-window-right)
           (other-window 1)))

  (general-def
    "M-x" 'counsel-M-x
    "M-/" 'hippie-expand
    "C-)" 'sp-forward-slurp-sexp
    "C-(" 'sp-add-to-previous-sexp
    "C-s-s" 'query-replace
    "C-s" 'swiper)

  (general-mmap
    "j"   'evil-next-visual-line
    "k"   'evil-previous-visual-line
    "C-e" 'evil-end-of-line
    "[ p" 'evil-paste-before
    "] p" 'evil-paste-after
    "`"   'evil-avy-goto-char
    "C-b" 'mode-line-other-buffer)

  (general-nmap
    "s-q" 'kill-this-buffer
    (general-chord "bi") 'ibuffer
    "C-," 'embrace-commander)

  (general-imap
    (general-chord "jj") 'evil-normal-state
    (general-chord "ww") 'evil-window-next)

  (general-vmap
    ")" 'er/expand-region)

  (general-def
    :keymaps 'goto-map
    "f" 'avy-goto-char
    "t" 'avy-goto-word-1)

  ;; package specific
  (general-def
    :keymaps 'company-active-map
    "C-w" 'evil-delete-backward-word
    "C-n"  'company-select-next
    "C-p"  'company-select-next
    "C-M-i" 'company-complete-common-or-cycle
    "<esc>" 'company-cancel)

  (general-def
    :keymaps 'rust-mode-map
    "C-c <tab>" 'rust-format-buffer)

  (general-def
    :keymaps 'org-mode-map
    :prefix "C-c"
    "a"   'org-agenda-list
    "C-a" 'org-archive-subtree
    "r"   'org-refile
    "!"   'org-time-stamp-inactive
    "l p" 'org-latex-export-to-pdf
    "l l" 'org-latex-export-as-latex)

  (general-def
    :keymaps 'org-mode-map
    "<return>" 'org-return)

  (general-def
    :keymaps 'treemacs-mode-map
    :states 'treemacs
    "C-w s" 'treemacs-switch-workspace))

(provide 'pfn-keys)
;;; pfn-keys.el ends here
