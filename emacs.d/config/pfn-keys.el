;;; pfn-keys.el --- bindings using General.el
;;; Commentary:
;;; Code:

(use-package key-chord
  :demand t
  :config (key-chord-mode 1))

(use-package which-key
  :demand t
  :config (which-key-mode 1))

(use-package smex
  :demand t)

(use-package hydra)

(use-package general
  :demand t
  :config
  (general-evil-setup)
  (general-override-mode)

  (general-create-definer evil-leader
    :prefix ",")

  (evil-leader
    :states '(normal visual emacs treemacs)
    :keymaps 'override
    "b" 'ibuffer
    "c" 'capitalize-dwim
    "d" 'dired-jump
    ;; "e" 'eval-last-sexp
    "g" 'evil-commentary-yank-line
    "f" 'ffap
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
             (org-capture nil "c"))
    ;; "d"
    "C-d" 'dired-jump-other-window
    ;; "e"
    "f"   'flymake-show-diagnostics-buffer
    ;; "g" "h"
    "i"   'ibuffer
    ;; "j"
    "k"   'counsel-ag
    "l"   'org-store-link
    ;; "m" "n" "o"
    "p"   'projectile-command-map
    ;; "q"
    "R"   '(lambda () (interactive)
             (revert-buffer :ignore-auto :noconfirm))
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
    "<menu>" 'counsel-M-x
    "M-x" 'counsel-M-x
    "M-/" 'hippie-expand
    "C-)" 'sp-forward-slurp-sexp
    "C-(" 'sp-add-to-previous-sexp
    "C-s" 'swiper
    "C-," 'embrace-commander)

  (general-mmap
    "j"   'evil-next-visual-line
    "k"   'evil-previous-visual-line
    "C-e" 'evil-end-of-line
    "[ p" 'evil-paste-before
    "] p" 'evil-paste-after
    "`"   'evil-avy-goto-char
    "C-b" 'mode-line-other-buffer)

  (general-def
    :keymaps 'evil-insert-state-map
    (general-chord "jj") 'evil-normal-state)

  (general-def
    :keymaps 'evil-normal-state-map
    (general-chord "bi") 'ibuffer
    (general-chord "tt") 'treemacs
    "s-q" 'kill-this-buffer
    "gt" 'centaur-tabs-forward
    "gT" 'centaur-tabs-backward)

  (general-def
    :keymaps 'evil-visual-state-map
    ")" 'er/expand-region
    "(" 'er/contract-region)

  (general-def
    :keymaps 'goto-map
    "f" 'avy-goto-char
    "t" 'avy-goto-word-1
    "<tab>" 'centaur-tabs-counsel-switch-group
    "g" 'dumb-jump-hydra/body
    "n" 'flymake-goto-next-error
    "p" 'flymake-goto-prev-error)

  (general-def
    :keymaps 'evil-window-map
    "C-w" 'ace-window)

  ;; mode specific
  (general-def
    :keymaps 'company-active-map
    "C-w" 'evil-delete-backward-word
    "C-n"  'company-select-next
    "C-p"  'company-select-previous
    "<tab>" 'company-complete-common-or-cycle
    "<esc>" 'company-cancel)

  (general-def
    :keymaps 'rust-mode-map
    "C-c <tab>" 'rust-format-buffer)

  (general-def
    :keymaps 'org-mode-map
    :prefix "C-c"
    "a"   'org-agenda-list
    "C-a" 'org-archive-subtree
    "r"   'org-hydra/body
    "!"   'org-time-stamp-inactive
    )

  (general-def
    :keymaps 'org-mode-map
    :states 'normal
    "<return>" 'org-return)

  (general-def
    :keymaps 'org-mode-map
    "C-," 'embrace-commander)

  (general-def
    :keymaps 'treemacs-mode-map
    :states 'treemacs
    "<up>" 'treemacs-switch-workspace)

(provide 'pfn-keys)
;;; pfn-keys.el ends here
