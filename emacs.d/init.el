;;; init.el --- Emacs Configuration
;;; Commentary:
;;; Code:

(setq user-full-name "Niels Eigenraam"
      user-mail-address "nielseigenraam@gmail.com")

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'package-settings)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(server-start)

(require 'evil-settings)
(require 'org-settings)
(require 'utils)                        ;functions

;; Packages
(use-package avy
  :demand t
  :config
  (setq avy-timeout-seconds 0.2))

(use-package paredit)

(use-package aggressive-indent)

(use-package yaml-mode
  :mode
  ("\\.yml" . yaml-mode)
  ("\\.yaml" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'delete-trailing-whitespace))

(use-package rust-mode)

(use-package python-mode
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

(use-package elpy
  :init
  (with-eval-after-load 'python (elpy-enable))
  :config
  (delete 'elpy-module-highlight-indentation elpy-modules))

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook 'pfn-setup-lisp-mode))

(use-package company
  :demand t
  :config
  (setq company-idle-delay 0.3
        company-selection-wrap-around t)
  (add-to-list 'company-backends 'org-keyword-backend))

(use-package magit
  :commands
  (magit-status magit-blame magit-log-buffer-file magit-log-all)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package rainbow-delimiters
  :demand t)

(use-package rainbow-mode)

(use-package eyebrowse
  :demand
  :config
  (setq eyebrowse-new-workspace t
        eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t)
  (eyebrowse-mode t))

(use-package shackle
  :demand t
  :config
  (progn
    (setq shackle-select-reused-windows nil) ; default nil
    (setq shackle-default-alignment 'below) ; default below
    (setq shackle-default-size 0.3) ; default 0.5
    (setq shackle-rules
          '(("*Warnings*"  :select t :align below :inhibit-window-quit nil :modeline nil)
            ("magit: *"  :regexp t :select t :align below :inhibit-window-quit nil :modeline nil)
            ("*Messages*"  :size 12 :noselect t)
            ("*Help*" :select t :align below :inhibit-window-quit nil :modeline nil)
            ("*Metahelp*" :size 0.3 :align left)
            ("*undo-tree*" :size 0.5 :align right)))
    (shackle-mode 1)))

(use-package which-key
  :demand t
  :config
  (which-key-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :demand t
  :config
  (counsel-mode 1))

(use-package hydra
  :demand t
  :config
  (require 'hydras))

(use-package olivetti
  :config
  (setq-default olivetti-body-width 90))

(use-package markdown-mode
  :mode
  ("\\.md" . markdown-mode)
  ("\\.mdpp" . markdown-mode)
  :init
  (setq markdown-command "pandoc")
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode)
  :config
  (font-lock-add-keywords 'markdown-mode
                          '(("@[[:alnum:]]+" . font-lock-keyword-face))))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))
  (yas-global-mode 1))

(use-package guess-language
  :config
  (setq guess-language-langcodes '((en . ("english" "English"))
                                   (nl . ("dutch" "Dutch")))
        guess-language-languages '(en nl)
        guess-language-min-paragraph-length 45))

(defun pfn-setup-lisp-mode ()
  "Setup lisp-modes such as racket and emacs-lisp."
  (eldoc-mode 1)
  (paredit-mode)
  (aggressive-indent-mode))

(add-hook 'emacs-lisp-mode-hook 'pfn-setup-lisp-mode)

(defun pfn-setup-prog-mode ()
  "Load 'prog-mode' minor modes."
  (auto-fill-mode)
  (rainbow-delimiters-mode)
  (display-line-numbers-mode)
  (delete-trailing-whitespace)
  (flycheck-mode 1)
  (company-mode 1))

(add-hook 'prog-mode-hook 'pfn-setup-prog-mode)

(defun pfn-setup-text-mode ()
  "Load 'text-mode' hooks."
  (flyspell-mode 1)
  (guess-language-mode 1)
  (turn-on-auto-fill)
  (rainbow-delimiters-mode 1)
  (abbrev-mode 1))
(add-hook 'text-mode-hook 'pfn-setup-text-mode)

(setq-default
 auto-window-vscroll nil         ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p ; Confirm before exiting Emacs
 cursor-in-non-selected-windows t ; Hide the cursor in inactive windows
 help-window-select t             ; Focus new help windows when opened
 indent-tabs-mode nil      ; Stop using tabs to indent
 tab-width 4               ; Set width for tabs
 fill-column 80            ; Set width for automatic line breaks
 inhibit-splash-screen t   ; Disable start-up screen
 inhibit-startup-message t ; No startup-message
 visual-bell nil           ; plz no visual bell
 ring-bell-function 'ignore
 mouse-yank-at-point t     ; Yank at point rather than pointer
 recenter-positions '(5 top bottom) ; Set re-centering positions
 scroll-conservatively most-positive-fixnum ; Always scroll by one line
 scroll-margin 10          ; Add a margin when scrolling vertically
 indicate-empty-lines nil  ; no fuzz at the end of a file
 x-select-enable-clipboard t ; Merge system's and Emacs' clipboard
 save-interprogram-paste-before-kill t ; ?
 sentence-end-double-space nil ; End a sentence after a dot and a space
 show-trailing-whitespace nil  ; Display trailing whitespaces
 window-combination-resize t   ; Resize windows proportionally
 x-stretch-cursor t            ; Stretch cursor to the glyph width
 vc-follow-symlinks t     ; so you end up at the file itself rather than editing
                                        ; the link
 large-file-warning-threshold nil ; this
 ispell-silently-savep t  ; don't ask for confirmation when adding a word to
 ispell-dictionary "dutch"
 ispell-extra-args '("-a" "utf-8"))                         ; personal dictionary

(setq
 make-backup-files t
 version-control t   ; use numbers for backup files
 kept-new-versions 10
 kept-old-versions 0
 delete-old-versions t
 backup-by-copying t
 vc-make-backup-files t
 auto-save-default nil
 backup-directory-alist
 `(("." . ,(concat user-emacs-directory "backups")))
 auto-save-file-name-transforms
 `(("." ,(concat user-emacs-directory "auto-saves") t))
 create-lockfiles nil)

(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                 ; Iterate through CamelCase words
(mouse-avoidance-mode 'exile)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

(scroll-bar-mode -1)                   ; Disable the scroll bar
(tool-bar-mode -1)                     ; Disable the tool bar
(tooltip-mode -1)                      ; Disable the tooltips
(menu-bar-mode -1)                     ; Disable the menu bar

;; General
(use-package general
  :demand t)

(use-package key-chord
  :demand t
  :config
  (key-chord-mode 1))

;; Make escape quit everything, whenever possible.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(general-def
  :keymaps '(minibuffer-local-map
             Minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             ivy-minibuffer-map
             swiper-map)
  [escape] 'minibuffer-keyboard-quit
  "C-w" 'pfn-backward-delete-word)

(general-def 'emacs occur-mode-map
  "/" 'evil-search-forward
  "n" 'evil-search-backward
  "N" 'evil-search-backward
  "C-d" 'evil-scroll-down
  "C-u" 'evil-scroll-up
  "C-w C-w" 'other-window)

(general-create-definer evil-leader-def
  :states '(normal visual insert emacs)
  :prefix ","
  :non-normal-prefix "M-,")

(evil-leader-def
  "t"  'hydra-toggle/body
  ":"  'counsel-find-file
  "e"  'eval-defun
  "i"  'pfn-open-init-file
  "o"  'olivetti-mode
  ","  'other-window
  "."  'mode-line-other-buffer
  "b"  'hydra-buffer/body
  "q"  'kill-buffer-and-window
  "w"  'save-buffer
  "x"  'counsel-M-x
  "p"  'counsel-yank-pop
  "m"  'counsel-bookmark
  "gs" 'magit-status
  "r"  'rainbow-mode)

(general-def 'emacs org-agenda-mode-map
  "C-w C-w" 'other-window)

(general-def '(motion normal) org-mode-map
  [return]  'org-open-at-point
  "<tab>"   'org-cycle
  "TAB"     'org-cycle)

(general-def 'motion
  "j"       'evil-next-visual-line
  "k"       'evil-previous-visual-line
  "-"       'dired-jump
  [escape]  'keyboard-quit
  "C-e"     'end-of-line
  "C-w C-v" 'pfn-vsplit-new-buffer
  "C-w C-h" 'pfn-hsplit-new-buffer
  "/"       'swiper
  "C-f"     'evil-avy-goto-char-timer)

(general-def 'visual eyebrowse-mode-map
  "gc" nil)

(general-def :keymaps 'evil-insert-state-map
  (general-chord "jj") 'evil-normal-state
  "C-e" 'end-of-line
  "C-a" 'beginning-of-line
  "<M-tab>" 'company-complete-common-or-cycle)

(general-def
  "C-c R" 'pfn-reload-init
  "C-c r" 'pfn-revert-buffer-no-confirm
  "C-c b" 'mode-line-other-buffer
  "C-c k" 'counsel-ag
  "C-c a" 'hydra-org/body
  "M-/"   'hippie-expand
  "C-c l" 'org-store-link
  "C-c c" 'org-capture)

;; completion
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; theme
(use-package challenger-deep-theme)
(use-package nord-theme)
(use-package solarized-theme)

(setq custom-safe-themes t)
(load-theme 'challenger-deep t)

(set-face-attribute 'default nil :font "Hack-10" )
(fringe-mode '(8 . 8))
(set-face-attribute 'fringe nil :inherit 'line-number)

;;; native line numbers
(setq display-line-numbers-width 4
      display-line-numbers-width-start 3
      display-line-numbers-widen t)
(set-face-attribute 'line-number nil :background 'unspecified)

;; modeline
(use-package smart-mode-line
  :demand t
  :config
  (line-number-mode t)
  (column-number-mode t)
  (setq sml/theme 'respectful)
  (setq sml/modified-char "+")
  (setq sml/mode-width 'full)
  (sml/setup))

(use-package minions
  :demand t
  :config
  (minions-mode 1))

(setq initial-buffer-choice "~/org/todo.org")

(add-hook 'after-init-hook 'global-company-mode)
(put 'dired-find-alternate-file 'disabled nil)

;; garbage collect on focus-out
(add-hook 'focus-out-hook #'garbage-collect)

;; lower garbace collection threshold
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;;; init.el ends here
