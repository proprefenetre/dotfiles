;;; init.el --- Emacs Configuration
;;; Commentary:
;;; Code:

(setq user-full-name "Niels Eigenraam"
      user-mail-address "nielseigenraam@gmail.com")

(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'package-settings)

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(server-start)

(require 'evil-settings)
(require 'org-settings)
(require 'utils)                        ;functions
(require 'hydras)                       ;hydras

;; Packages
(use-package avy
  :config
  (setq avy-timeout-seconds 0.2))

(use-package paredit
  :ensure t)

(use-package aggressive-indent
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode
  ("\\.yml" . yaml-mode)
  ("\\.yaml" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'delete-trailing-whitespace))

(use-package rust-mode
  :ensure t)

(use-package python-mode
  :ensure t
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

(use-package elpy
  :ensure t
  :init
  (with-eval-after-load 'python (elpy-enable))
  :config
  (delete 'elpy-module-highlight-indentation elpy-modules))

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook 'pfn/setup-lisp-mode))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package magit
  :ensure t
  :commands
  (magit-status magit-blame magit-log-buffer-file magit-log-all)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package rainbow-delimiters
  :ensure t
  :demand t)

(use-package eyebrowse
  :ensure t
  :demand
  :config
  (setq eyebrowse-new-workspace t)
  (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode t))

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :ensure t
  :demand t
  :config
  (counsel-mode 1))

(use-package hydra
  :ensure t)

(use-package olivetti
  :ensure t
  :config
  (setq-default olivetti-body-width 90))

(use-package markdown-mode
  :ensure t
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
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))
  (yas-global-mode 1))

(use-package guess-language
  :ensure t
  :config
  (setq guess-language-langcodes '((en . ("english" "English"))
                                   (nl . ("dutch" "Dutch")))
        guess-language-languages '(en nl)
        guess-language-min-paragraph-length 45))

(defun pfn/setup-lisp-mode ()
  "Setup lisp-modes such as racket and emacs-lisp."
  (eldoc-mode 1)
  (paredit-mode)
  (aggressive-indent-mode))

(add-hook 'emacs-lisp-mode-hook 'pfn/setup-lisp-mode)

(defun pfn/setup-prog-mode ()
  "Load 'prog-mode' minor modes."
  (auto-fill-mode)
  (rainbow-delimiters-mode)
  (display-line-numbers-mode)
  (delete-trailing-whitespace)
  (flycheck-mode 1)
  (company-mode 1))

(add-hook 'prog-mode-hook 'pfn/setup-prog-mode)

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
 mouse-yank-at-point t     ; Yank at point rather than pointer
 recenter-positions '(5 top bottom) ; Set re-centering positions
 scroll-conservatively most-positive-fixnum ; Always scroll by one line
 scroll-margin 10          ; Add a margin when scrolling vertically
 indicate-empty-lines nil  ; no fuzz at the end of a file
 x-select-enable-clipboard t ; Merge system's and Emacs' clipboard
 save-interprogram-paste-before-kill t ; ?
 sentence-end-double-space nil ; End a sentence after a dot and a space
 show-trailing-whitespace t  ; Display trailing whitespaces
 window-combination-resize t   ; Resize windows proportionally
 x-stretch-cursor t            ; Stretch cursor to the glyph width
 vc-follow-symlinks t     ; so you end up at the file itself rather than editing
                                        ; the link
 large-file-warning-threshold nil ; this
 ispell-silently-savep t)  ; don't ask for confirmation when adding a word to
                           ; personal dictionary

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-saves") t)))

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
  :ensure t
  :demand t)

(use-package key-chord
  :ensure t
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
             minibuffer-local-isearch-map)
  [escape] 'minibuffer-keyboard-quit
  [C-w] 'pfn/backward-delete-word)

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
  "c"  'company-complete
  "f"  'avy-goto-char-timer
  "/"  'swiper
  "e"  'eval-defun
  "i"  'pfn/open-init-file
  "o"  'olivetti-mode
  ","  'other-window
  "."  'mode-line-other-buffer
  "b"  'hydra-buffer/body
  "q"  'kill-this-buffer
  "w"  'save-buffer
  "x"  'counsel-M-x
  "y"  'counsel-yank-pop
  "m"  'counsel-bookmark
  "gs" 'magit-status)

(general-def 'emacs org-agenda-mode-map
  "C-w C-w" 'other-window)

(general-def 'motion
  "j"   'evil-next-visual-line
  "k"   'evil-previous-visual-line
  "-"   'counsel-find-file
  "_"   'counsel-recentf
  [escape] 'keyboard-quit
  "C-e" 'end-of-line
  "C-w v" 'pfn/vsplit-new-buffer
  "C-w h" 'pfn/hsplit-new-buffer
  "gc" nil
  "gC" 'eyebrowse-close-window-config)

(general-def :keymaps 'evil-insert-state-map
  (general-chord "jj") 'evil-normal-state
  "C-e" 'end-of-line
  "C-a" 'beginning-of-line)

(general-def
  "C-c s" 'pfn/ispell-toggle-dictionary
  "C-c g" 'magit-status
  "C-c R" 'pfn/reload-init
  "C-c r" 'pfn/revert-buffer-no-confirm
  "C-c b" 'mode-line-other-buffer
  "C-c k" 'counsel-ag
  "C-c l" 'org-store-link
  "C-c c" 'org-capture
  "C-c f" 'org-archive
  "C-c w" 'org-refile
  "C-c a" 'hydra-org/body)

;; theme
(use-package nord-theme)
(use-package nordless-theme)
(use-package gruvbox-theme)
(use-package solarized-theme)

(setq custom-safe-themes t)
(load-theme 'nord t)

(setq solarized-use-variable-pitch nil)
(setq monokai-use-variable-pitch nil)

(set-face-attribute 'default nil :font "Hack-10" )

(fringe-mode '(8 . 8))
;; (set-face-attribute 'fringe nil :inherit 'line-number)

;;; native line numbers
(setq display-line-numbers-width 4
      display-line-numbers-width-start 3
      display-line-numbers-widen t)
;; (set-face-attribute 'line-number nil :background 'unspecified)

;; modeline
(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  (line-number-mode t)
  (column-number-mode t)
  (setq sml/theme 'respectful)
  (setq sml/modified-char "+")
  (setq sml/mode-width 'right)
  (sml/setup))

(use-package minions
  :ensure t
  :demand t
  :config
  (minions-mode 1))

(setq initial-buffer-choice "~/org/algemeen.org")

(defun pfn/text-mode-hooks ()
  "Load 'text-mode' hooks."
  (flyspell-mode 1)
  (guess-language-mode 1)
  (turn-on-auto-fill)
  (rainbow-delimiters-mode 1)
  (abbrev-mode 1))
(add-hook 'text-mode-hook 'pfn/text-mode-hooks)

;; garbage collect on focus-out
(add-hook 'focus-out-hook #'garbage-collect)

;; lower garbace collection threshold
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;;; init.el ends here
