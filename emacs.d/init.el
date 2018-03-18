; Emacs configuration

(setq user-full-name "Niels Eigenraam"
      user-mail-address "nielseigenraam@gmail.com")

(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package-settings)
(require 'evil-settings)
(require 'org-settings)
(require 'prog-settings)
(require 'modeline-settings)
(require 'utils) ; functions and keybindings

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))
  ;(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

(use-package magit
  :commands (magit-status magit-blame magit-log-buffer-file magit-log-all))

(use-package base16-theme
  :ensure t
  :init
  (setq custom-safe-themes t)
  (setq base16-distinct-fringe-background nil)
  :demand t
  :config
  (load-theme 'base16-gruvbox-dark-soft))

(use-package rainbow-delimiters
  :ensure t
  :demand t)

(use-package eyebrowse
  :ensure t
  :demand t
  :init
  :config
  (setq eyebrowse-new-workspace t)
  (eyebrowse-setup-opinionated-keys)
  (define-key evil-motion-state-map "gc" nil)
  (define-key evil-motion-state-map "gC" 'eyebrowse-close-window-config)
  (eyebrowse-mode t))

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))

(use-package helm
  :ensure t
  :config
  (setq helm-split-window-inside-p t)
  (helm-mode 1))

(use-package olivetti
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode
  ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc")
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode)
  :config
  (font-lock-add-keywords 'markdown-mode
                        '(("@[[:alnum:]]+" . font-lock-keyword-face))))

(use-package guess-language
  :ensure t
  :defer t
  :config
  (setq guess-language-langcodes '((en . ("english" "English"))
                                   (nl . ("dutch" "Dutch")))
        guess-language-languages '(en nl)
        guess-language-min-paragraph-length 45))

(setq-default
 auto-window-vscroll nil                          ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 tab-width 4                                      ; Set width for tabs
 fill-column 80                                   ; Set width for automatic line breaks
 inhibit-splash-screen t                         ; Disable start-up screen
 inhibit-startup-message t
 visual-bell nil
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 recenter-positions '(5 top bottom)               ; Set re-centering positions
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 scroll-margin 10                                 ; Add a margin when scrolling vertically
 indicate-empty-lines nil
 x-select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 save-interprogram-paste-before-kill t
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                       ; Display trailing whitespaces
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                              ; Stretch cursor to the glyph width
 vc-follow-symlinks t
 large-file-warning-threshold nil
 make-backup-files nil
 ispell-silently-savep t)

(fringe-mode 0)                                   ; Disable fringes
(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(mouse-avoidance-mode 'banish)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)

(when window-system
  (scroll-bar-mode 0)                             ; Disable the scroll bar
  (tool-bar-mode 0)                               ; Disable the tool bar
  (tooltip-mode 0)                               ; Disable the tooltips
  (menu-bar-mode 0))                                 ; Disable the menu bar

(set-face-attribute 'default nil :font "Hack-10" )
(setq initial-buffer-choice "~/org/todo.org")

;; (setq-default left-fringe-width nil)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'guess-language-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)

;; garbage collect on focus-out
(add-hook 'focus-out-hook #'garbage-collect)

; lower garbace collection threshold
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
