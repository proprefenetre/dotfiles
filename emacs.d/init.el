;; Emacs configuration
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
(require 'prog-settings)
(require 'utils) ; functions and keybindings

(use-package aggressive-indent
  :ensure t)

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
  :demand t
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

;; (use-package helm
;;   :ensure t
;;   :demand t
;;   :config
;;   (setq helm-split-window-inside-p t)
;;   (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;;   (helm-mode 1))

(use-package ivy
  :demand t
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
  ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc")
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode)
  :config
  (font-lock-add-keywords 'markdown-mode
                          '(("@[[:alnum:]]+" . font-lock-keyword-face))))

(use-package yasnippet
  :ensure t
  :demand t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))
  (yas-global-mode 1))


(use-package guess-language
  :ensure t
  :defer t
  :config
  (setq guess-language-langcodes '((en . ("english" "English"))
                                   (nl . ("dutch" "Dutch")))
        guess-language-languages '(en nl)
        guess-language-min-paragraph-length 45))

(setq-default auto-window-vscroll nil         ; Lighten vertical scroll
              confirm-kill-emacs 'yes-or-no-p ; Confirm before exiting Emacs
              cursor-in-non-selected-windows t ; Hide the cursor in inactive windows
              help-window-select t      ; Focus new help windows when opened
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
              show-trailing-whitespace nil  ; Display trailing whitespaces
              window-combination-resize t   ; Resize windows proportionally
              x-stretch-cursor t            ; Stretch cursor to the glyph width
              vc-follow-symlinks t     ; so you end up at the file itself rather
                                        ; than editing the link
              large-file-warning-threshold nil ; this
              make-backup-files nil            ; no thanks
              ispell-silently-savep t)  ; don't ask for confirmation when adding
                                        ; a word to personal dictionary

(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(mouse-avoidance-mode 'banish)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

(scroll-bar-mode -1)                   ; Disable the scroll bar
(tool-bar-mode -1)                     ; Disable the tool bar
(tooltip-mode -1)                      ; Disable the tooltips
(menu-bar-mode -1)                    ; Disable the menu bar

(use-package monokai-theme)
(use-package gruvbox-theme)
(use-package solarized-theme)

(setq custom-safe-themes t)
(load-theme 'solarized-light t)

(setq solarized-use-variable-pitch nil)
(setq monokai-use-variable-pitch nil)

(set-face-attribute 'default nil :font "Hack-10" )

(fringe-mode '(8 . 8))
(set-face-attribute 'fringe nil :inherit 'line-number)

;; native line numbers
(setq display-line-numbers-width 3
      display-line-numbers-widen t)
(set-face-attribute 'line-number nil :background 'unspecified)

;; modeline
(line-number-mode t)
(column-number-mode t)

(use-package minions
  :ensure t
  :demand t
  :config
  (minions-mode 1))

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
                         minions-mode-line-modes
                         mode-line-misc-info
                         mode-line-end-spaces))

(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  ;; (setq rm-whitelist
  ;;       (format "^ \\(%s\\)$"
  ;;               (mapconcat #'identity
  ;;                          '("=>" "Paredit")
  ;;                          "\\|")))
  ;; (dolist (prop '(("\\` Paredit\\'" 'display " ()" 'face 'font-lock-comment-face)))
  ;;   (add-to-list 'rm-text-properties prop))
  (setq sml/theme 'respectful)
  (setq sml/modified-char "+")
  (setq sml/mode-width 'right)
  (sml/setup))

(setq initial-buffer-choice "~/org/algemeen.org")

(defun pfn/text-mode-hooks ()
  "load text-mode hooks"
  (flyspell-mode)
  (guess-language-mode)
  (turn-on-auto-fill)
  (rainbow-delimiters-mode))
(add-hook 'text-mode-hook 'pfn/text-mode-hooks)

;; garbage collect on focus-out
(add-hook 'focus-out-hook #'garbage-collect)

;; lower garbace collection threshold
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
