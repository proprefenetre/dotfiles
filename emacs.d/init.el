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
(require 'utils) ; functions and keybindings

(use-package paredit
  :ensure t)

(use-package aggressive-indent
  :ensure t)

(use-package yaml-mode
  :ensure t
  :defer t
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'delete-trailing-whitespace))

(defun pfn/setup-prog-mode ()
  (auto-fill-mode)
  (rainbow-delimiters-mode)
  (display-line-numbers-mode)
  (delete-trailing-whitespace)
  (flycheck-mode 1))

(add-hook 'prog-mode-hook 'pfn/setup-prog-mode)

(use-package rust-mode
  :ensure t)

(use-package python-mode
  :ensure t
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

(defun pfn/setup-lisp-mode ()
  (interactive)
  (eldoc-mode 1)
  (paredit-mode)
  (aggressive-indent-mode))

(add-hook 'emacs-lisp-mode 'pfn/setup-lisp-mode)

(use-package racket-mode
  :config
  (add-hook 'racket-mode-hook 'pfn/setup-lisp-mode))


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
  (define-key evil-motion-state-map "gc" nil)
  (define-key evil-motion-state-map "gC" 'eyebrowse-close-window-config)
  (eyebrowse-mode t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

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
              ispell-silently-savep t)  ; don't ask for confirmation when adding
                                        ; a word to personal dictionary

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-saves") t)))

(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(mouse-avoidance-mode 'exile)                    ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

(scroll-bar-mode -1)                   ; Disable the scroll bar
(tool-bar-mode -1)                     ; Disable the tool bar
(tooltip-mode -1)                      ; Disable the tooltips
(menu-bar-mode -1)                    ; Disable the menu bar

;; keys
(use-package general
  :ensure t
  :demand t)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix ","
 :non-normal-prefix "M-,"
 "e"  'eval-defun
 "a"  'org-agenda-list
 "i"  'pfn/open-init-file
 "o"  'olivetti-mode
 ","  'other-window
 "."  'mode-line-other-buffer
 ":"  'eval-expression
 "b"  'ivy-switch-buffer
 "q"  'kill-this-buffer
 "w"  'save-buffer
 "x"	 'counsel-M-x
 "y"  'counsel-yank-pop)

(use-package key-chord
  :ensure t
  :demand t
  :config
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "gs" 'magit-status)
  (key-chord-mode 1))

(evil-add-hjkl-bindings occur-mode-map 'emacs
  (kbd "/")       'evil-search-forward
  (kbd "n")       'evil-search-next
  (kbd "N")       'evil-search-previous
  (kbd "C-d")     'evil-scroll-down
  (kbd "C-u")     'evil-scroll-up
  (kbd "C-w C-w") 'other-window)
(evil-define-key 'normal global-map (kbd "/")       'swiper)
(evil-define-key 'normal global-map (kbd "j")       'evil-next-visual-line)
(evil-define-key 'normal global-map (kbd "k")       'evil-previous-visual-line)
(evil-define-key 'normal global-map (kbd "-")       'counsel-find-file)
(evil-define-key 'normal global-map (kbd "C-e")     'end-of-line)
  ;;; insert
(evil-define-key 'insert global-map (kbd "C-e")     'end-of-line)
(evil-define-key 'visual global-map (kbd "C-e")     'end-of-line)

;; Make escape quit everything, whenever possible.
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
    In Delete Selection mode, if the mark is active, just deactivate it;
    then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key (kbd "C-c s") 'pfn/ispell-toggle-dictionary)
(global-set-key (kbd "C-x 2") 'pfn/vsplit-new-buffer)
(global-set-key (kbd "C-x 3") 'pfn/hsplit-new-buffer)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c R") 'pfn/reload-init)
(global-set-key (kbd "C-c r") 'pfn/revert-buffer-no-confirm)
(global-set-key (kbd "C-c b") 'mode-line-other-buffer)
(global-set-key (kbd "C-c k") 'counsel-ag)

(global-set-key (kbd "C-c a")
                (defhydra hydra-org-agenda (:color blue :hint nil)
                  "
 ^Agenda^
----------
  Modes:

 _a_genda
 _t_odo
"
                  ("a" org-agenda-list)
                  ("t" org-todo-list)))

;; theme
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

;;; native line numbers
(setq display-line-numbers-width 3
      display-line-numbers-widen t)
(set-face-attribute 'line-number nil :background 'unspecified)

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
