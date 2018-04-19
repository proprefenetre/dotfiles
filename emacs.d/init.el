;;; init.el --- Emacs Configuration
;;; Commentary:
;;; Sunday iteration. After dicking around with literate org files for days.
;;; Code:

;;; Initialization
;; Personal settngs
(setq user-full-name "Niels Eigenraam"
      user-mail-address "nielseigenraam@gmail.com")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; garbage collection
(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

;; package.el
(require 'package)
(setq package-enable-at-startup nil
      load-prefer-newer t
      package-user-dir "~/.emacs.d/elpa"
      package--init-file-ensured t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; use-package

;; (package-initialize)                    ; emacs 27+ does this for you. TIL

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-verbose t)

(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;;; theme
(setq custom-safe-themes t)

(use-package challenger-deep-theme)
(use-package nord-theme)

(load-theme 'challenger-deep t)

(fringe-mode '(8 . 8))

(set-face-attribute 'default nil :font "Hack 10")
(set-face-attribute 'line-number nil :background 'unspecified)
(set-face-attribute 'fringe nil :inherit 'line-number)

;;; Packages
(use-package aggressive-indent
  :demand t
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package avy
  :demand t
  :config
  (defun avy-goto-paren ()
    (interactive)
    (avy--generic-jump ")" nil 'pre))
  (setq avy-timeout-seconds 0.2))

(use-package company
  :demand t
  :init
  (defun org-keyword-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'org-keyword-backend))
      (prefix (and (eq major-mode 'org-mode)
                   (cons (company-grab-line "^#\\+\\(\\w*\\)" 1)
                         t)))
      (candidates (mapcar #'upcase
                          (cl-remove-if-not
                           (lambda (c) (string-prefix-p arg c))
                           (pcomplete-completions))))
      (ignore-case t)
      (duplicates t)))
  :config
  (setq company-idle-delay 0.3
        company-selection-wrap-around t)
  (add-to-list 'company-backends 'org-keyword-backend)
  (global-company-mode))

(use-package counsel
  :demand t)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package elpy
  :after python-mode
  :init
  (with-eval-after-load 'python (elpy-enable))
  :config
  (delete 'elpy-module-highlight-indentation elpy-modules))

(use-package evil
  :demand t
  :init
  (setq evil-want-integration nil
        evil-want-Y-yank-to-eol t
        evil-vsplit-window-right t
        evil-cross-lines t)             ; motions work across newlines
  :config
  (setq evil-search-wrap t
        evil-regexp-search t)
  (setq evil-emacs-state-cursor '("#906cff" box)
        evil-normal-state-cursor '("#91ddff" box)
        evil-motion-state-cursor '("#65b2ff" hollow)
        evil-operator-state-cursor '("#65b2ff" hollow)
        evil-visual-state-cursor '("#ffd75f" box)
        evil-insert-state-cursor '("#62d196" hbar)
        evil-replace-state-cursor '("#ff5458" hbar))
  (evil-mode 1))

(use-package evil-collection
  :demand t
  :after evil
  :init (setq evil-collection-outline-bind-tab-p nil)
  :config (evil-collection-init))

(use-package evil-surround
  :demand t
  :config (global-evil-surround-mode))

(use-package evil-embrace
  :after evil-surround
  :demand t
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-commentary
  :after evil
  :demand t
  :config (evil-commentary-mode))

(use-package eyebrowse
  :demand t
  :config
  (setq eyebrowse-new-workspace t
        eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t)
  (eyebrowse-mode t))

(use-package flycheck
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package general
  :demand t)

(use-package key-chord
  :demand t
  :config (key-chord-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package hydra
  :demand t
  :config
  (defhydra hydra-projectile (:columns 4)
    "Projectile"
    ("f"   projectile-find-file                "Find File")
    ("r"   projectile-recentf                  "Recent Files")
    ("z"   projectile-cache-current-file       "Cache Current File")
    ("x"   projectile-remove-known-project     "Remove Known Project")
    
    ("d"   projectile-find-dir                 "Find Directory")
    ("b"   projectile-switch-to-buffer         "Switch to Buffer")
    ("c"   projectile-invalidate-cache         "Clear Cache")
    ("X"   projectile-cleanup-known-projects   "Cleanup Known Projects")
    
    ("o"   projectile-multi-occur              "Multi Occur")
    ("s"   projectile-switch-project           "Switch Project")
    ("k"   projectile-kill-buffers             "Kill Buffers")
    ("q"   nil "Cancel" :color blue))

  (defhydra hydra-eval (:columns 2)
    "Eval"
    ("b" eval-buffer "buffer" :exit t)
    ("d" eval-defun "defun" :exit t)
    ("l" eval-last-sexp "last" :exit t)
    ("r" eval-region "region" :exit t)
    ("q" nil "nvm" :color blue))

  (defhydra hydra-buffer (:color blue :columns 4)
    "Buffers"
    ("n" next-buffer "next" :color red)
    ("p" previous-buffer "prev" :color red)
    ("b" ivy-switch-buffer "ivy-switch")
    ("B" ibuffer "ibuffer")
    ("N" evil-buffer-new "new")
    ("s" save-buffer "save" :color red)
    ("d" kill-this-buffer "delete" :color red)
    ;; don't come back to previous buffer after delete
    ("D" (progn (kill-this-buffer) (next-buffer)) "Delete" :color red)
    ("w" delete-window "window" :color red))

  (defhydra hydra-org (:color blue :columns 3)
    "Agenda"
    ("A" org-agenda "agenda menu" :color blue)
    ("a" org-agenda-list "agenda" :color blue)
    ("t" org-todo-list "global to do-list" :color blue)
    ("r" org-refile "refile" :color red)
    ("x" org-archive-subtree "archive" :color red))

  (defhydra hydra-toggle (:columns 2)
    "Toggle"
    ("r" rainbow-mode "rainbow-mode")
    ("f" flyspell-mode "flyspell-mode")
    ("p" paredit-mode "paredit")
    ("a" aggressive-indent-mode "aggressive-indent-mode")
    ("q" nil "nothing" :color blue))

  (defhydra hydra-compile (:columns 2)
    "Make"
    ("m" (compile "make") "default")
    ("t" (compile "make tex") "tex")
    ("c" (compile "make clean") "clean"))

  (defhydra hydra-eyebrowse (:hint nil)
    "
_0_    _1_    _2_    _3_     _4_     _5_    _6_    _7_    _8_     _9_
"
    ("0" eyebrowse-switch-to-window-config-0)
    ("1" eyebrowse-switch-to-window-config-1)
    ("2" eyebrowse-switch-to-window-config-2)
    ("3" eyebrowse-switch-to-window-config-3)
    ("4" eyebrowse-switch-to-window-config-4)
    ("5" eyebrowse-switch-to-window-config-5)
    ("6" eyebrowse-switch-to-window-config-6)
    ("7" eyebrowse-switch-to-window-config-7)
    ("8" eyebrowse-switch-to-window-config-8)
    ("9" eyebrowse-switch-to-window-config-9)
    ("n" eyebrowse-create-window-config "new")
    ("c" eyebrowse-close-window-config "close")
    ("<" eyebrowse-prev-window-config "prev")
    (">" eyebrowse-next-window-config " next")
    ("q" nil "quit" :color blue)))


(use-package magit
  :commands (magit-status magit-blame magit-log-buffer-file magit-log-all)
  :config (setq magit-completing-read-function 'ivy-completing-read))

(use-package markdown-mode
  :mode
  ("\\.md" . markdown-mode)
  ("\\.mdpp" . markdown-mode)
  :init
  (setq markdown-command "pandoc")
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode)
  :config
  (font-lock-add-keywords 'markdown-mode
                          '(("@[[:alnum:]]+" . font-lock-keyword-face))))

(use-package projectile
  :demand t
  :config
  (projectile-mode))

(use-package olivetti
  :config (setq-default olivetti-body-width 90))

;;; ORRRRG
(use-package org
  :ensure org-plus-contrib
  :pin org
  :demand t
  :commands (org-capture)
  :init
  (require 'cl)
  (setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
  :config
  (setq org-directory "~/org"
        org-default-notes-file "~/org/todo.org"
        org-agenda-files '("~/org/todo.org")
        org-archive-location "~/org/archief::datetree/"
        org-startup-indented t
        org-hide-leading-stars nil
        org-log-done nil
        org-log-into-drawer nil
        org-return-follows-link t
        org-reverse-note-order t
        org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil)))

  (setq org-refile-targets
        '((nil :maxlevel . 1)
          (org-agenda-files :maxlevel . 1)))

  (setq org-todo-keyword-faces
        '(;("TODO" . "#c991e1")
          ("AFSPRAAK" . "#aaffe4")
          ("BELLEN" . "#aaffe4")
          ("INTAKE" . "#aaffe4")
          ("CANCELED" . "#ff5458")
          ("READ" . "#65b2ff")
          ("IDEE" . "#65b2ff")))

  ;; (set-face-attribute 'org-level-1 nil :foreground "#87ffff")
  ;; (set-face-attribute 'org-level-2 nil :foreground "#87d7ff")
  ;; (set-face-attribute 'org-level-3 nil :foreground "#5fffaf")
  ;; (set-face-attribute 'org-level-4 nil :foreground "#87ffff")
  ;; (set-face-attribute 'org-level-5 nil :foreground "#87d7ff")
  ;; (set-face-attribute 'org-level-6 nil :foreground "#5fffaf")

  (setq org-capture-templates
        '(("w" "word" entry (file+headline "~/org/dict.org" "Words") "* %? :: ")
          ("W" "usage" entry (file+headline "~/org/dict.org" "Usage") "* %? :: ")
          ("t" "todo" entry (file+headline "~/org/todo.org" "Needs") "* TODO %?")
          ("l" "link" entry (file+headline "~/org/todo.org" "Needs") "* READ [[%?][]]")
          ("n" "note" entry (file+headline "~/org/todo.org" "Notes") "* %?")
          ("s" "scriptie note" entry (file+headline "~/projects/thesis/todo.org" "Notes") "* %?")
          ("S" "scriptie todo" entry (file+headline "~/projects/thesis/todo.org" "To Do") "* TODO %?"))))

;; global org-capture

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

;; make the frame contain a single window. by default org-capture
;; splits the window.
(add-hook 'org-capture-mode-hook
          'delete-other-windows)

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defun make-capture-frame ()
  "Create a new frame and run 'org-capture'."
  (interactive)
  (make-frame '((name . "capture")
                (width . 120)
                (height . 15)))
  (select-frame-by-name "capture")
  (setq word-wrap 1)
  (setq truncate-lines nil)
  (org-capture))

(use-package org-pdfview
  :demand t
  :after org
  :config
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open link)))))

;;; And on we go:

(use-package paredit
  :demand t
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'racket-mode-hook 'paredit-mode))

(use-package pdf-tools
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t))

(use-package python-mode
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

(use-package racket-mode)

(use-package rainbow-delimiters
  :demand t)

(use-package rainbow-mode)

(use-package rust-mode)

(use-package shackle
  :demand t
  :config
  (setq shackle-select-reused-windows nil) ; default nil
  (setq shackle-default-alignment 'below) ; default below
  (setq shackle-default-size 0.3) ; default 0.5
  (setq shackle-default-rule '(:select t)
        shackle-rules '((compilation-mode :noselect t)))
  ;; '(:align below :inhibit-window-quit nil :select t :modeline nil)
  (shackle-mode 1))

(use-package smart-mode-line
  :demand t
  :config
  (line-number-mode t)
  (column-number-mode t)
  (setq sml/theme 'respectful)
  (setq sml/modified-char "+")
  (setq sml/mode-width 'full)
  (setq rm-whitelist (format "^ \\(%s\\)$"
                             (mapconcat #'identity
                                        '("=>" "Paredit" "Elpy")
                                        "\\|")))
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/thesis" ":TH:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/" ":PRJ:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/dotfiles" ":.F:") t)
  (sml/setup))

(use-package which-key
  :demand t
  :config (which-key-mode 1))

(use-package yaml-mode
  :mode
  ("\\.yml" . yaml-mode)
  ("\\.yaml" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'delete-trailing-whitespace))

(use-package yasnippet
  :demand t
  :config
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))
  (yas-global-mode 1))


;;; Utility functions
(defun pfn-cycle-themes ()
  "Cycle through available themes."
  (interactive)
  (let* ((themes [challenger-deep nord])
         (idx-before (if (get 'pfn-cycle-themes 'state)
                         (get 'pfn-cycle-themes 'state) 0))
         (idx-after (% (+ idx-before (length themes) 1) (length themes)))
         (prev (aref themes idx-before))
         (next (aref themes idx-after)))
    (put 'pfn-cycle-themes 'state idx-after)
    (disable-theme prev)
    (load-theme next t)
    (set-face-attribute 'line-number nil :background 'unspecified)
    (set-face-attribute 'fringe nil :inherit 'line-number)))

(defun pfn-revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (message "buffer reloaded"))

;;; Hooks
(defun pfn-setup-prog-mode ()
  "Load 'prog-mode' minor modes."
  (eldoc-mode 1)
  (auto-fill-mode)
  (rainbow-delimiters-mode)
  (display-line-numbers-mode)
  (delete-trailing-whitespace)
  (flycheck-mode 1)
  (outline-minor-mode))
(add-hook 'prog-mode-hook 'pfn-setup-prog-mode)

(defun pfn-setup-text-mode ()
  "Load 'text-mode' hooks."
  (turn-on-auto-fill)
  (rainbow-delimiters-mode 1)
  (abbrev-mode 1))
(add-hook 'text-mode-hook 'pfn-setup-text-mode)

(add-hook 'focus-out-hook 'garbage-collect)

;;; Editor Settings
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
 vc-follow-symlinks t                         ; so you end up at the file itself rather than editing the link
 large-file-warning-threshold nil)

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-subword-mode 1)

(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(setq display-line-numbers-width 4
      display-line-numbers-width-start 3
      display-line-numbers-widen t)

(setq ispell-silently-savep t
      ispell-dictionary "english"
      ispell-extra-args '("-a" "utf-8"))

;;; Custom Keys
(general-evil-setup)

(general-create-definer evil-leader
  :states '(normal visual insert emacs)
  :prefix ","
  :non-normal-prefix "M-,")

(evil-leader
  "b" 'hydra-buffer/body
  "d" 'dired-jump-other-window
  "e" 'hydra-eval/body
  "i" '(lambda () (interactive)
         (find-file user-init-file))
  "gs"'magit-status
  "m" 'counsel-bookmark
  "o" 'olivetti-mode
  "p" 'counsel-yank-pop
  "q" 'delete-window
  "r" 'hydra-compile/body
  "t" 'hydra-toggle/body
  "w" 'hydra-eyebrowse/body
  "x" 'counsel-M-x
  "." 'mode-line-other-buffer
  "," 'other-window
  ":" 'counsel-find-file)

(general-omap
  "C-a" 'avy-goto-word-or-subword-1
  "C-o" 'evil-avy-goto-subword-0
  "C-e" 'evil-avy-goto-line
  "C-u" 'evil-avy-goto-char-timer)

(general-mmap
  :keymaps 'org-mode-map
  "<ret>" 'org-open-at-point
  "RET"   'org-open-at-point
  "<tab>" 'org-cycle
  "TAB"   'org-cycle)

(general-mmap
  "j"       'evil-next-visual-line
  "k"       'evil-previous-visual-line
  "<tab>"   'outline-toggle-children
  "TAB"     'outline-toggle-children
  "C-e"     'evil-end-of-line)

(general-def :keymaps 'evil-insert-state-map
  (general-chord "jj") 'evil-normal-state)

(general-def :keymaps 'evil-window-map
  "C-n" 'evil-window-vnew)

(general-def :keymaps 'org-agenda-mode-map
  "C-w C-W" 'other-window)

(general-def
  "C-c a"   'hydra-org/body
  "C-c b"   'mode-line-other-buffer
  "C-c c"   'org-capture
  "C-c f"   'hydra-projectile/body
  "C-c k"   'counsel-ag
  "C-c l"   'org-store-link
  "C-c R"   '(lambda () (interactive)
               (load-file user-init-file))
  "C-c r"   '(lambda () (interactive)
               (revert-buffer :ignore-auto :noconfirm)
               (message "buffer reloaded"))
  "C-c w"   'hydra-eyebrowse/body
  "C-s"     'swiper
  "M-/"     'hippie-expand
  "<M-tab>" 'company-complete-common-or-cycle)

;; completion
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;;; starting up
(setq initial-buffer-choice "~/org/todo.org")
(put 'dired-find-alternate-file 'disabled nil)

;; lower garbace collection threshold
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;;; init.el ends here
