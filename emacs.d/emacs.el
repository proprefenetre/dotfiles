(setq user-full-name "Niels Eigenraam"
      user-mail-address "nielseigenraam@gmail.com")

(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t
        package-enable-at-startup nil)

  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t)))

(setq use-package-always-defer t
      use-package-verbose t)

(eval-and-compile
  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(eval-when-compile
  (require 'package)

  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

  ;; (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package no-littering
    :config
    (setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
    (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package company
  :demand t
  :config
  (setq company-idle-delay 0.3
        company-selection-wrap-around t)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package counsel-projectile)
(use-package counsel
  :demand t)



(use-package evil
  :demand t
  :init
  (setq evil-want-integration nil)	; required by evil-collection
  (setq evil-want-Y-yank-to-eol t)	; Y == y$
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-outline-bind-tab-p nil)
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :demand t
  :config
  (global-evil-surround-mode))

(use-package evil-embrace
  :after evil
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (evil-embrace-enable-evil-surround-integration))

(use-package evil-commentary
  :after evil
  :demand t
  :config
  (evil-commentary-mode))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package eyebrowse
  :demand
  :config
  (setq eyebrowse-new-workspace t
        eyebrowse-wrap-around t
        eyebrowse-switch-back-and-forth t)
  (eyebrowse-mode t))

(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package general
  :demand t
  :config
  (general-create-definer evil-leader-def
    :states '(normal visual insert emacs)
    :prefix ","
    :non-normal-prefix "M-,")

  (evil-leader-def
    "t"  'hydra-toggle/body
    ":"  'counsel-find-file
    "e"  'eval-defun
    ;; "i"  'pfn-open-init-file
    "o"  'olivetti-mode
    ","  'other-window
    "."  'mode-line-other-buffer
    "b"  'hydra-buffer/body
    "q"  'kill-buffer-and-window
    "w"  'save-buffer
    "x"  'counsel-M-x
    "p"  'counsel-yank-pop
    "m"  'counsel-bookmark)

  (general-define-key
    :keymaps 'evil-insert-state-map
    (general-chord "jj") 'evil-normal-state
    "C-e" 'end-of-line
    "C-a" 'beginning-of-line
    "<M-tab>" 'company-complete-common-or-cycle)

  (general-evil-setup)

  (general-mmap
    "j"       'evil-next-visual-line
    "k"       'evil-previous-visual-line
    "-"       'dired-jump
    "_"       'counsel-recentf
    "C-e"     'evil-end-of-line
    ;; "C-w C-v" 'pfn-vsplit-new-buffer
    ;; "C-w C-h" 'pfn-hsplit-new-buffer
    "/"       'swiper)

  (general-nmap
    "C-c R" 'pfn-reload-init
    "C-c r" 'pfn-revert-buffer-no-confirm
    "C-c b" 'mode-line-other-buffer
    "C-c k" 'counsel-ag
    "C-c C-f" 'counsel-find-file
    "C-c a" 'hydra-org/body
    "M-/"   'hippie-expand
    "C-c l" 'org-store-link
    "C-c c" 'org-capture))

(use-package hydra
  :demand t
  :config
  (defhydra hydra-buffer (:color blue :columns 3)
    " Buffers: "
    ("n" next-buffer "next" :color red)
    ("p" previous-buffer "prev" :color red)
    ("b" ivy-switch-buffer "ivy-switch")
    ("B" ibuffer "ibuffer")
    ("N" evil-buffer-new "new")
    ("s" save-buffer "save" :color red)
    ("d" kill-this-buffer "delete" :color red)
    ;; don't come back to previous buffer after delete
    ("D" (progn (kill-this-buffer) (next-buffer)) "Delete" :color red))

  (defhydra hydra-org (:color blue :columns 3)
    " AGENDA: "
    ("A" org-agenda "agenda menu" :color blue)
    ("a" org-agenda-list "agenda" :color blue)
    ("t" org-todo-list "global to do-list" :color blue)
    ("r" org-refile "refile" :color red)
    ("x" org-archive-subtree "archive" :color red))

  (defhydra hydra-toggle (:color blue :columns 3)
    " Toggle: "
    ("r" rainbow-mode "rainbow-mode" :color blue)
    ("f" flyspell-mode "flyspell-mode" :color red)
    ("p" paredit-mode "paredit" :color blue)
    ("a" aggressive-indent-mode "aggressive-indent-mode" :color red)))

(use-package ivy
  :demand t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil))

(use-package key-chord
  :demand t
  :config
  (key-chord-mode 1))

(use-package magit
  :commands (magit-status magit-blame magit-log-buffer-file magit-log-all))

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

(use-package olivetti
  :config
  (setq-default olivetti-body-width 90))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :defer t
  :commands (org-capture)
  :config
  (setq org-directory "~/org"
        org-default-notes-file "~/org/werk.org"
        org-agenda-files '("~/org/werk.org")
        org-archive-location "~/org/archief::datetree/"
        org-log-done nil
        org-log-into-drawer t
        org-cycle-separator-lines 2
        outline-blank-line t            ; newlines are not content
        org-level-color-stars-only t
        org-return-follows-link t
        org-tags-column -80
        org-reverse-note-order t))      ; enter new note as first item

;; Ensure ELPA org is prioritized above built-in org.
(require 'cl)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

(defun pfn-org-level-sizes ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (interactive)
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :height 1.0)))

(defun pfn-org-level-colors ()
  "Taste the rainbow!"
  (interactive)
  (set-face-attribute 'org-level-1 nil :foreground "#87ffff")
  (set-face-attribute 'org-level-2 nil :foreground "#87d7ff")
  (set-face-attribute 'org-level-3 nil :foreground "#5fffaf")
  (set-face-attribute 'org-level-4 nil :foreground "#87ffff")
  (set-face-attribute 'org-level-5 nil :foreground "#87d7ff")
  (set-face-attribute 'org-level-6 nil :foreground "#5fffaf"))

(eval-after-load "org" '(progn (pfn-org-level-colors)
                               (pfn-org-level-sizes)))

(defun pfn-org-checkbox-todo ()
  "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise."
  (let ((todo-state (org-get-todo-state)) beg end)
    (unless (not todo-state)
      (save-excursion
    (org-back-to-heading t)
    (setq beg (point))
    (end-of-line)
    (setq end (point))
    (goto-char beg)
    (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                   end t)
        (if (match-end 1)
        (if (equal (match-string 1) "100%")
            (unless (string-equal todo-state "DONE")
              (org-todo 'done))
          (unless (string-equal todo-state "TODO")
            (org-todo 'todo)))
          (if (and (> (match-end 2) (match-beginning 2))
               (equal (match-string 2) (match-string 3)))
          (unless (string-equal todo-state "DONE")
            (org-todo 'done))
        (unless (string-equal todo-state "TODO")
          (org-todo 'todo)))))))))
(add-hook 'org-checkbox-statistics-hook 'pfn-org-checkbox-todo)

(setq org-todo-keyword-faces
      '(("TODO" . "#c991e1")
        ("AFSPRAAK" . "#aaffe4")
        ("BELLEN" . "#aaffe4")
        ("INTAKE" . "#aaffe4")
        ("CANCELED" . "#ff5458")
        ("TOREAD" . "#65b2ff")
        ("IDEE" . "#65b2ff")))

(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)))

(setq org-capture-templates
      '(("w" "word" entry (file+headline "~/org/dict.org" "Words") "* %? :: ")
        ("W" "usage" entry (file+headline "~/org/dict.org" "Usage") "* %? :: ")
        ("t" "todo" entry (file+headline "~/org/todo.org" "To Do") "* TODO %?")
        ("l" "link" entry (file+headline "~/org/todo.org" "To Do") "* READ [[%?][]]")
        ("n" "note" entry (file+headline "~/org/todo.org" "Notes") "* %?")))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package projectile
  :demand t)

(use-package rainbow-delimiters
  :ensure t
  :demand t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package swiper
  :commands (swiper swiper-all))

(setq custom-safe-themes t)

(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  (line-number-mode t)
  (column-number-mode t)

  (setq sml/theme 'respectful
        sml/shorten-directory t
        sml/name-width 40
        sml/mode-width 'right)
  (sml/setup))

(use-package minions
  :ensure t
  :demand t
  :config
  (minions-mode 1))

(use-package challenger-deep-theme)
(use-package nord-theme)
(use-package solarized-theme)

(use-package toc-org
  :after org
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package rainbow-mode)

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))

(use-package yaml-mode
  :mode
  ("\\.yml" . yaml-mode)
  ("\\.yaml" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook 'delete-trailing-whitespace))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (with-eval-after-load 'warnings
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))
  (yas-global-mode 1))

(load-theme 'challenger-deep t)

(set-face-attribute 'default nil :font "Hack 10" )
(fringe-mode '(8 . 8))
(set-face-attribute 'fringe nil :background nil)

;;; native line numbers
(setq display-line-numbers-width 4
      display-line-numbers-width-start 3
      display-line-numbers-widen t)
(set-face-attribute 'line-number nil :background 'unspecified)

(setq-default
 confirm-kill-emacs 'yes-or-no-p             ; Confirm before exiting Emacs
 help-window-select t                        ; Focus new help windows when opened
 indent-tabs-mode nil                        ; Stop using tabs to indent
 tab-width 4                                 ; Set width for tabs
 fill-column 80                              ; Set width for automatic line breaks
 visual-bell nil                             ; plz no visual bell
 ring-bell-function 'ignore
 mouse-yank-at-point t                       ; Yank at point rather than pointer
 scroll-conservatively most-positive-fixnum  ; Always scroll by one line
 indicate-empty-lines nil                    ; no fuzz at the end of a file
 vc-follow-symlinks t)	               ; so you end up at the file itself 
                                             ;rather than editing the link
(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)                ; Replace yes/no prompts with y/n

(scroll-bar-mode -1)                         ; Disable the scroll bar
(tool-bar-mode -1)                           ; Disable the tool bar
(tooltip-mode -1)                            ; Disable the tooltips
(menu-bar-mode -1)                           ; Disable the menu bar

(put 'dired-find-alternate-file 'disabled nil)

(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

(setq
 ispell-silently-savep t
 ispell-dictionary "dutch"
 ispell-extra-args '("-a" "utf-8"))

(setq sentence-end-double-space nil)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
	try-complete-file-name
	try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill))

(defun pfn-text-mode-hooks ()
  "Load 'text-mode' hooks."
  (turn-on-auto-fill)
  (rainbow-delimiters-mode 1)
  (abbrev-mode 1))
(add-hook 'text-mode-hook 'pfn-text-mode-hooks)

(defun pfn-setup-prog-mode ()
  "Load 'prog-mode' minor modes."
  (auto-fill-mode)
  (rainbow-delimiters-mode)
  (display-line-numbers-mode)
  (delete-trailing-whitespace)
  (flycheck-mode 1))
(add-hook 'prog-mode-hook 'pfn-setup-prog-mode)

(defun pfn-setup-lisp-mode ()
  "Setup lisp-modes such as racket and emacs-lisp."
  (interactive)
  (eldoc-mode 1)
  (paredit-mode)
  (aggressive-indent-mode))
(add-hook 'emacs-lisp-mode 'pfn-setup-lisp-mode)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'focus-out-hook #'garbage-collect)

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
