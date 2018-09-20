;;; init.el --- Emacs Configuration
;;; Commentary:
;;; Sunday iteration. After dicking around with literate org files for days.
;;; Code:

;;; Initialization
;; Personal settngs
(setq user-full-name "Niels Eigenraam"
      user-mail-address "nielseigenraam@gmail.com")

;; personal lisp collection
(add-to-list 'load-path "~/.emacs.d/lisp")

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

;;; (package-initialize)                    ; emacs 27+ does this for you. TIL

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
  (add-hook 'TeX-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package avy
  :demand t
  :config
  (defun pfn-avy-goto-paren ()
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
  (setq company-idle-delay 0
        company-selection-wrap-around t)
  (add-to-list 'company-backends 'org-keyword-backend)
  (add-hook 'nxml-mode 'company-mode))

(use-package counsel
  :demand t)

(use-package counsel-projectile
  :demand t
  :config
  (counsel-projectile-mode))

(use-package dashboard
  :demand t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5))))

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (setq evil-search-wrap t
        evil-regexp-search t
        evil-complete-next-func 'hippie-expand
        evil-want-Y-yank-to-eol t
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-cross-lines t)             ; motions work across newlines

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
  :config
  (push '(?* . ("*" . "*")) evil-surround-pairs-alist)
  (push '(?_ . ("_" . "_")) evil-surround-pairs-alist)
  (push '(?/ . ("/" . "/")) evil-surround-pairs-alist)
  (global-evil-surround-mode))

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
  :delight " Fly"
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

  (defhydra hydra-projectile (:columns 2)
    "Projectile"
    ("a"   counsel-projectile                  "Jump")
    ("o"   counsel-recentf                  "Recent Files")
    ("e"   counsel-projectile-find-file        "Find File")
    ("h"   counsel-projectile-switch-to-buffer "Switch to Buffer")
    ("p"   counsel-projectile-ag               "Counsel-ag")
    ("."   counsel-projectile-switch-project   "Switch Project")
    (","   projectile-kill-buffers             "Kill Buffers")
    ("q"   nil "Quit" :color blue))

  (defhydra hydra-eval (:columns 2 :color blue)
    "Eval"
    ("b" eval-buffer "buffer")
    ("d" eval-defun "defun")
    ("l" eval-last-sexp "last")
    ("r" eval-region "region")
    ("q" nil "nvm"))

  (defhydra hydra-buffer(:color blue :hint nil)
    "
  ^Buffer^        ^kill^        ^list^
  ^^^^^^^-----------------------------------
  _<_: previous   _b_: buffer   _i_: ivy
  _>_: next       _w_: window   _B_: iBuffer
  _n_: new        _W_: both     _d_: dired
  _s_: save       _E_: Emacs    _D_: dired-other-window
  _q_: nvm
"
    ("<" previous-buffer :color red)
    (">" next-buffer :color red)
    ("n" evil-buffer-new)
    ("s" evil-save)
    ("b" kill-buffer :color red)
    ("w" evil-window-delete :color red)
    ("W" kill-buffer-and-window :color red)
    ("E" save-buffers-kill-emacs)
    ("i" ivy-switch-buffer)
    ("B" ibuffer-other-window)
    ("D" dired-other-window)
    ("d" dired)
    ("q" nil nil))

  (defhydra hydra-todo (:columns 4 :color red)
    "Todos"
    ("t" (org-todo) "TODO")
    ("d" (org-todo 'done) "DONE")
    ("b" (org-todo "BEZIG") "BEZIG")
    ("w" (org-todo "WAITING") "WAITING")
    ("f" (org-todo "FEEDBACK") "FEEDBACK")
    ("a" (org-todo "AFSPRAAK") "AFSPRAAK")
    ("v" (org-todo "VERPLAATST") "VERPLAATST")
    ("c" (org-todo "CANCELED") "CANCELED")
    ("SPC" (org-todo 'none) "clear")
    ("T" (find-file "~/org/todo.org") "todo.org")
    ("N" (find-file "~/org/notes.org") "notes.org")

    ("r" org-refile "refile" :color blue)
    ("x" org-archive-subtree "archive" :color blue)
    ("q" nil "nvm" :color blue)
    ("A" org-agenda "agenda" :color blue))

  (defvar rainbow-mode nil)
  (defvar orgtbl-mode nil)
  (defhydra hydra-toggle (:color pink)
    "

  ^Toggle^                             ^State^
  ^─^──────────────────────────────────^─────^
  _a_ aggressive-indent-mode:          [%`aggressive-indent-mode]
  _b_ abbrev-mode:                     [%`abbrev-mode]
  _c_ rainbow-mode:                    [%`rainbow-mode]
  _d_ flyspell:                        [%`flyspell-mode]
  _D_ flycheck:                        [%`flycheck-mode]
  _e_ paredit:                         [%`paredit-mode]
  _f_ visual-line-mode:                [%`visual-line-mode]
  _g_ orgtbl-mode                      [%`orgtbl-mode]
  _h_ display-line-numbers-mode        [%`display-line-numbers-mode]
  _q_ nvm.
  ^─^──────────────────────────────────^─────^
"
    ("a" aggressive-indent-mode nil)
    ("b" abbrev-mode nil)
    ("c" rainbow-mode nil)
    ("d" flyspell-mode nil)
    ("D" flycheck-mode nil)
    ("e" paredit-mode nil)
    ("f" visual-line-mode nil)
    ("g" orgtbl-mode nil)
    ("h" display-line-numbers-mode nil)
    ("q" nil nil :color blue))


  (defhydra hydra-compile (:columns 2 :color blue)
    "Make"
    ("m" (compile "make") "default")
    ("t" (compile "make tex") "tex")
    ("c" (compile "make clean") "clean")
    ("i" (compile "make install") "install"))


  (defhydra hydra-eyebrowse (:color blue)
    "
  ^Workspaces^
  ^^^^^^^^-----------------------
  _0_  _1_  _2_    _c_lose
  _3_  _4_  _5_    _i_nit.el
  _6_  _7_  _8_    _p_ackage
  _<_  _9_  _>_    _n_ew
  ^^^^^^           _q_uit
"
    ("0" eyebrowse-switch-to-window-config-0 nil)
    ("1" eyebrowse-switch-to-window-config-1 nil)
    ("2" eyebrowse-switch-to-window-config-2 nil)
    ("3" eyebrowse-switch-to-window-config-3 nil)
    ("4" eyebrowse-switch-to-window-config-4 nil)
    ("5" eyebrowse-switch-to-window-config-5 nil)
    ("6" eyebrowse-switch-to-window-config-6 nil)
    ("7" eyebrowse-switch-to-window-config-7 nil)
    ("8" eyebrowse-switch-to-window-config-8 nil)
    ("9" eyebrowse-switch-to-window-config-9 nil)
    ("n" eyebrowse-create-window-config nil)
    ("i" pfn-eyebrowse-open-init nil)
    ("p" pfn-eyebrowse-open-package nil)
    ("c" eyebrowse-close-window-config nil)
    ("<" eyebrowse-prev-window-config nil)
    (">" eyebrowse-next-window-config nil)
    ("q" nil nil)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

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
                          '(("@[[:alnum:]]+\\(-[[:alnum:]]+\\)?" . font-lock-keyword-face))))

(use-package prescient
  :demand t
  :config
  (ivy-prescient-mode)
  (company-prescient-mode)
  (prescient-persist-mode))

(use-package projectile
  :demand t
  :config
  (setq projectile-mode-line '(:eval (format " PRJ:%s" (projectile-project-name)))
        projectile-completion-system 'ivy)
  (projectile-mode))

(use-package olivetti
  :config (setq-default olivetti-body-width 90))


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
        org-agenda-files '("~/org/todo.org"
                           "~/org/notes.org"
                           "~/org/foxy.org")
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-archive-location "~/org/archief::datetree/"
        org-startup-indented t
        org-log-done nil
        org-log-into-drawer t
        org-return-follows-link t
        org-reverse-note-order t
        org-M-RET-may-split-line nil
        org-outline-path-complete-in-steps nil         ; Refile in a single go
        org-refile-allow-creating-parent-nodes t
        org-refile-use-outline-path t
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil)))

  (setq org-todo-keywords '((sequence "TODO" "WAITING" "BEZIG" "|" "DONE" "CANCELED")
                            (sequence "AFSPRAAK" "VERPLAATST" "|" "DONE" "AFGEZEGD")
                            (type "|" "READ" "IDEE")))

  (setq org-todo-keyword-faces
        '(("AFSPRAAK" . "#aaffe4")
          ("BELLEN" . "#aaffe4")
          ("CANCELED" . "#ff5458")
          ("READ" . "#65b2ff")
          ("IDEE" . "#65b2ff")))

  (setq org-capture-templates
        '(("w" "word" entry (file+headline "~/org/dict.org" "Words") "* %? :: ")
          ("W" "terminologie" entry (file+headline "~/org/dict.org" "Terminologie") "* %? ")
          ("t" "todo" entry (file+headline "~/org/todo.org" "To do") "* TODO %?")
          ("l" "link" entry (file+headline "~/org/todo.org" "To do") "* [[%?][]]")
          ("n" "note" entry (file+headline "~/org/todo.org" "Notes") "* %?")
          ("s" "scriptie note" entry (file+headline "~/projects/thesis/todo.org" "Notes") "* %?")
          ("S" "scriptie todo" entry (file+headline "~/projects/thesis/todo.org" "To Do") "* TODO %?")))

  (add-to-list 'org-structure-template-alist '("ll" "#+BEGIN_LATEX latex\n?\n#+END_LATEX")))

(use-package org-pdfview
  :demand t
  :after org
  :config
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open link)))))

(use-package paredit
  :demand t
  :delight " ()"
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

;; (use-package python-mode
;;   :config
;;   (setq python-shell-interpreter "ipython"
;;         python-shell-interpreter-args "-i --simple-prompt"))

(use-package racket-mode)

(use-package rainbow-delimiters
  :demand t)

(use-package rainbow-mode)

(use-package rust-mode)

(use-package s)

(use-package shackle
  :demand t
  :config
  (setq shackle-select-reused-windows nil) ; default nil
  (setq shackle-default-alignment 'below) ; default below
  (setq shackle-default-size 0.3) ; default 0.5
  (setq shackle-default-rule '(:select t))
  (setq shackle-rules '(("\\`\\*edit-indirect .*\\*\\'" :regexp t :same t)
                        ("\\`\\*Man .*\\*\\'" :regexp t :same t)))
  (shackle-mode 1))

(use-package smart-mode-line
  :demand t
  :config
  (line-number-mode t)
  (column-number-mode t)
  (setq sml/theme 'respectful)
  (setq sml/modified-char "+")
  (setq sml/name-width 40)
  (setq sml/shorten-modes nil)
  (setq sml/mode-width 0)
  (setq rm-whitelist
        (format "^ \\(%s\\)$"
                (mapconcat #'identity
                           '("()" "Fly" "PRJ.*" "=>")
                           "\\|")))
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/thesis" ":TH:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/" ":PRJ:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/dotfiles" ":DF:"))
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
  (setq yas-triggers-in-field t
        yas-snippet-revival t
        yas-indent-line nil)
  (yas-global-mode 1))


;;; Hooks
(defun pfn-setup-prog-mode ()
  "Load 'prog-mode' minor modes."
  (eldoc-mode 1)
  (auto-fill-mode)
  (rainbow-delimiters-mode)
  (display-line-numbers-mode)
  (delete-trailing-whitespace)
  (flycheck-mode 1)
  (outline-minor-mode)
  (company-mode-on)
  (abbrev-mode 1))
(add-hook 'prog-mode-hook 'pfn-setup-prog-mode)

(defun pfn-setup-text-mode ()
  "Load 'text-mode' hooks."
  (delete-trailing-whitespace)
  (turn-on-auto-fill)
  (rainbow-delimiters-mode 1)
  (abbrev-mode 1))
(add-hook 'text-mode-hook 'pfn-setup-text-mode)

(add-hook 'focus-out-hook 'garbage-collect)

;;; Editor Settings

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(setq-default
 auto-window-vscroll nil         ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p ; Confirm before exiting Emacs
 cursor-in-non-selected-windows t ; Hide the cursor in inactive windows
 help-window-select t             ; Focus new help windows when opened
 indent-tabs-mode nil      ; Stop using tabs to indent
 tab-width 4               ; Set width for tabs
 fill-column 80            ; Set width for automatic line breaks
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
 large-file-warning-threshold nil
 tab-always-indent 'complete
 bookmark-default-file "~/dotfiles/emacs.d/var/bookmarks"
 bookmark-save-flag 1)


(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-input-method "latin-postfix")

(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-subword-mode 1)

(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(put 'narrow-to-region 'disabled nil)             ; Enable narrowing

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode 1)

(setq display-line-numbers-width 4
      display-line-numbers-width-start 3
      display-line-numbers-widen t)

(setq ispell-silently-savep t
      ispell-dictionary "english"
      ispell-extra-args '("-a" "utf-8"))

;;; tramp
(setq tramp-default-method "ssh")

;;; Custom Keys

(general-evil-setup)

(general-create-definer evil-leader
  :states '(normal visual insert emacs)
  :prefix ","
  :non-normal-prefix "M-,")

(evil-leader
  "b" 'mode-line-other-buffer
  ;; "c"
  "d" 'dired-jump
  ;;"e"
  "i" '(lambda () (interactive)
         (find-file user-init-file))
  ;; "m"
  "o" 'olivetti-mode
  "p" 'counsel-yank-pop
  "q" 'delete-window
  "r" '(lambda () (interactive)
         (revert-buffer :ignore-auto :noconfirm))
  "R" '(lambda () (interactive)
         (load-file user-init-file)
         (message "buffer reloaded"))
  "s" 'magit-status
  ;; "t"
  "," 'other-window
  "-" 'counsel-find-file)

(general-omap                           ; operator map
  "C-a" 'avy-goto-word-or-subword-1
  "C-o" 'evil-avy-goto-subword-0
  "C-e" 'evil-avy-goto-line
  "C-u" 'evil-avy-goto-char-timer)

(general-imap
  :keymaps 'python-mode-map
  "C-<right>" 'sp-forward-slurp-sexp
  "C-<left>" 'sp-backward-slurp-sexp
  "M-<right>" 'sp-forward-barf-sexp
  "M-<left>" 'sp-backward-barf-sexp)

(general-mmap
  :keymaps 'org-mode-map
  "<ret>" 'org-open-at-point
  "RET"   'org-open-at-point
  "C-c 1" 'hydra-table/body)

(general-mmap
  "j"   'evil-next-visual-line
  "k"   'evil-previous-visual-line
  "C-e" 'evil-end-of-line
  "[ p" 'evil-paste-before
  "] p" 'evil-paste-after)

(general-def
  :states 'normal
  :keymaps 'image-mode-map
  "," nil)

(general-def :keymaps 'evil-insert-state-map
  (general-chord "jj") 'evil-normal-state
  "TAB" 'company-complete-common-or-cycle)

(general-def :keymaps 'evil-window-map
  "N" 'evil-window-vsplit)

(general-def :keymaps 'org-agenda-mode-map
  "C-w C-W" 'other-window)

(general-def :keymaps 'markdown-mode-map
  "C-c l" 'pfn-edit-indirect-latex)

(general-def
  ;; "C-c a"
  "C-c b"   'hydra-buffer/body
  "C-c c"   'org-capture
  "C-c d"   'hydra-toggle/body
  "C-c e"   'hydra-eval/body
  "C-c f"   'ffap
  "C-c g"   'hydra-projectile/body
  ;; "C-c h"
  ;; "C-c i"
  ;; "C-c j"
  "C-c k"   'counsel-ag
  "C-c l"   'org-store-link
  "C-c m"   'hydra-compile/body
  ;; "C-c n"
  ;; "C-c o"
  ;; "C-c p"
  ;; "C-c q"
  "C-c R"   '(lambda () (interactive)
               (load-file user-init-file))
  ;; "C-c s"
  "C-c t"   'hydra-todo/body
  "C-c u"   'evil-avy-goto-char-timer
  ;; "C-c v"
  "C-c w"   'hydra-eyebrowse/body
  "C-c x"   'org-archive-subtree
  ;; "C-c y"
  ;; "C-c z"
  "C-s"     'swiper
  "C-c )"   'pfn-avy-goto-paren
  ;; "M-/"     'hippie-expand
  ;; "<M-tab>" 'company-complete-common-or-cycle
  )

;;; starting up
(require 'custom-functions)

(put 'dired-find-alternate-file 'disabled nil)

;; lower garbace collection threshold
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;;; init.el ends here
