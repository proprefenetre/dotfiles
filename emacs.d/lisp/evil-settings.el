;;; evil-settings.el -- evil config
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration nil)
  :config

  (setq evil-search-wrap t
        evil-regexp-search t)

  ;; Use Emacs state in these additional modes.
  ;; (dolist (mode '(dired-mode
  ;;                 docview-mode
  ;;                 eshell-mode
  ;;                 term-mode))
  ;;   (add-to-list 'evil-emacs-state-modes mode))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; Global bindings.
  ;;; normal
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

  (evil-mode))

(use-package evil-collection
  :ensure t
  :demand t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-leader
  :ensure t
  :demand t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
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
    "y"  'counsel-yank-pop))

(use-package evil-surround
  :ensure t
  :demand t
  :config
  (global-evil-surround-mode))

(use-package evil-commentary
  :ensure t
  :demand t
  :config
  (evil-commentary-mode))

(use-package key-chord
  :ensure t
  :demand t
  :config
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "gs" 'magit-status)
  (key-chord-mode 1))

(provide 'evil-settings)
