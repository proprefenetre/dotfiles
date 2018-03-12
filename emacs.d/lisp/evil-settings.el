;;; evil-settings.el -- evil config

(defun pfn/config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "i"  'pfn/open-init-file
    "o"  'olivetti-mode
    ","  'other-window
    "."  'mode-line-other-buffer
    ":"  'eval-expression
    "b"  'helm-mini
    "q"  'kill-this-buffer
    "p"  'helm-show-kill-ring
    "w"  'save-buffer
    "x"	 'helm-M-x
    "y"  'yank-to-x-clipboard))


(defun pfn/config-evil-surround ()
  (add-hook 'markdown-mode-hook (lambda ()
                             (push '(?* . ("*" . "*"))
                                   evil-surround-pairs-alist)))
  (add-hook 'markdown-mode-hook (lambda ()
                             (push '(?/ . ("/" . "/"))
                                   evil-surround-pairs-alist)))
  (add-hook 'markdown-mode-hook (lambda ()
                             (push '(?_ . ("_" . "_"))
                                   evil-surround-pairs-alist))))

(defun pfn/config-evil ()
  "Configure evil mode."

  (setq evil-search-wrap t
      evil-regexp-search t)

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(dired-mode
                  eshell-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (delete 'term-mode evil-insert-state-modes) ; why?
  (delete 'eshell-mode evil-insert-state-modes) ; why?

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; Global bindings.
  ;;; normal
  (evil-define-key 'normal global-map (kbd "j")       'evil-next-visual-line)
  (evil-define-key 'normal global-map (kbd "k")       'evil-previous-visual-line)
  (evil-define-key 'normal global-map (kbd "-")       'helm-find-files)
  (evil-define-key 'normal global-map (kbd "C-e")     'end-of-line)
  ;;; insert
  (evil-define-key 'insert global-map (kbd "C-e")     'end-of-line)

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


  (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ; stop killing emacs with :q
  (defun pfn/ex-kill-buffer-and-close ()
    (interactive)
    (unless (char-equal (elt (buffer-name) 0) ?*)
      (kill-this-buffer)))

  (defun pfn/ex-save-kill-buffer-and-close ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))

  (evil-ex-define-cmd "q[uit]" 'pfn/ex-kill-buffer-and-close )
  (evil-ex-define-cmd "wq" 'pfn/ex-save-kill-buffer-and-close))


(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (add-hook 'evil-mode-hook 'pfn/config-evil)
  (evil-mode))


(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (pfn/config-evil-leader))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode)
  (pfn/config-evil-surround))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package key-chord
  :ensure t
  :config
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "gs" 'magit-status)
  (key-chord-mode 1))

(provide 'evil-settings)
