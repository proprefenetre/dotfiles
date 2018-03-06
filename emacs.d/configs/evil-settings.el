;;; evil-settings.el -- evil config

(defun pfn--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    ","  'other-window
    "."  'mode-line-other-buffer
    ":"  'eval-expression
    "b"  'helm-mini
    "d"  'kill-this-buffer
    "p"  'helm-show-kill-ring
    "w"  'save-buffer
    "x"	 'helm-M-x
    "y"  'yank-to-x-clipboard))

(defun pfn--config-evil ()
  "Configure evil mode."
  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  ;; Global bindings.
  (evil-define-key 'normal global-map (kbd "<down>")  'evil-next-visual-line)
  (evil-define-key 'normal global-map (kbd "<up>")    'evil-previous-visual-line)
  (evil-define-key 'insert global-map (kbd "s-d")     'eval-last-sexp)
  (evil-define-key 'normal global-map (kbd "s-d")     'eval-defun)
  (evil-define-key 'normal global-map (kbd "-")       'helm-find-files)

  ;; Make TAB work 
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
    In Delete Selection mode, if the mark is active, just deactivate it;
    then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-escape-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(provide 'evil-settings)
