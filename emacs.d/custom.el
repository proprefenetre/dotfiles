(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))
 '(org-agenda-files '("~/org/algemeen.org" "~/org/thesis.org"))
 '(org-capture-templates
   '(("w" "word" entry
      (file "~/org/dict.org")
      "* %?" :empty-lines-after 1)
     ("t" "todo" entry
      (file+headline "~/org/algemeen.org" "To do")
      "* todo %?")
     ("e" "emacs-todo" entry
      (file+headline "~/org/algemeen.org" "Emacs")
      "* todo %?")
     ("s" "scriptie" entry
      (file+headline "~/org/thesis.org" "Algemeen")
      "* todo %?")
     ("l" "link" entry
      (file+headline "~/org/algemeen.org" "To Read")
      "* read [[%?][]]")
     ("n" "Note" entry
      (file+headline "~/org/algemeen.org" "NB")
      "* %?")) t)
 '(package-selected-packages
   '(nord-theme nordless-theme material-theme elpy emacs-lisp-mode general swiper hydra counsel ivy edit-indirect minions aggressive-indent color-theme-sanityinc-tomorrow company dash gruvbox-theme magit monokai-theme org-plus-contrib rich-minority s solarized-theme toc-org which-key yasnippet paredit racket-mode yaml-mode guess-language python-mode rust-mode flycheck-rust flycheck smart-mode-line rainbow-delimiters base16-theme markdown-mode olivetti eyebrowse key-chord evil-commentary evil-surround evil-leader evil-collection evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-comment-face ((t (:foreground "#93a1a1" :strike-through nil))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.0)))))
