(setq user-full-name "Your Name Here"
      user-mail-address "user@email.com")

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

  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package evil
  :demand t
  :config
  (evil-mode 1))

(use-package ivy
  :demand t)

(use-package counsel-projectile)
(use-package counsel
  :demand t)

(use-package swiper
  :commands (swiper swiper-all))

(use-package magit
  :commands (magit-status magit-blame magit-log-buffer-file magit-log-all))

(use-package projectile
  :demand t)

(use-package org
  :ensure org-plus-contrib
  :pin org
  :defer t)

;; Ensure ELPA org is prioritized above built-in org.
(require 'cl)
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

(use-package toc-org
  :after org
  :init (add-hook 'org-mode-hook #'toc-org-enable))

(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
