; use-package settings
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

(provide 'package-settings)
