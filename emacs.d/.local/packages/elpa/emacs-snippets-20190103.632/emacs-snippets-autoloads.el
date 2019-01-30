;;; emacs-snippets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "emacs-snippets" "emacs-snippets.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from emacs-snippets.el

(autoload 'emacs-snippets-initialize "emacs-snippets" "\
Add `emacs-snippets-dir' to `yas-snippet-dirs', replacing the default
yasnippet directory.

\(fn)" nil nil)

(eval-after-load 'yasnippet `(emacs-snippets-initialize))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacs-snippets" '("emacs-snippets-dir")))

;;;***

;;;### (autoloads nil nil ("emacs-snippets-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emacs-snippets-autoloads.el ends here
