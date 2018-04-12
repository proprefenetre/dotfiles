;;; edit-indirect-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "edit-indirect" "edit-indirect.el" (0 0 0 0))
;;; Generated autoloads from edit-indirect.el

(autoload 'edit-indirect-region "edit-indirect" "\
Edit the region BEG..END in a separate buffer.\nThe region is copied, without text properties, to a separate\nbuffer, called edit-indirect buffer, and\n`edit-indirect-guess-mode-function' is called to set the major\nmode.\nWhen done, exit with `edit-indirect-commit', which will remove the\noriginal region and replace it with the edited version; or with\n`edit-indirect-abort', which will drop the modifications.\n\nThis differs from `clone-indirect-buffer' with narrowing in that\nthe text properties are not shared, so the parent buffer major mode\nand the edit-indirect buffer major mode will not be able to tread\non each other's toes by setting up potentially conflicting text\nproperties, which happens surprisingly often when the font-lock\nmode is used.\n\nEdit-indirect buffers use the `edit-indirect-mode-map' keymap.\n\nIf there's already an edit-indirect buffer for BEG..END, use that.\nIf there's already an edit-indirect buffer active overlapping any\nportion of BEG..END, an `edit-indirect-overlapping' error is\nsignaled.\n\nWhen DISPLAY-BUFFER is non-nil or when called interactively,\ndisplay the edit-indirect buffer in some window and select it.\n\nIn any case, return the edit-indirect buffer.\n\n(fn BEG END &optional DISPLAY-BUFFER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "edit-indirect" '("edit-indirect-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; edit-indirect-autoloads.el ends here
