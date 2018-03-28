;;; hydras.el --- Hydra definitions
;;; Commentary:
;;; Code:

(require 'evil)

(defhydra hydra-buffer (:color blue :columns 3)
  "
  Buffers:
  "
  ("n" next-buffer "next" :color red)
  ("p" previous-buffer "prev" :color red)
  ("b" ivy-switch-buffer "ivy-switch")
  ("B" ibuffer "ibuffer")
  ("N" evil-buffer-new "new")
  ("s" save-buffer "save" :color red)
  ("d" kill-this-buffer "delete" :color red)
  ;; don't come back to previous buffer after delete
  ("D" (progn (kill-this-buffer) (next-buffer)) "Delete" :color red))

(defhydra hydra-org-agenda (:color blue :columns 3)
  "
  AGENDA:
  "
  ("A" org-agenda "agenda menu" :color red)
  ("a" org-agenda-list "agenda" :color blue)
  ("t" org-todo-list "global to do-list" :color blue))

(provide 'hydras)

;;; hydras.el ends here
