;;; pfn-hydras.el --- 
;;; Commentary:
;;; 
;;; Code:

(defhydra dumb-jump-hydra (:color blue :columns 3)
  "Dumb Jump"
  ("j" dumb-jump-go "Go")
  ("o" dumb-jump-go-other-window "Other window")
  ("e" dumb-jump-go-prefer-external "Go external")
  ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
  ("i" dumb-jump-go-prompt "Prompt")
  ("l" dumb-jump-quick-look "Quick look")
  ("b" dumb-jump-back "Back"))

(defhydra org-hydra (:color blue :columns 2)
  ("a" org-archive-subtree "Archive")
  ("r" org-refile "Refile"))

(defhydra frequent-files (:color blue :columns 3)
  ("i" (find-file "/home/eigenraam/.emacs.d/init.el") "init.el")
  ("k" (find-file "/home/eigenraam/.emacs.d/config/pfn-keys.el") "pfn-keys.el")
  ("k" (find-file "/home/eigenraam/.emacs.d/config/pfn-python.el") "pfn-python.el")
  ("t" (find-file "/home/eigenraam/org/todo.org") "todo.org")
  ("b" (find-file "/home/eigenraam/org/inbox.org") "inbox.org"))

(provide 'pfn-hydras)

;;; pfn-hydras.el ends here
