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

(defhydra org-hydra (:color blue :columns 1)
  ("a" org-archive-subtree "Archive")
  ("r" org-refile "Refile"))

;; (defhydra )
  (provide 'pfn-hydras)

;;; pfn-hydras.el ends here
