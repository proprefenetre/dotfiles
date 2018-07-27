;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("xlist" "\\begin{xlist}\n$1\n\\end{xlist}\n" "xlist" nil nil nil "/home/niels/.emacs.d/etc/yasnippet/snippets/org-mode/xlist" nil nil)
                       ("ref" "(\\ref{${1:label}})$0" "ref" nil nil nil "/home/niels/.emacs.d/etc/yasnippet/snippets/org-mode/ref" nil nil)
                       ("gl" "\\glt $2\\\\\\\\\n$3\\\\\\\\\n\\glt $4" "gloss" nil nil nil "/home/niels/.emacs.d/etc/yasnippet/snippets/org-mode/exl" nil nil)
                       ("exe" "\\begin{exe}\n$1\n\\end{exe}" "begin-exe" nil nil nil "/home/niels/.emacs.d/etc/yasnippet/snippets/org-mode/examples" nil nil)
                       ("ex" "\\ex \\label{${1:label}}\n" "ex" nil nil nil "/home/niels/.emacs.d/etc/yasnippet/snippets/org-mode/ex" nil nil)
                       ("ctab" "#+caption: $1 {#tbl:$2}\n| Case       | |\n|------------+-|\n| Nominative |$3 |\n| Genitive   | |\n| Dative     | |\n| Accusative | |\n| Vocative   | |\n| Ablative   | |" "ctab" nil nil nil "/home/niels/.emacs.d/etc/yasnippet/snippets/org-mode/ctab" nil nil)))


;;; Do not edit! File generated at Tue Jul 24 16:38:14 2018
