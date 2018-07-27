;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
                     '(("xlist" "\\begin{xlist}\n$1\n\\end{xlist}\n$0\n" "xlist" nil nil nil "/home/niels/.emacs.d/etc/yasnippet/snippets/latex-mode/xlist" nil nil)
                       ("ref" "(\\ref{${1:label}})$0" "ref" nil nil nil "/home/niels/.emacs.d/etc/yasnippet/snippets/latex-mode/ref" nil nil)
                       ("gl" "\\gll $2\\\\\\\\\n$3\\\\\\\\\n\\glt '$4'" "gloss" nil nil nil "/home/niels/.emacs.d/etc/yasnippet/snippets/latex-mode/gl" nil nil)
                       ("exe" "\\begin{exe}\n\n\\end{exe}" "begin-exe" nil nil nil "/home/niels/.emacs.d/etc/yasnippet/snippets/latex-mode/examples" nil nil)
                       ("ex" "\\ex \\label{${1:label}}\n" "ex" nil nil nil "/home/niels/.emacs.d/etc/yasnippet/snippets/latex-mode/ex" nil nil)))


;;; Do not edit! File generated at Tue Jul 24 16:38:14 2018
