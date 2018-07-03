;;; Compiled snippets and support files for `yasnippet'
;;; Snippet definitions:
;;;
(yas-define-snippets 'yasnippet
                     '(("xlist" "\\begin{xlist}\n$1\n\\end{xlist}\n$0\n" "xlist" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/latex-mode/xlist" nil nil)
                       ("ref" "(\\ref{${1:label}})$0" "ref" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/latex-mode/ref" nil nil)
                       ("gl" "\\glt $2\\\\\\\\\n$3\\\\\\\\\n\\glt $4" "gloss" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/latex-mode/exl" nil nil)
                       ("exe" "\\begin{exe}\n$1\n\\end{exe}" "begin-exe" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/latex-mode/examples" nil nil)
                       ("ex" "\\ex \\label{${1:label}}\n" "ex" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/latex-mode/ex" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'yasnippet
                     '(("xlist" "\\begin{xlist}\n$1\n\\end{xlist}\n$0\n" "xlist" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/markdown-mode/xlist" nil nil)
                       ("[@" "[@$1 $2]$0\n" "verwijzing" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/markdown-mode/verwijzing" nil nil)
                       ("ref" "(\\ref{${1:label}})$0" "ref" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/markdown-mode/ref" nil nil)
                       ("def-dict" "<!-- Local Variables: -->\n<!-- ispell-check-comments: exclusive -->\n<!-- ispell-local-dictionary: \"nederlands\" -->\n<!-- End: -->\n" "ispell settings" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/markdown-mode/ispell-default-dictionary" nil nil)
                       ("gl" "\\ex $0" "gloss" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/markdown-mode/exl" nil nil)
                       ("exe" "\\begin{exe}\n$0\n\\end{exe}" "begin-exe" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/markdown-mode/examples" nil nil)
                       ("ex" "\\ex \\label{${1:label}}\n\\gll $2\\\\\\\\\n$3\\\\\\\\\n\\glt $4\n" "ex" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/markdown-mode/ex" nil nil)
                       ("ctab" "#+caption: $1 {#tbl:$2}\n| Case       | Noun |\n|------------+------|\n| Nominative |      |\n| Genitive   |      |\n| Dative     |      |\n| Accusative |      |\n| Vocative   |      |\n| Ablative   |      |\n" "case table" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/markdown-mode/case-table" nil nil)
                       ("bf" "\\textbf{`(yank)`}$0" "bold" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/markdown-mode/bf" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'yasnippet
                     '(("snip" "# -*- mode: snippet -*-\n# name: $1\n# key: $2\n# --\n$3\n" "snip" nil nil nil "/home/niels/dotfiles/emacs.d/etc/yasnippet/snippets/snippet-mode/snip" nil nil)))


;;; Do not edit! File generated at Wed May 30 16:12:28 2018
