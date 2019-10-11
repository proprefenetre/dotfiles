;;; pfn-org.el --- setup org-mode
;;; Commentary:
;;; Code:
(use-package org
  :demand t
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :commands org-capture
  :config
  (set-face-attribute 'org-level-1 nil :height 1.0 :box nil)
  (setq org-agenda-files '("~/org/todo.org" "~/org/notes.org" "~/org/inbox.org"))
  (setq org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)))
  (setq org-directory "~/org"
        org-default-notes-file "~/org/todo.org"
        ;; org-agenda-files (append
        ;;                   (file-expand-wildcards "~/org/*.org"))
        org-refile-allow-creating-parent-nodes t
        org-refile-use-outline-path 'file
        org-archive-location "~/org/archief::datetree/"
        org-cycle-separator-lines -1
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil))
        org-return-follows-link t
        org-reverse-note-order nil 
        org-outline-path-complete-in-steps nil
        org-use-speed-commands t
        org-pretty-entities t
        org-log-done nil
        org-startup-indented t)
  (compdef
   :modes 'org-mode
   :company '(company-yasnippet company-files company-capf company-keywords company-dict
                                (company-abbrev company-dabbrev-code))
   :capf 'pcomplete-completions-at-point)

  (setq org-capture-templates
        '(("c" "Capture" entry (file "~/org/inbox.org")
           "* TODO %?\n")))

  (setq org-todo-keywords '((sequence "TODO" "|" "DONE")
                            (type "AFSPRAAK(a)" "GOOGLE(g)" "READ(r)" "NB(n)" "IDEE(i)" "|" "DONE(d)")
                            (sequence "FIXME(f)" "STARTED(s)" "AFWACHTEN(w)" "BEZIG(b)" "|" "DONE(d)" "CANCELED(c)")))

  (setq org-todo-keyword-faces
        '(("TODO" . "yellow")
          ("FIXME" . "red")
          ("BEZIG" . "SpringGreen")
          ("AFWACHTEN" . "SpringGreen" )
          ("READ" . "cyan")
          ("GOOGLE" . "cyan")
          ("AFSPRAAK" . "magenta")
          ("CANCELED" . "red")
          ("IDEE" . "orange")
          ("NB". "orange")))

  ;; Latex stuff
  (setq org-use-sub-superscripts nil)

  (require 'ox-latex)
  (add-to-list 'org-latex-default-packages-alist '("" "fontspec" t ("xelatex")))

  (setq org-latex-compiler "xelatex")
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("frame" "lines")))

  (add-to-list 'org-latex-classes
               '("pfn-article"
                 "\\documentclass[11pt,a4paper]{article}
[DEFAULT-PACKAGES]
\\usepackage{fullpage}
\\setmainfont[Mapping=tex-text]{DejaVu Serif}
\\setsansfont[Mapping=tex-text]{DejaVu Sans}
\\setmonofont{Hack}
\\usepackage[hyperref,x11names]{xcolor}
\\usepackage[parfill]{parskip}
\\usepackage{float}
\\usepackage{needspace}
\\usepackage{minted}
\\usepackage{etoolbox}
\\usepackage{titlesec}
[PACKAGES]
\\preto\\verbatim{\\topsep=5pt \\partopsep=5pt}
\\preto\\minted{\\needspace{4\\baselineskip}}
\\makeatletter \\renewcommand{\\fps@listing}{htp} \\makeatother
\\newcommand{\\sectionbreak}{\\clearpage}
\\hypersetup{colorlinks=true,urlcolor=blue,linkcolor=blue}
\\AtBeginEnvironment{quote}{\\itshape}
\\frenchspacing
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; (require 'ob-shell)
  (org-babel-do-load-languages 'org-babel-load-languages '((ditaa . t)
                                                           (dot . t)
                                                           (shell . t)
                                                           (python . t)))


  (setq org-latex-create-formula-image-program 'dvipng)

  (defun pfn-confirm-lang (lang body)
    (not (member t (mapcar (lambda (l) (string= lang l)) '("ditaa" "dot")))))

  (setq org-confirm-babel-evaluate 'pfn-confirm-lang)
  )

(provide 'pfn-org)
;;; pfn-org.el ends here
