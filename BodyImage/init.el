(require 'ess-site)
(require 'ess-smart-underscore)

;; Active the R language in Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))

;; Auto complete
(require 'auto-complete-config)
(ac-config-default)

;;Org BibTeX references
(require 'org-ref)
(org-ref-define-citation-link "citeA" ?a)
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	))

;; Org Mode Latex Templates
(require 'ox-latex)

;; American Psychological Association papers
(add-to-list 'org-latex-classes '("apa6"
    "\\documentclass[a4paper, jou]{apa6}
     [NO-DEFAULT-PACKAGES]
     \\usepackage[hidelinks]{hyperref}
     \\usepackage{apacite}
     \\usepackage[british]{babel}
     \\usepackage{ccicons}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%Rs}")
	       ))
