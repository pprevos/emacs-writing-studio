(use-package bibtex
  :custom
  (bibtex-dialect 'BibTeX)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to document file." ":")))
  (bibtex-align-at-equal-sign t))

;; Biblio package for adding BibTeX records and download publications
(use-package biblio)

;; Citar to access bibliographies
(use-package citar
  :custom
  (org-cite-global-bibliography
   (directory-files
    (concat (getenv "HOME") "/Documents/library/") t
    "^[A-Z|a-z|0-9].+.bib$"))
  (citar-bibliography org-cite-global-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind
  (("C-c d o" . citar-open)
   (:map org-mode-map
         :package org
         ("C-c b" . #'org-cite-insert))))
