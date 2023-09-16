#+title:      Marcuse: An Essay on Liberation
#+date:       [2022-11-12 Sat 19:23]
#+filetags:   :bib:culture:marketing:philosophy:
#+identifier: 20221112T192310
#+reference:  marcuse_1969_essay

(use-package citar-denote
  :config
  (citar-denote-mode)
  :custom
  (citar-open-always-create-notes t)
  :bind (("C-c n c c" . citar-create-note)
         ("C-c n c o" . citar-denote-open-note)
         ("C-c n c d" . citar-denote-dwim)
         ("C-c n c a" . citar-denote-add-citekey)
         ("C-c n c x" . citar-denote-remove-citekey)))
