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
