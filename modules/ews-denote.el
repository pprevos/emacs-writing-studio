;; NOTE-TAKING

;; Denote
(use-package denote
  :init
  (require 'denote-org-dblock)
  :custom
  (denote-directory "~/Documents/notes/")
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :bind
  (("C-c n n" . denote)
   ("C-c n d" . denote-date)
   ("C-c n i" . denote-link-or-create)
   ("C-c n l" . denote-link-find-file)
   ("C-c n b" . denote-link-find-backlink)
   ("C-c n D" . denote-org-dblock-insert-links)
   ("C-c n s" . denote-rename-file-using-front-matter)
   ("C-c n k" . denote-keywords-add)
   ("C-c n K" . denote-keywords-remove)))

(use-package org
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :custom
  ;; Set default file for fleeting notes
  (org-default-notes-file
   (car (denote-directory-files-matching-regexp "inbox")))
  ;; Capture templates
  (org-capture-templates
   '(("f" "Fleeting note" item
      (file+headline org-default-notes-file "Notes")
      "- %?")
     ("t" "New task" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %i%?"))))
