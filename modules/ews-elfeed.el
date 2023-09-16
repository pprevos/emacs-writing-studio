;; Configure Elfeed
(use-package elfeed
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
   (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c w" . elfeed ))

;; Configure Elfeed with org mode
(use-package elfeed-org
  :after denote
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (denote-directory-files-matching-regexp "elfeed")))
