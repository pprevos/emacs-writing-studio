;; Configure Elfeed
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-x w" . elfeed ))

;; Configure Elfeed with org mode
(use-package elfeed-org
  :ensure t
  :config
 (setq elfeed-show-entry-switch 'display-buffer)
 (setq rmh-elfeed-org-files (list "elfeed.org")))
