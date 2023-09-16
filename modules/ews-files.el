;; FILE MANAGEMENT

;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)

(setq dired-guess-shell-alist-user `(("\\.png\\'" "gimp")
                                     ("\\.jpe?g\\'" "gimp")
                                     ("\\.mp4\\'" "vlc")))

(use-package dired
  :ensure
  nil
  :commands
  (dired dired-jump)
  :custom
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t))

;; Hide hidden files
(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

;; Keep folders clean (create new directory when not yet existing)
;;(make-directory (expand-file-name "backups/" user-emacs-directory) t)
(setq-default backup-directory-alist
              `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
              create-lockfiles nil)  ; No lock files
