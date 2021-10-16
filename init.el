;; This configuration is described in detail on the lucidmanager.org website
;; https://lucidmanager.org/productivity/more-productive-with-emacs/

;; Define the init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Define and initialise package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;; Keyboard-centric user interface
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Theme
(use-package spacegray-theme
  :config (load-theme 'spacegray t))

;; Open dired in same buffer
(put 'dired-find-alternate-file 'disabled nil)

;; Sort Dired buffers
(setq dired-listing-switches "-agho --group-directories-first")

;; Copy and move files netween dired buffers
(setq dired-dwim-target t)

;; Only y/n answers 
(defalias 'yes-or-no-p 'y-or-n-p)

;; Move deleted files to trash
(setq delete-by-moving-to-trash t)

;; Keep folders clean (create new directory when not yet existing)
(make-directory (expand-file-name "backups/" user-emacs-directory) t)
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

;; Define external image viewer/editor
(setq image-dired-external-viewer "/usr/bin/gimp")

;; Image-dired Keyboard shortcuts
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "C-t C-d") 'image-dired)
    (define-key dired-mode-map (kbd "C-<return>") 'image-dired-dired-display-external))

;; Helm configuration
  (use-package helm
    :config
    (require 'helm-config)
    :init
    (helm-mode 1)
    :bind
    (("M-x"     . helm-M-x) ;; Evaluate functions
     ("C-x C-f" . helm-find-files) ;; Open or create files
     ("C-x b"   . helm-mini) ;; Select buffers
     ("C-x C-r" . helm-recentf) ;; Select recently saved files
     ("C-c i"   . helm-imenu) ;; Select document heading
     ("M-y"     . helm-show-kill-ring) ;; Show the kill ring
     :map helm-map
     ("C-z" . helm-select-action)
     ("<tab>" . helm-execute-persistent-action)))

;; Auto completion
(use-package company
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 3
        company-selection-wrap-around t))
(global-company-mode)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle 0.5
	which-key-idle-dely 50)
  (which-key-setup-minibuffer))

;; Sensible line breaking
(add-hook 'text-mode-hook 'visual-line-mode)
;; Overwrite selected text
(delete-selection-mode t)
;; Scroll to the first and last line of the buffer
(setq scroll-error-top-bottom t)

;; Org-Mode initial setup
(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)))

;; Spell checking
;; Requires Hunspell
(use-package flyspell
  :config
  (setq ispell-program-name "hunspell"
        ispell-default-dictionary "en_AU")
  :hook (text-mode . flyspell-mode)
  :bind (("M-<f7>" . flyspell-buffer)
         ("<f7>" . flyspell-word)
         ("C-;" . flyspell-auto-correct-previous-word)))

;; Set default, fixed and variabel pitch fonts
;; Use M-x menu-set-font to view available fonts
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120)
  (set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono")
  (set-face-attribute 'variable-pitch nil :family "DejaVu Sans"))

;; Required for proportional font
(use-package company-posframe
  :config
  (company-posframe-mode 1))

;; Improve org mode looks
(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

;; Show hidden emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; Nice bullets
(use-package org-superstar
    :config
    (setq org-superstar-special-todo-items t)
    (add-hook 'org-mode-hook (lambda ()
                               (org-superstar-mode 1))))

;; Increase line spacing
(setq-default line-spacing 6)

;; Distraction-free screen
(use-package olivetti
  :init
  (setq olivetti-body-width .67)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  :bind
  (("<f9>" . distraction-free)))

;; Increase size of LaTeX fragment previews
(plist-put org-format-latex-options :scale 2)

;; Org-Roam basic configuration
(setq org-directory (concat (getenv "HOME") "/Documents/org-roam/"))

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-directory))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n r" . org-roam-node-random)		    
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

(use-package deft
  :config
  (setq deft-directory org-directory
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t)
  :bind
  ("C-c n d" . deft))

;; Spell checking (requires the ispell software)
(add-hook 'bibtex-mode-hook 'flyspell-mode)

;; Change fields and format
(setq bibtex-user-optional-fields '(("keywords" "Keywords to describe the entry" "")
                                    ("file" "Link to document file." ":"))
      bibtex-align-at-equal-sign t)

(setq bib-files-directory (directory-files
                           (concat (getenv "HOME") "/Documents/bibliography") t
                           "^[A-Z|a-z].+.bib$")
      pdf-files-directory (concat (getenv "HOME") "/Documents/pdf"))

(use-package helm-bibtex
  :config
  (require 'helm-config)
  (setq bibtex-completion-bibliography bib-files-directory
        bibtex-completion-library-path pdf-files-directory
        bibtex-completion-pdf-field "File"
        bibtex-completion-notes-path org-directory))

(use-package org-ref
  :config
  (setq org-ref-completion-library 'org-ref-helm-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-default-bibliography bib-files-directory
        org-ref-notes-directory org-directory
        org-ref-notes-function 'orb-edit-notes))

(use-package org-roam-bibtex
  :after (org-roam helm-bibtex)
  :bind (:map org-mode-map ("C-c n b" . orb-note-actions))
  :config
  (require 'org-ref))
  (org-roam-bibtex-mode)

;; Scratch buffer
(setq inhibit-startup-message t
      initial-scratch-message "#+title: Scratch Buffer\n\nWelcome to Emacs for content creators.\n\nPlease change the folder locations and fonts in the configuration file to match your preferences.\n\nGo to the [[https://lucidmanager.org/productivity/configure-emacs/][Lucid Manager]] website for full documentation of this configuration file.\n"
      initial-major-mode 'org-mode)
