;; Init file for More Productive with Emacs
;; https://lucidmanager.org/tags/emacs/

;; CONFIGURE EMACS: FROM VANILLA EMACS TO PRODUCTIVITY
;; https:///lucidmanager.org/productivity/configure-emacs/

;; Define the init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Define and initialise package repositories
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure 't)

;; Keyboard-centric users interface
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Theme
(use-package dracula-theme
  :init (load-theme 'dracula t))

;; Scratch buffer
(setq initial-scratch-message "#+title: Scratch Buffer\n\n"
      initial-major-mode 'org-mode)

;; EMACS FOR DISTRACTION-FREE WRITING
;; https://lucidmanager.org/productivity/emacs-for-distraction-free-writing/

;; Sensible line breaking
(add-hook 'text-mode-hook 'visual-line-mode)

;; Configure Org-Mode basics
(use-package org
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  ;; Org mode ricing
  (setq org-hide-leading-stars t
	org-startup-indented t
        org-hide-emphasis-markers t
	org-image-actual-width 600
	org-startup-with-inline-images t))

;; Overwrite selected text
(delete-selection-mode t)

;; Spell checking
;; Requires Hunspell
(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (setq ispell-program-name "hunspell"))

;; Distraction-free screen
(use-package olivetti
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn 
          (delete-other-windows)
          (text-scale-increase 2)
          (setq olivetti-body-width 100)
          (olivetti-mode t))
      (progn
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  (global-set-key (kbd "<f9>") 'distraction-free))

;; GETTING THINGS DONE WITH ORG-MODE
;; https://lucidmanager.org/productivity/getting-things-done-with-emacs/

;; Set todo-item keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Set Agenda
;; Change to suit your agenda file(s)
(setq org-agenda-files '("~/todo.org" "~/other-file.org"))

;; Capture distractions
(global-set-key "\C-c c" 'org-capture)
(setq org-capture-templates
	'(("d" "Distraction" entry (file+headline "~/distractions.org" "Notes")
	 "* %?\n%T")))

;; TAKING NOTES WITH EMACS ORG-MODE AND ORG-ROAM
;; https://lucidmanager.org/productivity/taking-notes-with-emacs-org-mode-and-org-roam/
(use-package org-roam
  :hook (after-init . org-roam-mode)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              ("C-c n i" . org-roam-insert))
  :config (setq org-roam-directory (concat (getenv "HOME") "/notes")
                org-roam-capture-templates '(("d" "default" plain 
                                              (function org-roam--capture-get-point)
                                              "%?"
                                              :file-name "${slug}"
                                              :head "#+title: ${title}\n#+date: %U\n#+roam_alias: \n#+roam_tags: \n\n"
                                              :unnarrowed t))))

;; CREATE WEBSITES WITH EMACS: BLOGGING WITH ORG MODE AND HUGO
;; https://lucidmanager.org/productivity/create-websites-with-org-mode-and-hugo/

  ;; Update files with last modifed date
  (setq time-stamp-active t
        time-stamp-start "#\\+lastmod:[ \t]*"
        time-stamp-end "$"
        time-stamp-format "%04Y-%02m-%02d")
  (add-hook 'before-save-hook 'time-stamp nil)

;; New link type for Org-Hugo internal links
  (org-link-set-parameters "hugo"
                           :complete (lambda ()
                                       (concat "{{% ref "
                                               (file-relative-name (read-file-name "File: "))
                                               " %}}")))

;; PUBLISHING ARTICLES AND BOOKS WITH ORG MODE EXPORT
;; https://lucidmaager.org/productivity/publishing-with-org-mode-export/

;; Export to MS-Word
;; Need to have LibreOffice on your computer
(setq org-odt-preferred-output-format "doc")

;; MANAGE YOUR LITERATURE WITH EMACS BIBTEX MODE

;; Spell checking (requires the ispell software)
(add-hook 'bibtex-mode-hook 'flyspell-mode)

;; Change fields and format
(setq bibtex-user-optional-fields '(("keywords" "Keywords to describe the entry" "")
                                    ("file" "Link to document file." ":"))
      bibtex-include-OPTkey nil
      bibtex-align-at-equal-sign t)
(setq-default fill-column 160)

;; File locations
;; Change to suite your preferences
(setq bib-files-directory (directory-files
                           (concat (getenv "HOME") "/references") t ".bib$")
      pdf-files-directory (concat (getenv "HOME") "/pdf")
      bib-notes-directory (concat (getenv "HOME") "/notes"))

;; Helm-BiBTeX
(use-package helm-bibtex
  :config
  (require 'helm-config)
  (setq bibtex-completion-bibliography bib-files-directory
        bibtex-completion-library-path pdf-files-directory
        bibtex-completion-pdf-field "File"
        bibtex-completion-notes-path bib-notes-directory)
  :bind
  (("<menu>" . helm-command-prefix)
   :map helm-command-map
   ("b" . helm-bibtex)
   ("<menu>" . helm-resume)))

;; Org Ref
(use-package org-ref
  :config
  (setq org-ref-completion-library 'org-ref-helm-cite
	org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
	org-ref-default-bibliography bib-files-directory
	org-ref-notes-directory bib-notes-directory))

;; MANAGE FILES WITH EMACS: ORGANISE YOUR DRIVE WITH DIRED
;; https://lucidmanager.org/productivity/manage-files-with-emacs/

;; Open folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file))

;; Copy and move between buffers
(setq dired-dwim-target t)

;; Use only Y or N as confirmation
(defalias 'yes-or-no-p 'y-or-n-p)

;; Move deleted files to trash
(setq delete-by-moving-to-trash t)

;; VIEWING IMAGES WITH EMACS AND THE IMAGE-DIRED PACKAGE
;; https://lucidmanager.org/productivity/using-emacs-image-dired/

;; Remap the keys to open the external image viewer with C-enter and lets you activate image-dired with C-t -C-d
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "C-t C-d") 'image-dired)
    (define-key dired-mode-map (kbd "C-<return>") 'image-dired-dired-display-external))


;; READ RSS FEEDS WITH EMACS AND ELFEED
;; https://lucidmanager.org/productivity/read-rss-feeds-with-emacs-and-elfeed/

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-x w" . elfeed ))

(use-package elfeed-org
  :ensure t
  :config
  (setq elfeed-show-entry-switch 'display-buffer)
  (setq rmh-elfeed-org-files (list "elfeed.org")))
