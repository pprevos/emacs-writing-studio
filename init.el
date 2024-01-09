;; init.el --- Emacs Writing Studio: configuration for authors

;; Copyright (C) 2023-2024 Peter Prevos
;;
;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; Homepage: https://github.com/pprevos/emacs-writing-studio
;; Version: 1.0
;; Package-Requires ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Emacs Writing Studio website: https://lucidmanager.org/tags/emacs
;;
;;; Code:

;; CONFIGURATION AND PACKAGES

;; Emacs 29?
(unless (>= emacs-major-version 29)
  (error "Emacs Writing Studio requires Emacs version 29 or later"))

;; Custom settings in a separate file and load the custom settings
(setq-default custom-file
              (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; Set package archives
(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

;; Package Management
(use-package use-package
  :custom
  (use-package-always-ensure t)
  (package-native-compile t)
  (warning-minimum-level :error))

;; Install ews package to define variables and provide convenience functions
(unless (package-installed-p 'ews)
  (package-vc-install
   '(ews
     :url "https://github.com/pprevos/emacs-writing-studio/"
     :main-file "ews.el")))

;; Check for missing external software
(ews-missing-executables
 '("soffice" "zip" "pdftotext" "ddjvu" ;; read ebooks
   "curl" ;; Read RSS feeds
   ("convert" "gm") ;; Modify images
   "latex";; Export to PDF
   "hunspell" ;; Spellcheck
   ("grep" "ripgrep") ;; Search files
   ("gs" "mutool") ;; PDF
   ("mpg321" "ogg123" "mplayer" "mpv" "vlc"))) ;; Play music

;; LOOK AND FEEL

;; Keyboard-centric user interface
(setq-default inhibit-startup-message t
              use-short-answers t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Modus Themes
(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-headings '((1 . (1.5))
                           (2 . (1.3))
                           (t . (1.1))))
  (modus-themes-to-toggle
   '(modus-operandi-tinted modus-vivendi-tinted))
  :bind
  (("C-c w m" . modus-themes-toggle)
   ("C-c w M" . modus-themes-select))
  :init
  (load-theme 'modus-operandi-tinted :no-confirm))

;; Set default, fixed and variable pitch fonts
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

;; RICING ORG MODE

;; Improve org mode looks
(setq-default org-startup-indented t
              org-pretty-entities t
              org-use-sub-superscripts "{}"
              org-hide-emphasis-markers t
              org-startup-with-inline-images t
              org-image-actual-width '(300))

;; Show hidden emphasis markers

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

;; Modernise Org mode interface
(use-package org-modern
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table nil))

;; LaTeX previews
(use-package org-fragtog
  :after org
  :custom
  (org-startup-with-latex-preview t)
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; Increase line spacing
(setq-default line-spacing 2)

;; Distraction-free writing
(use-package olivetti
  :demand t
  :bind
  (("<f9>" . ews-distraction-free)))

;; MINIBUFFER COMPLETION

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha))

;; Persist history over Emacs restarts.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :init
  (marginalia-mode))

;; Improve keyboard shortcut discoverability
(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

;; READ EBOOKS

;; Read ePub files
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Doc-View
(use-package doc-view
  :custom
  (doc-view-resolution 300)
  (doc-view-mupdf-use-svg t)
  (large-file-warning-threshold (* 50 (expt 2 20))))

;; Reading LibreOffice files
;; Fixing bug in Org mode 9.6.6
(use-package ox-odt
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(?:OD[CFIGPST]\\|od[cfigpst]\\)\\'" . doc-view-mode-maybe)))

;; Managing Bibliographies
(use-package bibtex
  :custom
  (bibtex-dialect 'BibTeX)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to a document file." "" )))
  (bibtex-align-at-equal-sign t))

;; Biblio package for adding BibTeX records and download publications
(use-package biblio)

;; Citar to access bibliographies
(use-package citar
  :custom
  (org-cite-global-bibliography
   (directory-files ews-bibliography-directory t
    "^[A-Z|a-z|0-9].+.bib$"))
  (citar-bibliography org-cite-global-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind
  (("C-c w c c" . citar-open)
   (:map org-mode-map
         :package org
         ("C-c w C". #'org-cite-insert))))

;; Integrating Biblio and Citar
(global-set-key (kbd "C-c w c b") 'ews-biblio-bibtex-lookup)

;; Configure Elfeed
(use-package elfeed
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
   (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c w e" . elfeed))

;; Configure Elfeed with org mode
(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files (list ews-elfeed-config-file)))

;; Easy insertion of weblinks
(use-package org-web-tools
  :bind
  (("C-c w w" . org-web-tools-insert-link-for-url)))

;; Emacs Multimedia System
(use-package emms
  :init
  (emms-all)
  (emms-default-players)
  (emms-mpris-enable)
  :custom
  (emms-source-file-default-directory ews-music-directory)
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  :bind
  (("<f5>"   . emms-browser)
   ("M-<f5>" . emms)
   ("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)))

;; IDEATION: NOTE-TAKING

;; Fleeting notes in Scratch Buffer
(setq initial-major-mode 'org-mode
      initial-scratch-message
      "#+title: Scratch Buffer\n\nWelcome to Emacs Writing Studio.\n\n")

(use-package persistent-scratch
  :hook
  (after-init . persistent-scratch-setup-default)
  :init
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode)
  :bind
  (("C-c w x" . scratch-buffer)))

;; Denote
(use-package denote
  :init
  (require 'denote-org-dblock)
  (denote-rename-buffer-mode t)
  :custom
  (denote-directory ews-notes-directory)
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :bind
  (("C-c n n" . denote-create-note)
   ("C-c n d" . denote-date)
   ("C-c n i" . denote-link-or-create)
   ("C-c n l" . denote-find-link)
   ("C-c n b" . denote-find-backlink)
   ("C-c n d" . denote-org-dblock-insert-links)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n k" . denote-keywords-add)
   ("C-c n K" . denote-keywords-remove)))

;; Denote extensions
(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   `(("Denote" ?d ,ews-notes-directory)))
  :bind
  (("C-c n f" . consult-notes)
   ("C-c n s" . consult-notes-search-in-all-notes)))

(package-vc-install '(denote-explore
                        :url "https://github.com/pprevos/denote-explore/"))

  (use-package denote-explore
    :after
    dashboard
    :load-path
    "~/Documents/projects/denote-explore/"
    :init
    ;; Add a widget to the dashboard
    (require 'denote-explore-dashboard)
    (denote-explore-dashboard-activate)
    :bind
    (;; Statistics
     ("C-c n e c" . denote-explore-count-notes)
     ("C-c n e C" . denote-explore-count-keywords)
     ;; Janitor
     ("C-c n e S" . denote-explore-single-keywords)
     ("C-c n e K" . denote-explore-rename-keyword)
     ;; Random walks
     ("C-c n e r" . denote-explore-random-note)
     ("C-c n e l" . denote-explore-random-link)
     ("C-c n e k" . denote-explore-random-keyword)
     ;; Visualise
     ("C-c n e w" . denote-explore-keywords-barchart)
     ("C-c n e x" . denote-explore-extensions-barchart)
     ("C-c n e n" . denote-explore-network-r)))


;; Fleeting notes
(use-package org
  :after
  denote
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :custom
  (org-default-notes-file ews-inbox-file)
  (org-capture-bookmark nil)
  ;; Capture templates
  (org-capture-templates
   '(("f" "Fleeting note" item
      (file+headline org-default-notes-file "Notes")
      "- %?")
     ("p" "Permanent note" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ("t" "New task" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %i%?"))))

(use-package citar-denote
  :config
  (citar-denote-mode)
  :custom
  (citar-open-always-create-notes t)
  :bind (("C-c w c n" . citar-create-note)
         ("C-c w c o" . citar-denote-open-note)
         ("C-c w c f" . citar-denote-find-citation)
         ("C-c w c d" . citar-denote-dwim)
         ("C-c w c e" . citar-denote-open-reference-entry)
         ("C-c w c a" . citar-denote-add-citekey)
         ("C-c w c k" . citar-denote-remove-citekey)
         ("C-c w c r" . citar-denote-find-reference)
         ("C-c w c l" . citar-denote-link-reference)
         ("C-c w c x" . citar-denote-nocite)
         ("C-c w c y" . citar-denote-cite-nocite)))

(use-package org
  :custom
  (org-list-allow-alphabetical t))

;; Notes drawers
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c C-x n") #'ews-org-insert-notes-drawer)
  (define-key org-mode-map (kbd "C-c w c") #'ews-org-insert-notes-drawer))

;; PUBLISHING DOCUMENTS

;; Org Export Settings
(use-package org
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%d %B %Y"))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("apa6"
                 "\\documentclass[a4paper, jou, 11pt]{apa6}
                  \\usepackage[nodoi]{apacite}
                  \\usepackage{graphicx}
                  \\usepackage[T1]{fontenc}
                  \\usepackage{times}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}"))))

;; LaTeX PDF Export settings
(use-package ox-latex
  :ensure nil
  :demand t
  :custom
  ;; Multiple LaTeX passes for bibliographies
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
           "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
           "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
           "tex" "bcf"))))

;; epub export
(use-package ox-epub
  :demand t)

;; Update Org files with last modified date when #+lastmod: is available
(setq time-stamp-active t
      time-stamp-start "#\\+lastmod:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "[%04Y-%02m-%02d %a]")
(add-hook 'before-save-hook 'time-stamp nil)

;; GETTING THINGS DONE
(use-package org
  :custom
  (org-agenda-files ews-todo-file)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  :bind
  (("C-c a" . org-agenda)))

;; FILE MANAGEMENT
(use-package dired
  :ensure
  nil
  :commands
  (dired dired-jump)
  :custom
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  :init  ;; Open dired folders in same buffer
  (put 'dired-find-alternate-file 'disabled nil))

;; Hide hidden files
(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

;; Backup files
(setq-default backup-directory-alist
              `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
              create-lockfiles nil)  ; No lock files

;; Recent files
(use-package recentf
  :config
  (recentf-mode t)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  :custom
  (recentf-max-saved-items 50))

;; Bookmarks
(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  :bind
  ("C-x r D" . bookmark-delete))

;; Image viewer
(use-package emacs
  :bind
  ((:map image-mode-map
              ("k" . image-kill-buffer)
              ("<right>" . image-next-file)
              ("<left>"  . image-previous-file))
   (:map dired-mode-map
    ("C-<return>" . image-dired-dired-display-external))))

;;; init.el ends here
