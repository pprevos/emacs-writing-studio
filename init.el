;; init.el --- Emacs Writing Studio: configuration for authors

;; Copyright (C) 2023-2024 Peter Prevos
;;
;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; Homepage: https://github.com/pprevos/emacs-writing-studio
;; Version: 1.1
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
     :url "https://github.com/pprevos/emacs-writing-studio/")))

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
  :init
  (load-theme 'modus-operandi-tinted :no-confirm))

;; Set default, fixed and variable pitch fonts
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

;; CONFIGURE TEXT MODES

;; Sensible line-breaking
(add-hook 'text-mode-hook 'visual-line-mode)

;; Overwrite selected text
(delete-selection-mode t)

;; Scroll to the first and last line of the buffer
(setq-default scroll-error-top-bottom t)

;; Copy the system clipboard to the kill ring
(setq save-interprogram-paste-before-kill t)

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
(setq-default line-spacing 3)

;; Distraction-free writing
(use-package olivetti)

;; Undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  ;; Prevent undo tree from saving hidden undo files 
  :custom
  (undo-tree-auto-save-history nil))

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
  :custom
  (which-key-popup-type 'minibuffer))

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
  (org-cite-activate-processor 'citar))

;; Configure Elfeed
(use-package elfeed
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer))

;; Configure Elfeed with org mode
(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files (list ews-elfeed-config-file)))

;; Easy insertion of weblinks
(use-package org-web-tools)

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
  (("<XF86AudioPrev>" . emms-previous)
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
  (persistent-scratch-autosave-mode))

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
  (denote-faces-link ((t (:slant italic)))))

;; Denote extensions
(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   `(("Denote" ?d ,ews-notes-directory))))

;; (unless (package-installed-p 'denote-explore)
;;   (package-vc-install
;;    '(denote-explore
;;      :url "https://github.com/pprevos/denote-explore/")))

(use-package denote-explore)

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
  (citar-open-always-create-notes t))

(use-package org
  :custom
  (org-list-allow-alphabetical t))

;; Update Org files with last modified date when #+lastmod: is available
(setq time-stamp-active t
      time-stamp-start "#\\+lastmod:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "[%04Y-%02m-%02d %a]")
(add-hook 'before-save-hook 'time-stamp nil)

;; PUBLISHING DOCUMENTS

;; Org Export Settings
(use-package org
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%e %B %Y"))

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

;; Set keybindings
;; https://www.youtube.com/watch?v=gojOZ3k1mmk
(defvar-keymap ews-denote-map
  :doc "Denote keybindings."
  "n" #'denote-create-note
  "d" #'denote-date
  "i" #'denote-link-or-create
  "l" #'denote-find-link
  "b" #'denote-find-backlink
  "D" #'denote-org-dblock-insert-links
  "r" #'denote-rename-file
  "R" #'denote-rename-file-using-front-matter
  "k" #'denote-keywords-add
  "K" #'denote-keywords-remove)

(defvar-keymap ews-denote-explore-map
  :doc "Denote-Epxlore keybindings"
  "c" #'denote-explore-count-notes
  "C" #'denote-explore-count-keywords
  "S" #'denote-explore-single-keywords
  "K" #'denote-explore-rename-keyword
  "r" #'denote-explore-random-note
  "l" #'denote-explore-random-link
  "k" #'denote-explore-random-keyword
  "w" #'denote-explore-keywords-barchart
  "x" #'denote-explore-extensions-barchart
  "n" #'denote-explore-network-r)

(defvar-keymap ews-modus-themes-map
  :doc "Modus Themes keymap."
  "t" #'modus-themes-toggle
  "s" #'modus-themes-select)

(defvar-keymap ews-bibliography-map
  :doc "Bibliograpic functions keymap."
  "a" #'citar-denote-add-citekey
  "c" #'citar-open
  "d" #'citar-denote-dwim
  "e" #'citar-denote-open-reference-entry
  "f" #'citar-denote-find-citation
  "k" #'citar-denote-remove-citekey
  "l" #'citar-denote-link-reference
  "n" #'citar-create-note
  "o" #'citar-denote-open-note
  "r" #'citar-denote-find-reference
  "x" #'citar-denote-nocite
  "y" #'citar-denote-cite-nocite
  "B" #'ews-biblio-bibtex-lookup)

(defvar-keymap ews-emms-map
  :doc "Emacs Multimedia System (EMMS) keymap."
  "b" #'emms-browser
  "e" #'emms)
	    
(defvar-keymap ews-map
  :doc "Emacs Writing Studio key bindings."
  "b" ews-bibliography-map
  "c" #'scratch-buffer
  "d" ews-denote-map
  "e" #'elfeed
  "m" ews-emms-map
  "n" #'consult-notes
  "o" #'ews-distraction-free
  "s" #'consult-notes-search-in-all-notes  
  "t" ews-modus-themes-map
  "w" #'org-web-tools-insert-link-for-url
  "x" ews-denote-explore-map)

(which-key-add-keymap-based-replacements ews-map
  "b" `("Bibliography" . ,ews-bibliography-map)
  "d" `("Denote" . ,ews-denote-map)
  "m" `("Music" . ,ews-emms-map)
  "t" `("Themes" . ,ews-modus-themes-map)
  "x" `("Explore Notes" . ,ews-denote-explore-map))

(keymap-set global-map "C-c w" ews-map)

;; Org mode keymap modifications
(with-eval-after-load "org"
  (keymap-set org-mode-map "C-c w b b" #'org-cite-insert)
  (keymap-set org-mode-map "C-c w S" #'ews-org-screenshot)
  (keymap-set org-mode-map "C-c x n" #'ews-org-insert-notes-drawer)
  (keymap-set org-mode-map "C-c w c" #'ews-org-count-words))
;;; init.el ends here
