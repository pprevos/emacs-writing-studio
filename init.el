;;; init.el --- Emacs Writing Studio: configuration for authors

;; Copyright (C) 2023 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; Homepage: https://github.com/pprevos/emacs-writing-studio
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:

;;; Code:

;; CONFIGURATION AND PACKAGES

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
  ;; Always load packages when not yet installed
  (use-package-always-ensure t)
  ;; Native compile packages
  (package-native-compile t)
  ;; Do not display the warning buffer unless it's an error
  (warning-minimum-level :error))

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
  (("<f12>"   . modus-themes-toggle)
   ("C-<f12>" . modus-themes-select))
  :init
  (load-theme 'modus-operandi-tinted :no-confirm))

;; Set default, fixed and variable pitch fonts
;; Use M-x menu-set-font to view available fonts
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

;; Configure Text Modes and Org Mode

;; Sensible line-breaking
(add-hook 'text-mode-hook 'visual-line-mode)

;; Overwrite selected text
(delete-selection-mode t)

;; Scroll to the first and last line of the buffer
(setq-default scroll-error-top-bottom t)

(setq-default initial-major-mode 'org-mode
              initial-scratch-message "#+title: Scratch Buffer\n\n")

(use-package persistent-scratch
  :ensure t
  :commands persistent-scratch-setup-default
  :hook (after-init . persistent-scratch-setup-default))

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
  :bind (:map minibuffer-local-map
	 ("M-A". marginalia-cycle))
  :init
  (marginalia-mode))

;; Improve keyboard shortcut discoverability
(use-package which-key
  :config
  (which-key-mode))

;; READING EBOOKS WITH EMACS

;; Read ePub files
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package bibtex
  :custom
  (bibtex-dialect 'BibTeX)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to document file." ":")))
  (bibtex-align-at-equal-sign t))

;; Biblio package for adding BibTeX records and download publications
(use-package biblio)

;; Citar to access bibliographies
(use-package citar
  :custom
  (org-cite-global-bibliography
   (directory-files
    (concat (getenv "HOME") "/Documents/library/") t
    "^[A-Z|a-z|0-9].+.bib$"))
  (citar-bibliography org-cite-global-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind
  (("C-c d o" . citar-open)
   (:map org-mode-map
         :package org
         ("C-c b" . #'org-cite-insert))))

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

;; Emacs Multimedia System configuration
(use-package emms
  :init
  (require 'emms-setup)
  (require 'emms-mpris)
  (emms-all)
  (emms-default-players)
  (emms-mpris-enable)
  :custom
  (emms-source-file-default-directory "~/Music/")
  :bind
  (("<f5>" . emms-browser)
   ("<M-f5>" . emms)
   ("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)))

;; Choose one of these
(setq emms-info-functions '(emms-info-tinytag))  ;; When using Tinytag
;;(setq emms-info-functions '(emms-info-exiftool)) When using Exiftool

;; Load cover images
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)

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

;; Notes drawers
(defun ews-org-insert-notes-drawer ()
  "Generate a NOTES drawer under the heading of the current or jump to an existing one."
  (interactive)
  (push-mark)
  (org-previous-visible-heading 1)
  (next-line 1)
  (if (looking-at-p "^[ \t]*:NOTES:")
      (progn
        (re-search-forward "^[ \t]*:END:" nil t)
        (previous-line)
        (end-of-line)
        (org-return))
    (org-insert-drawer nil "NOTES"))
  (message "Press C-u C-SPACE to return to previous position."))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c C-x n") #'ews-org-insert-notes-drawer))

;; Auto completion
(use-package company
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 4)
  (company-selection-wrap-around t)
  :init
  (global-company-mode))

;; Required for proportional font
(use-package company-posframe
  :config
  (company-posframe-mode 1))

;; RICING ORG MODE

;; Improve org mode looks
(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

;; Show hidden emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode))

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
  :hook
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)))

;; Increase line spacing
(setq-default line-spacing 2)

;; Distraction-free writing
(use-package olivetti
  :config
  (defun distraction-free ()
    "Distraction-free writing environment using Olivetti package."
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-set 2)
          (olivetti-mode t))
      (progn
        (if (eq (length (window-list)) 1)
            (jump-to-register 1))
        (olivetti-mode 0)
        (text-scale-set 0))))
  :bind
  (("<f9>" . distraction-free)))

;; PUBLISHING DOCUMENTS

;; Org Export Settings
(use-package org
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil))

;; LaTeX PDF Export settings
(use-package ox-latex
  :ensure nil
  :demand t
  :custom
  ;; Multiple LaTeX passes for bibliographies
  (org-latex-pdf-process
   '(“pdflatex -interaction nonstopmode -output-directory %o %f”
     "bibtex %b"
     “pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f”
     “pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f”))
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions
   (quote (“lof” “lot” “tex~” “aux” “idx” “log” “out”
           "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
           "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
           “tex” “bcf” ))))

;; epub export
(use-package ox-epub
  :demand t)

;; Update Org files with last modified date when #+lastmod: is available
(setq time-stamp-active t
      time-stamp-start "#\\+lastmod:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "[%04Y-%02m-%02d %a]")
(add-hook 'before-save-hook 'time-stamp nil)

;; Create Hugo links
(defun ews-hugo-list-content ()
  "List the content of the Hugo website of the current buffer.
When not in an apparent Hugo directory then return error."
  (if-let* ((dir (if (string-match "\\(.*\\)content" default-directory)
                     (match-string 1 default-directory)
                   nil))
            (hugo-p (not (null (directory-files dir nil "^config\\..*"))))
            (content-dir (concat dir "content/")))
      (let ((org-files (directory-files-recursively content-dir "\\.org\\'"))
            (md-files (directory-files-recursively content-dir "\\.md\\'")))
        (append org-files md-files))
    (user-error "Not in a Hugo buffer")))

(defun ews-hugo-link-complete ()
  "Complete a Hugo weblink through the `org-insert-link' and hugo: hyperlink type."
  (let* ((posts (ews-hugo-list-content))
         (titles (mapcar (lambda (post)
                           (string-remove-prefix
                            (concat hugodir "content/") post)) posts))
         (selection (completing-read "Choose page:" titles))
         (target (concat "/"
                         (replace-regexp-in-string
                          "_index.*" "" selection))))
    (when titles
      (concat "{{< ref \"" target "\" >}}"))))

;; New link type for Org-Hugo internal links
(org-link-set-parameters
 "hugo"
 :complete #'ews-hugo-link-complete)

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


;;; init.el ends here
