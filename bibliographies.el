;; Manage Bibliographies with Emacs BibTeX Mode
;; https://lucidmanager.org/productivity/emacs-bibtex-mode/

;; Basic setup
;; Custom settings in a separate file and load the custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Package repositories
(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap use-package to simplify the config file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Install any missing packages
(setq use-package-always-ensure 't)

;; Bibliography Management
;; -----------------------

;; Location of bib and pdf files
(setq bib-files-directory (directory-files (concat (getenv "HOME") "/references") t ".bib$")
      pdf-files-directory (concat (getenv "HOME") "/pdf")
      bib-notes-directory (concat (getenv "HOME") "/notes"))

;; Spell checking
;; Needs ispell to work: https://www.gnu.org/software/ispell/
(add-hook 'bibtex-mode-hook 'flyspell-mode)

;; bibtex-mode optimisation
(setq bibtex-dialect "BibTeX" ;; Optionally change to "biblatex"
      bibtex-user-optional-fields '(("keywords" "Keywords to describe the entry")
                                    ("file"     "Link to document file."))
      bibtex-include-OPTkey nil
      bibtex-align-at-equal-sign t)
(setq-default fill-column 160)

;; Helm BibTeX
(use-package helm-bibtex
  :config
  (require 'helm-config)
  (setq bibtex-completion-bibliography bib-files-directory
        bibtex-completion-library-path pdf-files-directory
        bibtex-completion-pdf-field "File"
	bibtex-completion-notes-path bib-notes-directory
	bibtex-completion-additional-search-fields '(keywords))
  :bind
  (("<menu>" . helm-command-prefix)
   :map helm-command-map
   ("b" . helm-bibtex)
   ("<menu>" . helm-resume)))

;; Org-ref

(use-package org-ref
  :config
  (setq
   org-ref-completion-library 'org-ref-helm-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-default-bibliography bib-files-directory
   org-ref-notes-directory bib-notes-directory))
