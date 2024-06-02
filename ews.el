;;; ews.el --- Convenience functions for authors  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>
;; Created: 1 January 2024
;; Version: 1.2
;; Keywords: convenience
;; Homepage: https://lucidmanager.org/tags/emacs/
;; URL: https://github.com/pprevos/emacs-writing-studio

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
;; Series of convenience functions for Emacs Writing Studio
;; https://lucidmanager.org/tags/emacs
;;
;;; Code:


;; Emacs Writing Studio Customisation

(defvar ews-home-directory
  (file-name-as-directory (getenv "HOME"))
  "Location of documents.")

(defgroup ews ()
  "Emacs Writing Studio."
  :group 'files
  :link '(url-link :tag "Homepage" "https://lucidmanager.org/tags/emacs/"))

(defcustom ews-bibtex-directory
  (concat (file-name-as-directory ews-home-directory) "Documents/library")
  "Location of BibTeX files and attachments."
  :group 'ews
  :type 'directory)

(defcustom ews-denote-para-keywords
  '("projects" "areas" "resources" "archives")
  "List of keywords to use for implementing the PARA method with Denote."
  :group 'ews
  :type 'list)

;; Check for missing external software
;;;###autoload
(defun ews-missing-executables (prog-list)
  "Identified missing executables in PROG-LIST.
Sublists indicate that one of the entries is required."
  (let ((missing '()))
    (dolist (exec prog-list)
      (if (listp exec)
          (unless (cl-some #'executable-find exec)
            (push (format "(%s)" (mapconcat 'identity exec " or ")) missing))
        (unless (executable-find exec)
          (push exec missing))))
    (if missing
        (message "Missing executable files(s): %s"
                 (mapconcat 'identity missing ", ")))))

;;; BIBLIOGRAPHY
(defvar ews-bibtex-files
  (when (file-exists-p ews-bibtex-directory)
    (directory-files ews-bibtex-directory t "^[A-Z|a-z|0-9].+.bib$"))
  "List of BibTeX files. Use `ews-bibtex-register-files` to configure.")

;;;###autoload
(defun ews-bibtex-register ()
  "Register the contents of the `ews-bibtex-directory` with `ews-bibtex-files`.
Use when adding or removing a BibTeX file from or to `ews-bibtex-directory`."
  (interactive)
  (when (file-exists-p ews-bibtex-directory)
    (let ((bib-files (directory-files ews-bibtex-directory t
				      "^[A-Z|a-z|0-9].+.bib$")))
      (setq ews-bibtex-files bib-files
  	    org-cite-global-bibliography bib-files
	    citar-bibliography bib-files)))
  (message "Registered:\n%s" (mapconcat #'identity ews-bibtex-files "\n")))

(defun ews--bibtex-combined-biblio-lookup ()
  "Combines biblio-lookup and biblio-doi-insert-bibtex."
  (let* ((dbs (biblio--named-backends))
         (db-list (append dbs '(("DOI" . biblio-doi-backend))))
         (db-selected (biblio-completing-read-alist
                       "Backend:"
                       db-list)))
    (if (eq db-selected 'biblio-doi-backend)
        (let ((doi (read-string "DOI: ")))
          (biblio-doi-insert-bibtex doi))
      (biblio-lookup db-selected))))

;;;###autoload
(defun ews-bibtex-biblio-lookup ()
  "Use curent buffer or Select BibTeX file, lookup with Biblio and insert entry."
  (interactive)
  (if-let ((current-mode major-mode)
	   ews-bibtex-files
	   (bibfiles (length ews-bibtex-files))
	   (bibfile (cond ((eq bibfiles 1) (car ews-bibtex-files))
			  ((equal major-mode 'bibtex-mode)
			   (buffer-file-name))
			  (t (completing-read
			      "Select BibTeX file:" ews-bibtex-files)))))
      (progn (find-file bibfile)
	     (goto-char (point-max))
	     (ews--bibtex-combined-biblio-lookup)
	     (save-buffer))
    (message "No BibTeX file(s) defined.")))

;; Search for missing BibTeX attachments and filenames
(defun ews--bibtex-extract-filenames ()
  "Extract attachment file names from BibTeX files in `ews-bibtex-directory'."
  (ews-bibtex-register)
  (let ((attachments '()))
    (dolist (bibtex-file ews-bibtex-files)
      (with-temp-buffer
        (insert-file-contents bibtex-file)
        (goto-char (point-min))
        (while (re-search-forward "file.*=.*{\\([^}]+\\)}" nil t)
          (let ((file-paths (split-string (match-string 1)
                                          "[[:space:]]*;[[:space:]]*")))
            (dolist (file-path file-paths)
              (push (expand-file-name (string-trim file-path)
                                      ews-bibtex-directory)
                    attachments))))))
    attachments))

(defun ews--bibtex-extract-files ()
  "List files recursively in `ews-bibtex-directory'.  Excludes `.bib` and `.csl`."
  (seq-remove (lambda (file)
                (or (string-suffix-p ".bib" file)
                    (string-suffix-p ".csl" file)))
              (directory-files-recursively ews-bibtex-directory "")))

(defun ews-bibtex-missing-files ()
  "List BibTeX attachments not listed in BibTeX files."
  (interactive)
  (let* ((files (ews--bibtex-extract-files))
         (attachments (ews--bibtex-extract-filenames))
         (missing (cl-remove-if
                   (lambda (f) (member f attachments)) files)))
    (message "%s files not registered in bibliography" (length missing))
    (dolist (file missing)
      (message "Missing file: %s" file))))

(defun ews-bibtex-missing-attachments ()
  "List BibTeX files without matching attachment."
  (interactive)
  (let* ((files (ews--bibtex-extract-files))
         (attachments (ews--bibtex-extract-filenames))
         (missing (cl-remove-if
                   (lambda (f) (member f files)) attachments)))
    (message "%s BibTeX files without matching attachment." (length missing))
    (dolist (file missing)
      (message "Missing file: %s" file))))

;; Denote
(defun ews-denote-assign-para ()
  "Move your note to either Project, Area, Reource or Archive (PARA)."
  (interactive)
  (if-let* ((file (buffer-file-name))
            ((denote-filename-is-note-p file))
            (all-keywords (string-split (denote-retrieve-filename-keywords file) "_"))
            (keywords (seq-remove (lambda (keyword)
                                    (member keyword ews-denote-para-keywords))
                                  all-keywords))
            (para (completing-read "Select category: " ews-denote-para-keywords))
            (new-keywords (push para keywords)))
      (denote-rename-file
       file
       (denote-retrieve-title-or-filename file (denote-filetype-heuristics file))
       new-keywords
       (denote-retrieve-filename-signature file))
    (message "Current buffer is not a Denote file.")))

;; Narrow Dired to Regular Expression
(defun ews-dired-narrow (selection)
  "Mark files in denote-firectory using a regular expression."
  (interactive "sMark files (regexp):")
  ;;  (dired denote-directory)
  (dired-mark-files-regexp selection)
  (dired-toggle-marks)
  (dired-do-kill-lines))

;; Distraction-free writing
;;;###autoload
(defun ews-olivetti ()
  "Distraction-free writing environment enhancing Olivetti mode."
  (interactive)
  (if (equal olivetti-mode nil)
      (progn
        (window-configuration-to-register 1)
        (delete-other-windows)
        (text-scale-set 1)
        (olivetti-mode t))
    (progn
      (if (eq (length (window-list)) 1)
          (jump-to-register 1))
      (olivetti-mode 0)
      (text-scale-set 0))))

;;;###autoload
(defun ews-org-insert-notes-drawer ()
  "Generate or open a NOTES drawer under the current heading.
If a drawer exists for this section, a new line is created at the end of the
current note."
  (interactive)
  (push-mark)
  (org-previous-visible-heading 1)
  (forward-line)
  (if (looking-at-p "^[ \t]*:NOTES:")
      (progn
        (org-fold-hide-drawer-toggle 'off)
        (re-search-forward "^[ \t]*:END:" nil t)
        (forward-line -1)
        (org-end-of-line)
        (org-return))
    (org-insert-drawer nil "NOTES"))
  (org-unlogged-message "Press <C-u C-SPACE> to return to the previous position."))

;;;###autoload
(defun ews-org-count-words ()
  "Add word count to each heading property drawer in an Org mode buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (let* ((start (point))
            (end (save-excursion (org-end-of-subtree)))
            (word-count (count-words start end)))
       (org-set-property "WORDCOUNT" (number-to-string word-count))))))

;;;###autoload
(defun ews-org-insert-screenshot ()
  "Take a screenshot with ImageMagick and insert as an Org mode link."
  (interactive)
  (let ((filename (read-file-name "Enter filename for screenshot: " default-directory)))
    (unless (string-equal "png" (file-name-extension filename))
      (setq filename (concat (file-name-sans-extension filename) ".png")))
    (call-process-shell-command (format "import %s" filename))
    (insert (format "#+caption: %s\n" (read-from-minibuffer "Caption: ")))
    (insert (format "[[file:%s]]" filename))
    (org-redisplay-inline-images)))

