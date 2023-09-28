;;; merge.el --- Emacs Writing Studio: merge modules into init file

;;; Commentary:
;; Merges the Emacs Writing Studio modules into an init.el file.

;;; Code:

(let ((file-list '("ews-packages.el"
		   "ews-user-interface.el"
		   "ews-completion.el"
		   "ews-ebooks.el"
		   "ews-bibtex.el"
		   "ews-elfeed.el"
	           "ews-emms.el"
		   "ews-denote.el"
		   "ews-citar-denote.el"
		   "ews-writing.el"
		   "ews-rice-org.el"
		   "ews-export.el"
		   "ews-hugo.el"
		   "ews-files.el"))
      (output-file "init.el"))
  ;; Create init.el
  (delete-file output-file)
  (with-temp-file output-file)
  ;; Append each file content to init.el
  (with-temp-buffer
    (dolist (file file-list)
      (insert-file-contents (concat "modules/" file))
      (goto-char (point-max))
      (insert "\n"))
    (insert "\n;;; init.el ends here")
    (goto-char (point-min))
    (while (re-search-forward "/\\*\\|\\*/" nil t)
      (replace-match "" t t))
    (write-file output-file))
  (copy-file output-file (concat "config/" output-file) t))

;;; merge.el ends here
