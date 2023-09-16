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
      (concat "{{</* ref \"" target "\" */>}}"))))

;; New link type for Org-Hugo internal links
(org-link-set-parameters
 "hugo"
 :complete #'ews-hugo-link-complete)
