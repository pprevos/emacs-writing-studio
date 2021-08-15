;; Update files with last modifed date, when #+lastmod: is available
(setq time-stamp-active t
      time-stamp-start "#\\+lastmod:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "%04Y-%02m-%02d")
(add-hook 'before-save-hook 'time-stamp nil)

;; New link type for Org-Hugo internal links
(org-link-set-parameters "hugo"
                         :complete (lambda ()
                                     (concat "{{%/* ref "(file-relative-name (read-file-name "File: "))" */%}}")))
