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
