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
