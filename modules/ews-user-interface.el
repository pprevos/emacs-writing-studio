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
