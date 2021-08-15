;; EMMS basic configuration
(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "~/Music/") ;; Change to your music folder

;; Choose one of these
(setq emms-info-functions '(emms-info-tinytag))  ;; When using Tinytag
;;(setq emms-info-functions '(emms-info-exiftool)) When using Exiftool

;; Load cover images
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)

;; Keyboard shortcuts
(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)
(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)

;; Helm EMMS
(use-package helm-emms
  :bind
  (("<C-f5>" . helm-emms)))
