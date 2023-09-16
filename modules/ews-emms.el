;; Emacs Multimedia System configuration
(use-package emms
  :init
  (require 'emms-setup)
  (require 'emms-empris)
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

python -m tinytag ~/Music/Frank_Zappa_1969_Hot_Rats/02_Willie_the_Pimp.mp3

;; Choose one of these
(setq emms-info-functions '(emms-info-tinytag))  ;; When using Tinytag
;;(setq emms-info-functions '(emms-info-exiftool)) When using Exiftool

;; Load cover images
(setq emms-browser-covers 'emms-browser-cache-thumbnail-async)

;; Helm EMMS
(use-package helm-emms
  :bind
  (("<C-f5>" . helm-emms)))
