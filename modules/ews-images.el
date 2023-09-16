;; Define external image viewer/editor
(setq image-dired-external-viewer "/usr/bin/gimp")

;; Image-dired Keyboard shortcuts
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "C-t C-d") 'image-dired)
    (define-key dired-mode-map (kbd "C-<return>") 'image-dired-dired-display-external))
