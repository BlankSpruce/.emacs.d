(when (display-graphic-p)
  ;; Cursor
  (setq-default cursor-type 'bar)

  ;; Font
  (add-to-list 'default-frame-alist
               '(font . "Meslo LG S DZ for Powerline-11"))

  ;; Maximize
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Title bar
  (setq frame-title-format "[Emacs] %f")

  ;; Others
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (column-number-mode 1)
  (line-number-mode 1)

  (setq frame-resize-pixelwise t)
  )

(when (not (display-graphic-p))
  (menu-bar-mode -1)
  )

;; Highlight matching parentheses
(show-paren-mode)

(provide 'ui)
