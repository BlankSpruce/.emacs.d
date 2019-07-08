(when (display-graphic-p)
  ;; Cursor
  (setq-default cursor-type 'bar)

  ;; Font
  (add-to-list 'default-frame-alist
               '(font . "Meslo LG M DZ for Powerline-9"))

  ;; Others
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  )

(if (display-graphic-p)
    (progn
      (menu-bar-mode 1)
      )
  (menu-bar-mode -1)
  )

;; Highlight matching parentheses
(show-paren-mode)

