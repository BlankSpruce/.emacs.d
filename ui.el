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

;; Theme
(require 'doom-themes)
(load-theme 'doom-city-lights t)

;; Line numbers
(setq nlinum-format "%4d\u2502")
(setq nlinum-highlight-current-line t)
(global-nlinum-mode)
(global-hl-line-mode)
(require 'elpy)
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((recents  . 10)
                        (projects . 10)
                        (bookmarks . 10)))

;; File tree
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'nerd 'ascii))
(setq neo-smart-open t)

; Disable line numbering in neotree
(add-hook 'neo-after-create-hook (lambda (&rest _) (nlinum-mode -1)))

;; Highlight matching parentheses
(show-paren-mode)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-column 140)
