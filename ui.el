;; Theme
(require 'solarized)
(load-theme 'solarized-dark t)

;; Cursor
(setq-default cursor-type 'bar)

;; Line numbers
(global-linum-mode)
(setq linum-format "%4d\u2502")

;; Font
;(set-face-attribute 'default t :font "Meslo LG M DZ for Powerline-9")
(add-to-list 'default-frame-alist
	     '(font . "Meslo LG M DZ for Powerline-9"))

;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)

;; File tree
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'nerd 'ascii))
(setq neo-smart-open t)

; Disable line numbering in neotree
(add-hook 'neo-after-create-hook
	  (lambda (&optional dummy) (linum-mode -1)))

;; Others
(tool-bar-mode -1)
(scroll-bar-mode -1)
