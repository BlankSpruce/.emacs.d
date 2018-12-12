;; Theme
(require 'solarized)
(load-theme 'solarized-dark t)

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

;; Others
(tool-bar-mode -1)
(scroll-bar-mode -1)
