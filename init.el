;; Melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives
	       '("gnu" . "http://elpa.gnu.org/packages/") t))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Packages
(defvar dynamic-packages
  '(
    clang-format
    dashboard
    magit
    move-text
    neotree
    pcre2el
    projectile
    solarized-theme
    undo-tree
    visual-regexp-steroids
    ))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      dynamic-packages)

;; Reload config
(defun reload-emacs-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; UI
(load-file "~/.emacs.d/config/ui.el")

;; Keybindings
(load-file "~/.emacs.d/config/keybindings.el")

;; Regexp
(setq vr/engine 'python)
