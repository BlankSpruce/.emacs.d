;; Melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "marmalade" (concat proto "://marmalade-repo.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
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
    pcre2el
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
