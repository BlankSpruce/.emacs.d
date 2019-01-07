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
	       '("gnu" . "http://elpa.gnu.org/packages/") t)
)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
)

;; Packages
(defvar dynamic-packages
  '(
    dashboard
    doom-themes
    elpy
    latex-preview-pane
    magit
    markdown-mode
    move-text
    neotree
    pcre2el
    projectile
    undo-tree
    vdiff
    vdiff-magit
    visual-regexp-steroids
    )
)

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      dynamic-packages)

;; Reload config
(defun reload-emacs-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Load configs
(defvar config-files
  '(
    "diff.el"
    "keybindings.el"
    "ui.el"
   )
  )

(mapc #'(lambda (single-file)
	  (load-file (concat "~/.emacs.d/config/" single-file)))
      config-files)

;; Regexp
(setq vr/engine 'python)

;; Ghostscript
(if (eq system-type 'windows-nt)
    (setq doc-view-ghostscript-program "gswin64c")
  )

;; Python
(add-hook 'python-mode-hook (lambda () (elpy-mode)))
