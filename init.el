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
    ag
    company
    dashboard
    doom-themes
    elpy
    fill-column-indicator
    helm-c-yasnippet
    hydra
    hlinum
    latex-preview-pane
    magit
    markdown-mode
    markdown-preview-mode
    move-text
    neotree
    origami
    pcre2el
    projectile
    undo-tree
    vdiff
    vdiff-magit
    visual-regexp-steroids
    wgrep-ag
    yaml-mode
    yasnippet
    yasnippet-snippets
    )
)

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      dynamic-packages)

;; Load configs
(defvar config-files
  '(
    "diff.el"
    "helpers.el"
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

;; Markdown
(setq markdown-css-filepath
      (concat
       (getenv "HOME")
       "/.emacs.d/config/markdown"
       "/github.css"
       )
      )
(setq markdown-command
      (concat
       "pandoc"
       " --mathjax"
       " --metadata pagetitle=\"...\""
       " --standalone"
       " --css="
       markdown-css-filepath
       " --self-contained"
       " --from=markdown --to=html5"
       " --highlight-style=pygments"
       )
      )
(setq markdown-fontify-code-blocks-natively t)
(setq markdown-header-scaling t)
(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4)))))
(setq markdown-preview-stylesheets (list))

;; Origami
(global-origami-mode)

;; Ido
(require 'ido)
(ido-mode t)

;; Fill column indicator
(require 'fill-column-indicator)
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq fci-rule-column 140)
	    (fci-mode)))

;; YASnippet
(require 'yasnippet)
(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(setq yas-snippet-dirs (append yas-snippet-dirs
	     '("~/.emacs.d/config/yasnippet")))
(yas-reload-all)
(add-hook 'c++-mode-hook (lambda () (yas-minor-mode)))
(add-hook 'python-mode-hook (lambda () (yas-minor-mode)))

