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
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; Load configs
(defvar config-files
  '(
    "helpers.el"
    "keybindings.el"
    "ui.el"
   )
  )
(mapc #'(lambda (single-file)
	  (load-file (concat "~/.emacs.d/config/" single-file)))
      config-files)

;; Ghostscript
(if (eq system-type 'windows-nt)
    (setq doc-view-ghostscript-program "gswin64c")
  )

;; Packages
(use-package ag
  :bind
  ("C-S-k" . 'ag)
  )
(use-package wgrep-ag
  :requires ag hydra

  :config
  (defhydra hydra-ag ()
    "Ag"
    ("e" wgrep-change-to-wgrep-mode "edit")
    ("f" wgrep-finish-edit "finish edits")
    ("s" wgrep-save-all-buffers "save edits")
    )

  :bind
  (:map ag-mode-map
	("M-i" . 'hydra-ag/body))
  (:map wgrep-mode-map
	("M-i" . 'hydra-ag/body))
  )

(use-package company
  :bind
  (:map company-mode-map
	("C-<SPC>" . 'company-complete)
	)
  )

(use-package dashboard
  :init
  (setq dashboard-items '((recents  . 10)
			  (projects . 10)
			  (bookmarks . 10)))

  :config
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook (lambda (&rest _) (nlinum-mode -1)))

  :bind
  ("<f7>" . (lambda () (interactive) (switch-to-buffer dashboard-buffer-name)))
  )

(use-package doom-themes
  :config
  (load-theme 'doom-city-lights t)
  )

(use-package elpy
  :config
  (add-hook 'python-mode-hook #'elpy-mode)
  ; Conflicts with windmove
  (unbind-key "<M-up>" elpy-mode-map)
  (unbind-key "<M-down>" elpy-mode-map)
  (unbind-key "<M-left>" elpy-mode-map)
  (unbind-key "<M-right>" elpy-mode-map)
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

  :bind
  (:map elpy-mode-map
	("C-<SPC>" . 'elpy-company-backend)
	)
  )

(use-package fill-column-indicator
  :config
  (setq fci-rule-column 140)
  )

(use-package hydra)

(use-package ido
  :config
  (ido-mode t)

  :bind
  ("<f6>" . 'ido-switch-buffer)
  )

(use-package magit
  :requires hydra

  :config
  (defhydra hydra-magit ()
    "Magit"
    ("b" magit-blame "blame")
    ("c" magit-checkout "checkout")
    ("h" magit-log-all "log")
    ("l" magit-log-buffer-file "file log")
    ("n" magit-branch-and-checkout "new branch from current one")
    ("r" magit-show-refs-popup "show branches")
    ("s" magit-status "status")
    )

  :bind
  ("M-g" . 'hydra-magit/body)
  (:map magit-blame-read-only-mode-map
	("c" . 'magit-blame-copy-hash))
  )

(use-package markdown-mode
  :init
  (setq markdown-css-filepath
	(concat (getenv "HOME") "/.emacs.d/config/markdown/github.css")
	)
  (setq markdown-command
      (concat
       "pandoc"
       " --mathjax"
       " --metadata pagetitle=\"...\""
       " --standalone"
       " --css=" markdown-css-filepath
       " --self-contained"
       " --from=markdown --to=html5"
       " --highlight-style=pygments"
       )
      )
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-header-scaling t)
  (setq markdown-preview-stylesheets (list))

  :custom-face
  (markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold))))
  (markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
  (markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.7))))
  (markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
  )

(use-package markdown-preview-mode
  :requires hydra

  :config
  (defhydra hydra-markdown ()
    "Markdown"
    ("p" markdown-preview-mode "preview")
    ("c" markdown-preview-cleanup "cleanup")
    )

  :bind
  ("M-m" . 'hydra-markdown/body)
  )

(use-package move-text
  :bind
  ("<C-S-up>" . 'move-text-up)
  ("<C-S-down>" . 'move-text-down)
  )

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd 'ascii))
  (setq neo-smart-open t)
  ;; Disable line numbering in neotree
  (add-hook 'neo-after-create-hook (lambda (&rest _) (nlinum-mode -1)))

  :bind
  ("<f8>" . 'neotree-toggle)
  (:map neotree-mode-map
	("c" . 'neotree-change-root)
	("u" . 'neotree-select-up-node)
	)
  )

(use-package nlinum
  :config
  (setq nlinum-format "%4d\u2502")
  (setq nlinum-highlight-current-line t)
  (global-nlinum-mode)
  (global-hl-line-mode)
  )

(use-package origami
  :config
  (global-origami-mode)

  :bind
  (:map origami-mode-map
	("<C-tab>" . 'origami-recursively-toggle-node)
	)
  )

(use-package projectile
  :requires ag
  :config
  (projectile-global-mode)

  :bind
  ("C-k" . 'projectile-find-file)
  ("M-k" . 'projectile-ag)
  )

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (unbind-key "C-/" undo-tree-map)

  :bind
  ("C-z" . 'undo-tree-undo)
  ("C-S-z" . 'undo-tree-redo)
  ("M-z" . 'undo-tree-visualize)
  )

(use-package vdiff
  :config
  (setq vdiff-diff-algorithm 'git-diff-patience)
  (defun custom-vdiff-3way-layout-function (buffer-a buffer-b buffer-c)
    (delete-other-windows)
    (switch-to-buffer buffer-a)
    (set-window-buffer (split-window-horizontally) buffer-c)
    (set-window-buffer (split-window-horizontally) buffer-b)
    (balance-windows)
    )
  (custom-set-variables
   '(vdiff-3way-layout-function (quote custom-vdiff-3way-layout-function))
   )
  (setq vdiff-subtraction-fill-char ? )

  :bind
  (:map vdiff-mode-map
	("C-c" . 'vdiff-mode-prefix-map))
  (:map vdiff-3way-mode-map
	("C-c" . 'vdiff-mode-prefix-map))

  :custom-face
  (diff-added   ((t (:background "#335533" :foreground "#ddffdd"))))
  (diff-removed ((t (:background "#553333" :foreground "#ffdddd"))))
  (diff-changed ((t (:background "#333355" :foreground "#ddddff"))))
  )

(use-package vdiff-magit
  :after magit vdiff

  :config
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)

  :bind
  (:map magit-mode-map
	("e" . 'vdiff-magit-dwim)
	("E" . 'vdiff-magit-popup))
  )

(use-package visual-regexp-steroids
  :config
  (setq vr/engine 'python)

  :bind
  ("M-f" . 'vr/isearch-forward)
  ("M-r" . 'vr/query-replace)
  )

(use-package windmove
  :bind
  ("<M-left>" . 'windmove-left)
  ("<M-right>" . 'windmove-right)
  ("<M-up>" . 'windmove-up)
  ("<M-down>" . 'windmove-down)
  )

(use-package yaml-mode)

(use-package yasnippet
  :config
  (add-hook 'c++-mode-hook #'yas-minor-mode)
  (add-hook 'python-mode-hook #'yas-minor-mode)
  (setq yas-snippet-dirs (append yas-snippet-dirs '("~/.emacs.d/config/yasnippet")))
  (yas-reload-all)
  )

(use-package yasnippet-snippets)

(use-package helm-c-yasnippet
  :requires yasnippet
  :config
  (setq helm-yas-space-match-any-greedy t)

  :bind
  ("M-/" . 'helm-yas-complete)
  )
