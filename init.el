;; Garbage collection threshold
(setq gc-cons-threshold (* 50 1000 1000))

;; Ask before exit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Melpa
(prefer-coding-system 'utf-8)
(defconst emacs-config (expand-file-name user-emacs-directory))

;; Ghostscript
(if (eq system-type 'windows-nt)
    (setq doc-view-ghostscript-program "gswin64c")
  )

;; Customize file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (not (file-exists-p custom-file))
    (with-temp-buffer (write-file custom-file))
  )
(load custom-file)

;; Spaces
(setq-default indent-tabs-mode nil)

;; Prefer newer files
(setq load-prefer-newer t)

(require 'package)
(setq package-archives
      '(
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade"    . "https://marmalade-repo.org/packages/")
        )
      )
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t
      )
(use-package use-package-hydra)

;; Local packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package esup
  :config
  (setq esup-depth 0)
  )

(use-package helpers
  :demand t
  :ensure nil
  )

(use-package blankspruce
  :after helm hydra helpers ialign markdown-preview-mode origami which-key
  :demand t
  :ensure nil

  :hydra
  (hydra-miscellaneous
   (:exit t :columns 4)
   ""
   ("a" ialign "Interactive align")
   ("e" eval-and-replace "Eval and replace")
   ("f" bs/fuzzy-find-file "Fuzzy find file")
   ("F" format-all-buffer)
   ("h" hl-line-mode "Line highlighting")
   ("l" display-line-numbers-mode "Line numbers")
   ("m" hydra-markdown/body "Markdown preview")
   ("o" hydra-origami/body "Origami")
   ("r" reload-emacs-config "Reload emacs config")
   ("S" edit-current-file-as-root "Edit current file as root")
   ("t" toggle-truncate-lines "Toggle truncate lines")
   ("w" whitespace-cleanup "Cleanup whitespaces")
   ("?" which-key-show-major-mode "which-key major-mode")
   )

  :config
  (setq-default cursor-type 'bar)
  (set-frame-parameter nil 'fullscreen 'maximized)
  (set-face-font 'default "MesloLGS NF-11")
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (column-number-mode 1)
  (delete-selection-mode 1)
  (display-line-numbers-mode 1)
  (show-paren-mode)
  (defun bs/fuzzy-find-file ()
    (interactive)
    (helm-find t))

  :custom
  (frame-title-format "[Emacs] %f")
  (frame-resize-pixelwise t)

  :bind*
  ("C-a" . mark-whole-buffer)
  ("C-l" . goto-line)
  ("C-s" . save-buffer)
  ("C-;" . comment-line)
  ("<C-return>" . rectangle-mark-mode)
  ("M-q" . kill-buffer)
  ("M-p" . recenter-top-bottom)
  ([f5] . revert-buffer-without-confirmation)
  ([f12] . hydra-miscellaneous/body)
  )

;; Foreign packages
(use-package ace-window
  :after winner

  :config
  (ace-window-display-mode)
  (defun bs/winner-undo ()
    (progn
      (winner-undo)
      (setq this-command 'winner-undo)
      )
    )

  :custom
  (aw-reverse-frame-list t)
  (aw-dispatch-always t)
  (aw-dispatch-alist
   '(
     (?o split-window-horizontally)
     (?e split-window-vertically)
     (?w delete-window)

     (?b balance-windows)
     (?m delete-other-windows "Maximize window")

     (?d ace-delete-window)
     (?s ace-swap-window)

     (?j bs/winner-undo)
     (?k winner-redo)
     )
   )
  (aw-background nil)

  :bind*
  ("M-o" . ace-window)
  )

(use-package rg)

(use-package wgrep-rg
  :ensure nil
  :after rg hydra

  :hydra (hydra-wgrep-rg (:exit t
                          :idle 1.0)
    "wgrep-rg"
    ("e" wgrep-change-to-wgrep-mode "edit")
    ("f" wgrep-finish-edit "finish edits")
    ("s" wgrep-save-all-buffers "save edits")
    )

  :bind
  (:map rg-mode-map
        ("M-i" . 'hydra-wgrep-rg/body))
  (:map wgrep-mode-map
        ("M-i" . 'hydra-wgrep-rg/body))
  )

(use-package cc-mode)

(use-package color-identifiers-mode
  :config
  (global-color-identifiers-mode)

  :custom
  (color-identifiers:timer (run-with-idle-timer 1.5 t 'color-identifiers:refresh))
  )

(use-package cmake-mode
  :custom
  (cmake-tab-width 4)
  )

(use-package company
  :hook
  (
   (c++-mode        . company-mode)
   (cmake-mode      . company-mode)
   (emacs-lisp-mode . company-mode)
   (python-mode     . company-mode)
   )

  :bind
  (:map company-mode-map
        ("C-<SPC>" . 'company-complete)
        )
  )

(use-package compile
  :after helpers

  :custom
  (compilation-scroll-output 'first-error)

  :hook (compilation-filter . colorize-compilation-buffer)

  :bind
  ([f2]   . compile)
  ([C-f2] . recompile)
  )

(use-package dashboard
  :custom
  (dashboard-items
        '(
          (recents   . 10)
          (projects  . 10)
          (bookmarks . 10)
          )
        )

  :config
  (dashboard-setup-startup-hook)

  :bind
  ("<f7>" . (lambda () (interactive) (switch-to-buffer dashboard-buffer-name)))
  )

(use-package dired
  :ensure nil
  )

(use-package dired-sidebar
  :bind
  ([f8] . dired-sidebar-toggle-sidebar)
  )

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("<tab>"           . dired-subtree-toggle)
        ("<C-tab>"         . dired-subtree-cycle)
        ("<S-iso-lefttab>" . dired-subtree-remove)
        )
  )

(use-package doom-themes
  :config
  (if (display-graphic-p)
      (load-theme 'doom-city-lights t)
    (load-theme 'doom-sourcerer t)
    )
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  )

(use-package elmacro)

(use-package elpy
  :defer t

  :init
  (advice-add 'python-mode :before 'elpy-enable)

  :config
  ; Conflicts with windmove
  (unbind-key "<M-up>"    elpy-mode-map)
  (unbind-key "<M-down>"  elpy-mode-map)
  (unbind-key "<M-left>"  elpy-mode-map)
  (unbind-key "<M-right>" elpy-mode-map)

  :hook
  (
   (elpy-mode   . (lambda () (highlight-indentation-mode -1)))
   (python-mode . elpy-mode)
   )

  :bind
  (:map elpy-mode-map
        ("C-<SPC>" . 'elpy-company-backend)
        )
  )

(use-package expand-region
  :bind
  ("M-@" . 'er/expand-region)
  ("M-#" . 'er/contract-region)
  )

(use-package fill-column-indicator
  :custom
  (fci-rule-column 140)
  )

(use-package flycheck
  :after cc-mode

  :hook
  (
   (c-mode-common . flycheck-mode)
   )
  )

(use-package format-all)

(use-package fzf)

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode)
  (setq-default fringes-outside-margins t)
  (setq git-gutter-fr:side 'left-fringe)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  )

(use-package graphviz-dot-mode
  :custom
  (graphviz-dot-indent-width 4)
  )

(use-package helm
  :custom
  (helm-always-two-windows nil)
  (helm-display-buffer-display-height 16)
  (helm-default-display-buffer-functions '(display-buffer-in-side-window))
  (helm-buffer-max-length 80)
  (helm-ff-lynx-style-map nil)

  :config
  (with-eval-after-load "helm-files"
    (bind-key "C-<backspace>" #'backward-kill-word helm-find-files-map)
    )
  (helm-mode 1)

  :bind
  ("<f6>" . 'helm-mini)
  ([remap apropos-command] . 'helm-apropos)
  ([remap execute-extended-command] . 'helm-M-x)
  ([remap find-file] . 'helm-find-files)
  ([remap yank-pop] . 'helm-show-kill-ring)
  (:map helm-map
        ("<left>" . 'backward-char)
        ("<right>" . 'forward-char)
        )
  )

(use-package helm-ag
  :after rg hydra

  :custom
  (helm-ag-base-command "rg --line-number --no-heading --smart-case")
  (helm-ag-success-exit-status '(0 2))

  :hydra (hydra-helm-ag (:exit t
                         :hint nil
                         :idle 1.0)
    "
^^              ^Interactive^       ^Current^           [Ag Search]
----------------------------------------------------
[_j_] just rg   [_b_] buffers       [_B_] buffers
[_k_] dwim      [_d_] directory     [_D_] directory
[_l_] project   [_f_] file          [_F_] file
^^              [_p_] project       [_P_] project"
    ("j" rg)
    ("k" rg-dwim)
    ("l" rg-project)
    ("b" helm-do-ag-buffers)
    ("d" helm-do-ag)
    ("f" helm-do-ag-this-file)
    ("p" helm-do-ag-project-root)
    ("B" helm-ag-buffers)
    ("D" helm-ag)
    ("F" helm-ag-this-file)
    ("P" helm-ag-project-root)
    )

  :bind
  ("M-k" . 'hydra-helm-ag/body)
  (:map helm-ag-mode-map
        ("RET" . 'helm-ag-mode-jump-other-window)
        ("C-o" . 'helm-ag-mode-jump)
        )

  :hook
  (cc-mode . (lambda () (local-unset-key (kbd "M-k"))))
  )

(use-package helpful
  :after helm

  :custom
  (helm-describe-function-function 'helpful-symbol)
  (helm-describe-variable-function 'helpful-symbol)

  :bind*
  ([remap describe-key] . 'helpful-key)
  ([remap describe-function] . 'helpful-callable)
  ([remap describe-variable] . 'helpful-variable)
  ([remap Info-goto-emacs-command-node] . 'helpful-function)
  ("<f1> ." . 'helpful-at-point)
  )

(use-package hl-anything
  :after hydra

  :hydra (hydra-highlight (:hint nil
                           :idle 1.0)
    "
^Navigation^      ^Local^ ^Global^          ^Others^       [highlights]
^----------^------^-----^-^------^----------^------^-------------------
[_,_] previous    [_h_]   [_H_] highlight   [_s_] save
[_._] next        [_c_]   [_C_] clear       [_r_] restore
[_q_] quit        ^^      [_T_] toggle
"
    ("," hl-find-prev-thing)
    ("." hl-find-next-thing)
    ("q" (lambda () (interactive) (deactivate-mark)) :exit t)
    ("h" hl-highlight-thingatpt-local)
    ("c" hl-unhighlight-all-local)
    ("H" hl-highlight-thingatpt-global)
    ("C" hl-unhighlight-all-global)
    ("T" hl-global-highlight-on/off)
    ("s" hl-save-highlights)
    ("r" hl-restore-highlights)
    )

  :custom
  (highlight-symbol-idle-delay 0.7)

  :hook
  (
   (text-mode . hl-highlight-mode)
   (prog-mode . hl-highlight-mode)
   )

  :bind
  ("M-h" . 'hydra-highlight/body)
  )

(use-package hydra)

(use-package ialign)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

(use-package lsp-mode
  :custom (lsp-prefer-flymake nil)
  )

(use-package ccls
  :custom
  (ccls-executable "/usr/bin/ccls")
  (ccls-initialization-options
   '(:index
     (:comments 0 :threads 2 :initialBlackList ".")
     :completion
     (:detailedLabel t)
     )
   )

  :hook
  ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp)))
  )

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-flycheck-enable nil)
  (lsp-ui-sideline-enable nil)

  :bind
  (:map lsp-ui-mode-map
        ("C-," . xref-pop-marker-stack)
        ("C-." . lsp-ui-peek-find-definitions)
        ("C-?" . lsp-ui-peek-find-references)
        )
  )

(use-package company-lsp
  :after company
  :config
  (push 'company-lsp company-backends)

  :custom
  (company-lsp-async t)
  (company-lsp-cache-candidates 'auto)
  )

(use-package magit
  :after hydra

  :custom
  (dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles")))
  (dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))

  :config
  (defun dotfiles-magit-status ()
    (interactive)
    (add-to-list 'magit-git-global-arguments dotfiles-git-dir)
    (add-to-list 'magit-git-global-arguments dotfiles-work-tree)
    (call-interactively 'magit-status)
    )
  (defun my-magit-status ()
    (interactive)
    (setq magit-git-global-arguments (remove dotfiles-git-dir magit-git-global-arguments))
    (setq magit-git-global-arguments (remove dotfiles-work-tree magit-git-global-arguments))
    (call-interactively 'magit-status)
    )

  :hydra (hydra-magit (:exit t
                       :idle 1.0)
    "Magit"
    ("a" magit-stage-file "stage file")
    ("b" magit-blame-addition "blame")
    ("c" magit-checkout "checkout")
    ("d" dotfiles-magit-status "status (dotfiles)")
    ("h" magit-log-all "log")
    ("l" magit-log-buffer-file "file log")
    ("m" magit-file-rename "rename file")
    ("n" magit-branch-and-checkout "new branch from current one")
    ("r" magit-show-refs "show branches")
    ("s" my-magit-status "status")
    )

  :bind
  ("M-g" . 'hydra-magit/body)
  (:map magit-blame-read-only-mode-map
        ("c" . 'magit-blame-copy-hash))
  )

(use-package markdown-mode
  :custom
  (markdown-command
        (concat "pandoc"
                " --mathjax"
                " --metadata pagetitle=\"...\""
                " --standalone"
                " --css=" (ec-path "markdown" "github.css")
                " --self-contained"
                " --from=markdown --to=html5"
                " --highlight-style=pygments"
                )
        )
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-preview-stylesheets (list))

  :custom-face
  (markdown-header-face   ((t (:inherit font-lock-function-name-face :weight bold))))
  (markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
  (markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.7))))
  (markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
  (markdown-table-face    ((t (:inherit font-lock-variable-name-face))))
  )

(use-package markdown-preview-mode
  :after hydra

  :hydra (hydra-markdown ()
    "Markdown"
    ("p" markdown-preview-mode "preview")
    ("c" markdown-preview-cleanup "cleanup")
    )
  )

(use-package move-text
  :bind
  ("<C-S-up>" . 'move-text-up)
  ("<C-S-down>" . 'move-text-down)
  )

(use-package multi-term
  :custom
  (multi-term-program "zsh")

  :config
  (defun multi-term-bind-keys ()
    (dolist
        (bind
         '(("C-<backspace>" . term-send-backward-kill-word)
           ("C-<delete>" . term-send-forward-kill-word)
           ("C-<left>" . term-send-backward-word)
           ("C-<right>" . term-send-forward-word)
           ("C-c C-j" . term-line-mode)
           ("C-c C-k" . term-char-mode)
           ("C-c C-v" . scroll-up)
           ("C-c C-z" . term-stop-subjob)
           ("C-c C-r" . term-send-reverse-search-history)
           ("M-DEL" . term-send-backward-kill-word)
           ("M-d" . term-send-forward-kill-word)
           ("M-r" . term-send-reverse-search-history)
           )
         )
      (add-to-list 'term-bind-key-alist bind)
      )
    )

  :hook
  (term-mode . multi-term-bind-keys)
  )

(use-package multiple-cursors
  :after hydra

  :custom
  (mc/list-file (ec-path "mc" ".mc-lists.el"))

  :hydra (hydra-multiple-cursors (:hint nil)
    "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("s" mc/mark-all-in-region-regexp :exit t)
    ("0" mc/insert-numbers :exit t)
    ("A" mc/insert-letters :exit t)
    ("<mouse-1>" mc/add-cursor-on-click)
    ;; Help with click recognition in this hydra
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q" nil))

  :bind
  ("M-n" . 'hydra-multiple-cursors/body)
  (:map mc/keymap
        ("<return>" . nil))
  )

(use-package display-line-numbers
  :hook
  (
   (prog-mode . display-line-numbers-mode)
   (prog-mode . hl-line-mode)
   )
  )

(use-package origami
  :config
  (global-origami-mode)

  :hydra
  (hydra-origami
   (:color red)
   "
_o_pen node    _n_ext fold       _t_oggle forward  _s_how current only
_c_lose node   _p_revious fold   toggle _a_ll      origami _r_eset
  "
   ("o" origami-open-node)
   ("c" origami-close-node)
   ("n" origami-next-fold)
   ("p" origami-previous-fold)
   ("t" origami-forward-toggle-node)
   ("a" origami-toggle-all-nodes)
   ("s" origami-show-only-node)
   ("r" origami-reset)
   )
  )

(use-package projectile
  :after rg
  :config
  (projectile-global-mode)
  (when (executable-find "fd")
    (setq projectile-git-command "fd . -0")
    )

  :custom
  (projectile-enable-caching t)
  (projectile-indexing-method 'hybrid)
  )

(use-package helm-projectile
  :after helm projectile helm-ag

  :config
  (defun actual-helm-projectile-find-file-dwim ()
    (interactive)
    (if (not (projectile-project-p))
        (helm-projectile-switch-project)
      (helm-projectile-find-file)
      )
    )

  :bind
  ("C-k" . 'actual-helm-projectile-find-file-dwim)
  )

(use-package restart-emacs)

(use-package rmsbolt)

(use-package savehist
  :config
  (savehist-mode)
  )

(use-package server
  :init
  (server-mode 1)
  (unless (server-running-p)
    (server-start)
    )

  :bind
  ("C-x y" . 'server-edit)
  )

(use-package swiper
  :hydra
  (hydra-inside-swiper
   (:exit t)
   "Inside swiper"
   ("a" swiper-avy "Jump to candidate")
   ("m" swiper-mc "Multiple cursors")
   ("r" swiper-recenter-top-bottom "Recenter")
   ("t" swiper-toggle-face-matching "Toggle face matching")
   )

  (hydra-swiper
   (:exit t)
   "Swiper"
   ("a" swiper-all "All buffers")
   ("f" swiper-thing-at-point "Thing at point")
   )

  :bind
  ("C-f" . 'swiper)
  ("M-f" . 'hydra-swiper/body)
  (:map swiper-map
        ("M-i" . 'hydra-inside-swiper/body)
        )
  )

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode)
  )

(use-package term
  :config
  (setq-default term-scroll-show-maximum-output t)
  )

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  )

(use-package undo-tree
  :custom
  (undo-tree-visualizer-timestamps t)

  :hook
  (
   (text-mode . undo-tree-mode)
   (prog-mode . undo-tree-mode)
   )

  :bind*
  ("C-z" . 'undo-tree-undo)
  ("C-S-z" . 'undo-tree-redo)
  ("M-z" . 'undo-tree-visualize)
  )

(use-package vdiff
  :after hydra

  :custom
  (vdiff-diff-algorithm 'git-diff-patience)
  (vdiff-subtraction-fill-char ?\?)

  :config
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  (define-key vdiff-3way-mode-map (kbd "C-c") vdiff-mode-prefix-map)

  :hydra (hydra-vdiff-entry (:hint nil)
    "
 2-way^^              3-way^^              Miscellaneous
------------------------------------------------------------------------
 [_b_] Diff 2 buffers [_B_] Diff 3 buffers [_c_] Diff current file
 [_f_] Diff 2 files   [_F_] Diff 3 files   [_m_] Resolve merge conflict"
    ("b" vdiff-buffers)
    ("B" vdiff-buffers3)
    ("f" vdiff-files)
    ("F" vdiff-files3)
    ("c" vdiff-current-file)
    ("m" vdiff-merge-conflict)
    )

  :bind
  ("M-d" . #'hydra-vdiff-entry/body)

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
  :custom
  (vr/engine 'python)

  :bind
  ("M-r" . 'vr/query-replace)
  )

(use-package which-key
  :after dired
  :hook (after-init . which-key-mode)

  :bind
  (:map dired-mode-map ("." . which-key-show-major-mode))
  )

(use-package winner
  :init
  (winner-mode 1)
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
  (add-to-list 'yas-snippet-dirs (ec-path "yasnippet"))
  (yas-reload-all)

  :hook
  (
   (c++-mode    . yas-minor-mode)
   (python-mode . yas-minor-mode)
   )
  )

(use-package yasnippet-snippets)

(use-package helm-c-yasnippet
  :after yasnippet

  :custom
  (helm-yas-space-match-any-greedy t)

  :bind
  ("M-/" . 'helm-yas-complete)
  )

;; Garbage collection threshold lower after loading packages
(setq gc-cons-threshold (* 2 1000 1000))
