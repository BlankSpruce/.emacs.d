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
(winner-mode 1)

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

(use-package helpers
  :demand t
  :ensure nil

  :bind
  ([f5] . revert-buffer-without-confirmation)
  )

(use-package blankspruce
  :after helm-fd hydra helpers ialign markdown-preview-mode
  :demand t
  :ensure nil

  :hydra (hydra-miscellaneous (:exit t) ""
  ("a" ialign "Interactive align")
  ("f" helm-fd "Find file here")
  ("F" (lambda () (interactive) (helm-fd 1)) "Find file in selected directory")
  ("h" hl-line-mode "Line highlighting")
  ("l" nlinum-mode "Line numbers")
  ("m" hydra-markdown/body "Markdown preview")
  ("r" reload-emacs-config "Reload emacs config")
  ("t" toggle-truncate-lines "Toggle truncate lines")
  ("w" whitespace-cleanup "Cleanup whitespaces")
  )

  :config
  (setq-default cursor-type 'bar)
  (set-frame-parameter nil 'fullscreen 'maximized)
  (set-face-font 'default "Meslo LG S DZ for Powerline-12")
  (cua-mode)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (column-number-mode 1)
  (line-number-mode 1)
  (show-paren-mode)

  :custom
  (frame-title-format "[Emacs] %f")
  (frame-resize-pixelwise t)

  :bind
  ("C-/" . comment-line)

  :bind*
  ("C-a" . mark-whole-buffer)
  ("C-l" . goto-line)
  ("C-s" . save-buffer)
  ("C-/" . comment-line)
  ("M-o" . split-window-horizontally)
  ("M-e" . split-window-vertically)
  ("M-w" . delete-window)
  ("M-q" . kill-buffer)
  ("M-p" . recenter-top-bottom)
  ([f12] . hydra-miscellaneous/body)
  )

  )

;; Foreign packages
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

(use-package clang-format
  :after cc-mode

  :custom
  (clang-format-style "LLVM")

  :bind
  (:map c-mode-base-map
        ("C-;" . 'clang-format-buffer)
        )
  )

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
  :after hydra
  :ensure nil
  :config
  (defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue)
  )

  :bind
  (:map dired-mode-map
        ("." . hydra-dired/body)
        )
  )

(use-package doom-themes
  :config
  (if (display-graphic-p)
      (load-theme 'doom-city-lights t)
    (load-theme 'doom-sourcerer t)
    )
  (doom-themes-neotree-config)
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  )

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

  :bind
  ("<f6>" . 'helm-buffers-list)
  ("M-x" . 'helm-M-x)
  ("C-M-x" . 'execute-extended-command)
  ("C-x C-f" . 'helm-find-files)
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

  :hook
  (cc-mode . (lambda () (local-unset-key (kbd "M-k"))))
  )

(use-package helm-fd)

(use-package hl-anything
  :after hydra

  :config
  (defun my-hl-find-prev-thing ()
    (interactive)
    (hl-find-prev-thing)
    (deactivate-mark)
    )
  (defun my-hl-find-next-thing ()
    (interactive)
    (hl-find-next-thing)
    (deactivate-mark)
    )

  :hydra (hydra-highlight (:hint nil
                           :idle 1.0)
    "
^Navigation^      ^Local^ ^Global^          ^Others^       [highlights]
^----------^------^-----^-^------^----------^------^-------------------
[_,_] previous    [_h_]   [_H_] highlight   [_s_] save
[_._] next        [_c_]   [_C_] clear       [_r_] restore
^^^^                      [_T_] toggle
"
    ("," my-hl-find-prev-thing)
    ("." my-hl-find-next-thing)
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

(use-package ido
  :config
  (ido-mode t)
  )

(use-package lsp-mode
  :config
  (require 'lsp-clients)

  :hook (c++-mode . lsp)
  :commands lsp
  :custom (lsp-prefer-flymake nil)
  )

(setq-default lsp-clients-clangd-args
              '(
                "-j=1"
                "--background-index"
                "--hedear-insertion=iwyu"
                "--suggest-missing-includes"
                "--clang-tidy"
                )
              lsp-ui-doc-max-height 30
              lsp-ui-doc-max-width 120
              ;; lsp-ui-doc-use-webkit t
              lsp-ui-sideline-ignore-duplicate t
              lsp-ui-sideline-show-hover nil
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

  :hydra (hydra-magit (:exit t
                       :idle 1.0)
    "Magit"
    ("b" magit-blame-addition "blame")
    ("c" magit-checkout "checkout")
    ("h" magit-log-all "log")
    ("l" magit-log-buffer-file "file log")
    ("m" magit-file-rename "rename file")
    ("n" magit-branch-and-checkout "new branch from current one")
    ("r" magit-show-refs "show branches")
    ("s" magit-status "status")
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

(use-package neotree
  :custom
  (neo-theme (if (display-graphic-p) 'nerd 'ascii))
  (neo-smart-open t)
  (neo-window-fixed-size nil)
  (neo-window-width 30)

  :bind
  ("<f8>" . 'neotree-toggle)
  (:map neotree-mode-map
        ("c" . 'neotree-change-root)
        ("u" . 'neotree-select-up-node)
        )
  )

(use-package nlinum
  :custom
  (nlinum-format "%4d\u2502")
  (nlinum-highlight-current-line t)

  :hook
  (
   (prog-mode . nlinum-mode)
   (prog-mode . hl-line-mode)
   )
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

(use-package helm-swoop
  :after helm hydra projectile

  :custom
  (helm-swoop-split-window-function
        '(lambda (buffer &rest restvar)
           (helm-default-display-buffer buffer))
        )

  :hydra (hydra-helm-swoop (:exit t
                            :hint nil
                            :columns 3)
    ("a" helm-multi-swoop-all "All buffers")
    ("m" helm-multi-swoop "Multiple buffers")
    ("s" helm-multi-swoop-current-mode "All same major-mode buffers")
    ("p" helm-multi-swoop-projectile "All current project buffers")
    ("f" helm-swoop "Swoop with preinput")
    )

  :bind
  ("C-f" . 'helm-swoop-without-pre-input)
  ("M-f" . 'hydra-helm-swoop/body)
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

  :config
  (unbind-key "C-/" undo-tree-map)

  :hook
  (
   (text-mode . undo-tree-mode)
   (prog-mode . undo-tree-mode)
   )

  :bind
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
  :config
  (which-key-mode)
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
