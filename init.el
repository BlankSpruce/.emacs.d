;; Melpa
(prefer-coding-system 'utf-8)
(defconst emacs-config (expand-file-name "config" user-emacs-directory))

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

;; Local packages
(use-package helpers
  :demand t
  :load-path "config/"

  :bind
  ([f5] . revert-buffer-without-confirmation)
  )

(use-package keybindings
  :demand t
  :load-path "config/"

  :config
  (cua-mode)

  :bind*
  ("C-a" . mark-whole-buffer)
  ("C-l" . goto-line)
  ("C-s" . save-buffer)
  ("C-/" . comment-line)
  ("M-o" . split-window-horizontally)
  ("M-e" . split-window-vertically)
  ("M-w" . delete-window)
  ("M-q" . kill-buffer)
  ("M-p" . reposition-window)
  )

(use-package ui
  :load-path "config/"
  )

(use-package ttcn-3-mode
  :load-path "config/static/ttcn-3-mode"
  )

;; Foreign packages
(use-package ag
  :bind
  ("C-S-k" . 'ag)
  )
(use-package wgrep-ag
  :after ag hydra

  :config
  (defhydra hydra-wgrep-ag (:exit t
                            :idle 1.0)
    "wgrep-ag"
    ("e" wgrep-change-to-wgrep-mode "edit")
    ("f" wgrep-finish-edit "finish edits")
    ("s" wgrep-save-all-buffers "save edits")
    )

  :bind
  (:map ag-mode-map
        ("M-i" . 'hydra-wgrep-ag/body))
  (:map wgrep-mode-map
        ("M-i" . 'hydra-wgrep-ag/body))
  )

(use-package cc-mode)

(use-package clang-format
  :after cc-mode

  :bind
  (:map c-mode-base-map
        ("C-;" . 'clang-format-buffer)
        )
  )

(use-package cmake-mode
  :config
  (setq cmake-tab-width 4)
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
  :bind
  ([f2] . compile)
  )

(use-package dashboard
  :init
  (setq dashboard-items
        '(
          (recents   . 10)
          (projects  . 10)
          (bookmarks . 10)
          )
        )

  :config
  (dashboard-setup-startup-hook)

  :hook
  (dashboard-mode . (lambda (&rest _) (nlinum-mode -1)))

  :bind
  ("<f7>" . (lambda () (interactive) (switch-to-buffer dashboard-buffer-name)))
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
  :config
  (setq fci-rule-column 140)
  )

(use-package flycheck
  :after cc-mode

  :hook
  (
   (c-mode-common . flycheck-mode)
   )
  )

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
  :config
  (setq helm-always-two-windows nil
        helm-display-buffer-display-height 16
        helm-default-display-buffer-functions '(display-buffer-in-side-window)
        helm-buffer-max-length 80
        helm-ff-lynx-style-map nil
        )
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
        ("<tab>" . 'helm-execute-persistent-action)
        ("C-z" . 'helm-select-action)
        )
  )

(use-package helm-ag
  :after ag hydra

  :config
  (defhydra hydra-helm-ag (:exit t
                           :hint nil
                           :idle 1.0)
    "
 ^Interactive^       ^Current^           [Ag Search]
----------------------------------------------------
 [_b_] buffers       [_B_] buffers
 [_d_] directory     [_D_] directory
 [_f_] file          [_F_] file
 [_p_] project       [_P_] project"
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
  ("M-a" . 'hydra-helm-ag/body)
  )

(use-package highlight-symbol
  :after hydra

  :config
  (defhydra hydra-highlight-symbol (:hint nil
                                    :idle 1.0)
    "
 ^Navigation^       ^Current symbol^     ^Miscellaneous^          [highlight-symbol]^
-------------------------------------------------------------------------------
 [_,_] previous     [_h_] highlight      [_c_] clear highlights
 [_._] next         [_r_] replace"
    ("," highlight-symbol-prev)
    ("." highlight-symbol-next)
    ("c" highlight-symbol-remove-all)
    ("h" highlight-symbol)
    ("r" highlight-symbol-query-replace)
    )
  (setq highlight-symbol-idle-delay 0.7)

  :hook
  (
   (text-mode . highlight-symbol-mode)
   (prog-mode . highlight-symbol-mode)
   )

  :bind
  ("M-h" . 'hydra-highlight-symbol/body)
  )

(use-package hydra
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

(use-package ialign)

(use-package ido
  :config
  (ido-mode t)
  )

(use-package magit
  :after hydra

  :config
  (defhydra hydra-magit (:exit t
                         :idle 1.0)
    "Magit"
    ("b" magit-blame-addition "blame")
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
  (setq markdown-command
        (concat "pandoc"
                " --mathjax"
                " --metadata pagetitle=\"...\""
                " --standalone"
                " --css=" (ec-path "markdown" "github.css")
                " --self-contained"
                " --from=markdown --to=html5"
                " --highlight-style=pygments"
         )
        markdown-fontify-code-blocks-natively t
        markdown-header-scaling t
        markdown-preview-stylesheets (list)
        )

  :custom-face
  (markdown-header-face   ((t (:inherit font-lock-function-name-face :weight bold))))
  (markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
  (markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.7))))
  (markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
  (markdown-table-face    ((t (:inherit font-lock-variable-name-face))))
  )

(use-package markdown-preview-mode
  :after hydra

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

(use-package multiple-cursors
  :after hydra

  :config
  (setq mc/list-file (ec-path "mc" ".mc-lists.el"))
  (defhydra hydra-multiple-cursors (:hint nil)
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
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd 'ascii)
        neo-smart-open t
        neo-window-fixed-size nil
        neo-window-width 30
        )

  :hook
  (
   ;; Disable line numbering in neotree
   (neo-after-create . (lambda (&rest _) (nlinum-mode -1)))
   )

  :bind
  ("<f8>" . 'neotree-toggle)
  (:map neotree-mode-map
        ("c" . 'neotree-change-root)
        ("u" . 'neotree-select-up-node)
        )
  )

(use-package nlinum
  :config
  (setq nlinum-format "%4d\u2502"
        nlinum-highlight-current-line t
        )
  (global-nlinum-mode)
  (global-hl-line-mode)
  )

(use-package rtags
  :init
  (setq rtags-completions-enabled t)
  (rtags-start-process-unless-running)

  :bind
  (:map c-mode-base-map
        ("M-." . 'rtags-find-symbol-at-point)
        ("M-," . 'rtags-find-references-at-point)
        )
  )

(use-package company-rtags
  :after company cc-mode rtags

  :config
  (push 'company-rtags company-backends)
  )

(use-package flycheck-rtags
  :after cc-mode flycheck rtags

  :config
  (defun flycheck-with-rtags-setup ()
    (flycheck-select-checker 'rtags)
    ;; RTags create more accurate overlays
    (setq-local flycheck-highlighting-mode nil)
    (setq-local flycheck-check-syntax-automatically nil)
    )

  :hook
  (
   (c-mode-common . flycheck-with-rtags-setup)
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

(use-package popwin
  :config
  (popwin-mode 1)
  (setq popwin:popup-window-height 0.25
        popwin:special-display-config nil
        )
  (push '(compilation-mode
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config)
  (push '(" *undo-tree*"
          :dedicated t
          :position bottom
          :stick t
          :noselect nil)
        popwin:special-display-config)
  (push '(ag-mode
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config)
  )

(use-package projectile
  :after ag
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
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
  ("M-k" . 'helm-projectile-ag)
  )

(use-package helm-swoop
  :load-path "config/static/helm-swoop/"

  :after helm hydra projectile

  :init
  (setq helm-swoop-split-window-function
        '(lambda (buffer &rest restvar)
           (helm-default-display-buffer buffer)))

  :config
  (defhydra hydra-helm-swoop (:exit t
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

  :hook (server-done . suspend-frame)

  :bind
  ("C-x y" . 'server-edit)
  )

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
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
  :after hydra

  :config
  (setq vdiff-diff-algorithm 'git-diff-patience
        vdiff-subtraction-fill-char ?
        )
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  (define-key vdiff-3way-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  (defhydra hydra-vdiff-entry (:hint nil)
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
  :config
  (setq vr/engine 'python)

  :bind
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

  :config
  (setq helm-yas-space-match-any-greedy t)

  :bind
  ("M-/" . 'helm-yas-complete)
  )
