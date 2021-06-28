;; Garbage collection threshold
(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(setq package-archives
      '(
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        )
      )
(prefer-coding-system 'utf-8)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose t)

(use-package use-package-hydra)

(defun bs/emacs-d (path)
  (expand-file-name path user-emacs-directory))

(add-to-list 'load-path (bs/emacs-d "lisp"))

;; Local packages
(use-package helpers
  :demand t
  :ensure nil
  )

(use-package blankspruce
  :demand t
  :ensure nil
  :after hydra

  :init
  (set-face-font 'default "MesloLGS NF-12")

  :config
  (setq-default cursor-type 'bar
                indent-tabs-mode nil)
  (column-number-mode 1)
  (delete-selection-mode 1)
  (display-line-numbers-mode 1)
  (show-paren-mode)
  (unless (file-exists-p custom-file)
    (with-temp-buffer (write-file custom-file)))
  (load custom-file)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  :custom
  (confirm-kill-emacs 'yes-or-no-p)
  (custom-file (bs/emacs-d "custom.el"))
  (frame-title-format "[Emacs] %f")
  (frame-resize-pixelwise t)
  (load-prefer-newer t)

  :hydra
  (hydra-miscellaneous
   (:exit t :columns 4)
   ""
   ("!" bs/eval-command-and-produce-nice-output "Eval command and produce nice output")
   ("a" ialign "Interactive align")
   ("c" bs/chmod-this "Chmod this")
   ("e" hydra-evals/body "Eval")
   ("F" format-all-buffer "Format all buffer")
   ("l" bs/reopen-file-literally "Reopen file literally")
   ("m" hydra-markdown/body "Markdown preview")
   ("o" hydra-origami/body "Origami")
   ("q" nil "Quit" :exit t)
   ("r" reload-emacs-config "Reload emacs config")
   ("S" edit-current-file-as-root "Edit current file as root")
   ("t" hydra-toggles/body "Toggles")
   ("w" whitespace-cleanup "Cleanup whitespaces")
   ("z" vterm "Vterm")
   ("Z" hydra-multi-vterm/body "Multiple vterms")
   ("?" which-key-show-major-mode "which-key major-mode")
   )

  (hydra-toggles
   (:hint none)
   "
[_c_] ?c? Color indentifiers   [_l_] ?l? Display line numbers   [_t_] ?t? Truncate long lines
[_f_] ?f? Follow               [_o_] ?o? Origami                [_E_] ?E? Debug on error
[_h_] ?h? Highlight line       [_w_] ?w? Whitespace             [_Q_] ?Q? Debug on quit         [_q_] quit"
   ("c" color-identifiers-mode (bs/toggle-to-string 'color-identifiers-mode))
   ("f" follow-mode (bs/toggle-to-string 'follow-mode))
   ("h" hl-line-mode (bs/toggle-to-string 'hl-line-mode))
   ("l" display-line-numbers-mode (bs/toggle-to-string 'display-line-numbers))
   ("o" origami-mode (bs/toggle-to-string 'origami-mode))
   ("w" whitespace-mode (bs/toggle-to-string 'whitespace-mode))
   ("t" toggle-truncate-lines (bs/toggle-to-string 'truncate-lines))
   ("E" toggle-debug-on-error (bs/toggle-to-string 'debug-on-error))
   ("Q" toggle-debug-on-quit (bs/toggle-to-string 'debug-on-quit))
   ("q" nil :exit t)
   )

  (hydra-evals
   (:hint nil :exit t)
   "
Elisp:                        ^^Shell:
[_b_] buffer                    [_c_] command
[_d_] defun                     [_l_] line and insert
[_s_] S-expression and insert   [_n_] command and produce nice output
[_r_] region                                                            [_q_] quit"
   ("b" eval-buffer)
   ("d" eval-defun)
   ("s" bs/eval-s-expression-and-insert-output)
   ("r" eval-region)
   ("c" shell-command)
   ("l" bs/eval-line-at-point-and-insert-output)
   ("n" bs/eval-command-and-produce-nice-output)
   ("q" nil))
  :bind*
  ("C-a" . mark-whole-buffer)
  ("C-s" . save-buffer)
  ("C-;" . comment-line)
  ("<C-return>" . rectangle-mark-mode)
  ("M-[" . switch-to-prev-buffer)
  ("M-]" . switch-to-next-buffer)
  ([f5] . revert-buffer-without-confirmation)
  ([f12] . hydra-miscellaneous/body)

  :commands (hydra-miscellaneous/body hydra-toggles/body hydra-evals/body)
  )

;; Foreign packages
(use-package ace-window
  :after hydra

  :config
  (ace-window-display-mode)
  (defun bs/winner-undo ()
    (interactive)
    (winner-undo)
    (setq this-command 'winner-undo))

  :hydra
  (hydra-windows
   (:hint nil :idle 1.0)
   "
Split:              ^^Winner:     ^^Other:
[_o_] horizontally    [_z_] undo    [_s_] swap      [_m_] toggle maximize
[_e_] vertically      [_Z_] redo    [_d_] delete    [_b_] balance            [_q_] quit
"
   ("o" split-window-horizontally :exit t)
   ("e" split-window-vertically :exit t)
   ("z" bs/winner-undo)
   ("Z" winner-redo :exit t)
   ("s" ace-swap-window :exit t)
   ("d" ace-delete-window :exit t)
   ("m" bs/maximize-window :exit t)
   ("b" balance-windows :exit t)
   ("q" nil)
   )

  :custom
  (aw-reverse-frame-list t)
  (aw-background nil)

  :commands (hydra-windows/body)
  )

(use-package all-the-icons)

(use-package avy
  :custom
  (avy-case-fold-search nil)
  )

(use-package cc-mode)

(use-package cmake-mode
  :custom
  (cmake-tab-width 4)
  )

(use-package color-identifiers-mode
  :hook
  (prog-mode . color-identifiers-mode)

  :custom
  (color-identifiers:timer (run-with-idle-timer 1.5 t 'color-identifiers:refresh))
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

  :config
  (use-package all-the-icons-dired
    :hook
    (dired-mode . all-the-icons-dired-mode)
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

  :custom
  (dired-listing-switches "-aGhlv --group-directories-first")

  :hook
  (dired-mode . (lambda () (interactive) (dired-hide-details-mode 1)))
  )

(use-package display-line-numbers
  :hook
  (
   (prog-mode . display-line-numbers-mode)
   (prog-mode . hl-line-mode)
   )
  )

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  )

(use-package doom-themes
  :config
  (if (display-graphic-p)
      (load-theme 'doom-city-lights t)
    (load-theme 'doom-sourcerer t)
    )
  )

(use-package elmacro)

(use-package esup
  :custom
  (esup-depth 0)

  :commands (esup)
  )

(use-package expand-region)

(use-package fill-column-indicator
  :custom
  (fci-rule-column 140)
  )

(use-package flycheck
  :hook
  (cc-mode . flycheck-mode)
  )

(use-package format-all
  :config
  (define-format-all-formatter gersemi
    (:executable "gersemi")
    (:install "pip install gersemi")
    (:languages "CMake")
    (:format (format-all--buffer-easy executable "-")))

  :commands (format-all-buffer)
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

(use-package graphviz-dot-mode
  :custom
  (graphviz-dot-indent-width 4)
  )

(use-package helm
  :defer t

  :custom
  (helm-always-two-windows nil)
  (helm-default-display-buffer-functions '(display-buffer-in-side-window))
  (helm-buffer-max-length 80)
  (helm-ff-lynx-style-map nil)
  (helm-mini-default-sources
   '(helm-source-buffers-list
     helm-source-bookmarks
     helm-source-recentf
     helm-source-buffer-not-found))

  :config
  (helm-mode 1)
  (defun bs/fuzzy-find-file ()
    (interactive)
    (helm-find t))

  :bind
  ("<f6>" . 'helm-mini)
  ([remap apropos-command] . 'helm-apropos)
  ([remap execute-extended-command] . 'helm-M-x)
  ([remap find-file] . 'helm-find-files)
  ([remap yank-pop] . 'helm-show-kill-ring)
  (:map helm-map
        ("<escape>" . 'helm-keyboard-quit)
        ("<left>" . 'backward-char)
        ("<right>" . 'forward-char)
        )
  (:map helm-find-files-map
        ("<C-backspace>" . 'backward-kill-word)
        )

  :commands (bs/fuzzy-find-file)
  )

(use-package helpful
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
  :hydra
  (hydra-highlight
   (:hint nil :idle 1.0)
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

  :commands (hydra-highlight/body)
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

  :config
  (use-package ccls
    :after lsp-mode
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
    (cc-mode . (lambda () (require 'ccls) (lsp)))
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
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))

  :hook
  (python-mode . lsp)

  :commands (lsp)
  )

(use-package magit
  :after hydra

  :config
  (setq dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles"))
        dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))
  (defun bs/dotfiles-magit-status ()
    (interactive)
    (add-to-list 'magit-git-global-arguments dotfiles-git-dir)
    (add-to-list 'magit-git-global-arguments dotfiles-work-tree)
    (call-interactively 'magit-status)
    )
  (defun bs/magit-status ()
    (interactive)
    (setq magit-git-global-arguments (remove dotfiles-git-dir magit-git-global-arguments))
    (setq magit-git-global-arguments (remove dotfiles-work-tree magit-git-global-arguments))
    (call-interactively 'magit-status)
    )
  (use-package vdiff-magit
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

  :hydra
  (hydra-magit
   (:exit t :idle 1.0)
   "Magit"
   ("a" magit-stage-file "stage file")
   ("b" magit-blame-addition "blame")
   ("c" magit-checkout "checkout")
   ("d" bs/dotfiles-magit-status "status (dotfiles)")
   ("h" magit-log-all "log")
   ("l" magit-log-buffer-file "file log")
   ("m" magit-file-rename "rename file")
   ("n" magit-branch-and-checkout "new branch from current one")
   ("r" magit-show-refs "show branches")
   ("s" bs/magit-status "status")
   )

  :commands (hydra-magit/body)
  )

(use-package markdown-mode
  :custom
  (markdown-command
   (concat "pandoc"
           " --mathjax"
           " --metadata pagetitle=\"...\""
           " --standalone"
           " --css=" (bs/emacs-d "markdown/github.css")
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

  :config
  (use-package markdown-preview-mode
    :hydra
    (hydra-markdown
     ()
     "Markdown"
     ("p" markdown-preview-mode "preview")
     ("c" markdown-preview-cleanup "cleanup")
     )

    :commands (hydra-markdown/body)
    )
  )

(use-package move-text)

(use-package multiple-cursors
  :custom
  (mc/list-file (bs/emacs-d "mc/.mc-lists.el"))

  :hydra
  (hydra-multiple-cursors
   (:hint nil)
   "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       ^^^^[_q_] Quit"
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
   ("q" mc/keyboard-quit :exit t))

  :bind
  (:map mc/keymap
        ("<return>" . nil))
  )

(use-package origami
  :config
  (defun bs/enable-origami ()
    (interactive)
    (unless origami-mode
      (origami-mode 1)))

  (defun bs/disable-origami ()
    (interactive)
    (origami-mode -1))

  :hydra
  (hydra-origami
   (:pre (bs/enable-origami)
         :color red
         :hint nil)
   "
_o_pen node    _n_ext fold       _t_oggle forward  _s_how current only  _q_uit
_c_lose node   _p_revious fold   toggle _a_ll      origami _r_eset      _Q_uit origami
  "
   ("o" origami-open-node)
   ("c" origami-close-node)
   ("n" origami-next-fold)
   ("p" origami-previous-fold)
   ("t" origami-forward-toggle-node)
   ("a" origami-toggle-all-nodes)
   ("s" origami-show-only-node)
   ("r" origami-reset)
   ("q" nil)
   ("Q" bs/disable-origami :exit t)
   )

  :commands (hydra-origami/body)
  )

(use-package projectile
  :config
  (projectile-mode)
  (when (executable-find "fd")
    (setq projectile-git-command "fd . -0")
    )
  (use-package helm-projectile
    :after helm

    :config
    (defun bs/helm-projectile-find-file-dwim ()
      (interactive)
      (if (not (projectile-project-p))
          (helm-projectile-switch-project)
        (helm-projectile-find-file)))
    )

  :custom
  (projectile-enable-caching t)
  (projectile-indexing-method 'hybrid)

  :commands (bs/helm-projectile-find-file-dwim projectile-command-map)
  )

(use-package restart-emacs)

(use-package rg
  :after hydra

  :config
  (use-package helm-ag
    :after hydra

    :custom
    (helm-ag-base-command "rg --line-number --no-heading --smart-case")
    (helm-ag-success-exit-status '(0 2))

    :bind
    (:map helm-ag-mode-map
          ("RET" . 'helm-ag-mode-jump-other-window)
          ("C-o" . 'helm-ag-mode-jump)
          )
    )

  (use-package wgrep
    :custom
    (wgrep-auto-save-buffer t)
    )

  :hydra
  (hydra-helm-ag
   (:exit t :hint nil :idle 1.0)
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

  :commands (hydra-helm-ag/body)
  )

(use-package rmsbolt)

(use-package savehist
  :config
  (savehist-mode)
  )

(use-package server
  :init
  (unless (server-running-p)
    (server-mode 1)
    (server-start))
  )

(use-package swiper
  :hydra
  (hydra-inside-swiper
   (:exit t)
   "Inside swiper"
   ("a" swiper-avy "Jump to candidate")
   ("m" swiper-mc "Multiple cursors")
   ("o" ivy-occur "Occur")
   ("r" swiper-recenter-top-bottom "Recenter")
   ("t" swiper-toggle-face-matching "Toggle face matching")
   )

  (hydra-swiper
   (:exit t)
   "Swiper"
   ("a" swiper-all "All buffers")
   ("f" swiper-isearch-thing-at-point "Thing at point")
   )

  :bind
  (:map swiper-map
        ("M-i" . 'hydra-inside-swiper/body)
        ("<escape>" . 'minibuffer-keyboard-quit)
        )
  )

(use-package term
  :defer t

  :config
  (setq-default term-scroll-show-maximum-output t)
  )

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  )

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)

  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-enable-undo-in-region t)

  :bind*
  ("C-z" . 'undo-tree-undo)
  ("C-S-z" . 'undo-tree-redo)
  ("M-z" . 'undo-tree-visualize)

  :commands (undo-tree-undo undo-tree-redo undo-tree-visualize)
  )

(use-package vdiff
  :after hydra

  :custom
  (vdiff-diff-algorithm 'git-diff-patience)
  (vdiff-subtraction-fill-char ?\?)

  :config
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  (define-key vdiff-3way-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  (defun bs/filter-vdiff-buffers-args (args)
    (let ((buffer-a (cl-first args))
          (buffer-b (cl-second args))
          (on-quit (cl-fourth args)))
      (list buffer-a buffer-b nil on-quit t nil)))

  (defun bs/filter-vdiff-buffers3-args (args)
    (let ((buffer-a (cl-first args))
          (buffer-b (cl-second args))
          (buffer-c (cl-third args))
          (on-quit (cl-fourth args)))
      (list buffer-a buffer-b buffer-c on-quit t nil)))

  (advice-add 'vdiff-buffers :filter-args 'bs/filter-vdiff-buffers-args)
  (advice-add 'vdiff-buffers3 :filter-args 'bs/filter-vdiff-buffers3-args)

  :hydra
  (hydra-vdiff-entry
   (:hint nil)
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
  (:map vdiff-mode-map ("M-i" . 'vdiff-hydra/body))
  (:map vdiff-3way-mode-map ("M-i" . 'vdiff-hydra/body))

  :commands (hydra-vdiff-entry/body)
  )

(use-package visual-regexp-steroids
  :custom
  (vr/engine 'python)
  )

(use-package vterm
  :config
  (use-package eterm-256color)
  (use-package multi-vterm)

  :custom
  (vterm-shell "zsh")

  :custom-face
  (vterm-color-red     ((t :foreground nil :background nil :inherit eterm-256color-red)))
  (vterm-color-green   ((t :foreground nil :background nil :inherit eterm-256color-green)))
  (vterm-color-yellow  ((t :foreground nil :background nil :inherit eterm-256color-yellow)))
  (vterm-color-blue    ((t :foreground nil :background nil :inherit eterm-256color-blue)))
  (vterm-color-magenta ((t :foreground nil :background nil :inherit eterm-256color-magenta)))
  (vterm-color-cyan    ((t :foreground nil :background nil :inherit eterm-256color-cyan)))

  :hook
  (vterm-mode . eterm-256color-mode)

  :hydra
  (hydra-multi-vterm
   (:exit nil :hint nil)
   ""
   ("z" multi-vterm "New term" :exit t)
   ("k" multi-vterm-prev "Previous term")
   ("j" multi-vterm-next "Next term")
   ("q" nil "Quit" :exit t)
   )

  :commands (vterm hydra-multi-vterm/body)
  )

(use-package which-key
  :hook (after-init . which-key-mode)

  :config
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)

  :bind
  (:map dired-mode-map ("." . which-key-show-major-mode))
  )

(use-package winner
  :init
  (winner-mode 1)
  )

(use-package yaml-mode)

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs (bs/emacs-d "yasnippet"))
  (use-package helm-c-yasnippet
    :custom
    (helm-yas-space-match-any-greedy t)

    :bind
    ("M-/" . 'helm-yas-complete)
    )

  (use-package yasnippet-snippets)

  (yas-reload-all)

  :hook
  (prog-mode . yas-minor-mode)
  )

(use-package ryo-modal
  :hook
  (markdown-mode . ryo-modal-mode)
  (prog-mode     . ryo-modal-mode)

  :bind*
  ("M-m" . ryo-modal-mode)

  :bind-keymap*
  ([f9] . ryo-modal-mode-map)

  :config
  (setq ryo-modal-cursor-color "green")
  (setq ryo-modal-cursor-type 'box)

  (ryo-modal-keys
   ("<left>" bs/do-nothing)
   ("<up>" bs/do-nothing)
   ("<down>" bs/do-nothing)
   ("<right>" bs/do-nothing)

   ("1" "C-1")
   ("2" "C-2")
   ("3" "C-3")
   ("4" "C-4")
   ("5" "C-5")
   ("6" "C-6")
   ("7" "C-7")
   ("8" "C-8")
   ("9" "C-9")
   ("0" "C-0")
   ("-" "C--")
   ("=" bs/universal-argument)

   ("!" bs/do-nothing)
   ("@" bs/do-nothing)
   ("#" bs/do-nothing)
   ("$" bs/do-nothing)
   ("%" bs/do-nothing)
   ("^" bs/do-nothing)
   ("&" bs/do-nothing)
   ("*" bs/do-nothing)
   ("(" backward-sexp)
   (")" forward-sexp)
   ("_" bs/do-nothing)
   ("+" bs/do-nothing)

   ("Q" keyboard-escape-quit)
   ("W" hydra-windows/body)
   ("E" er/contract-region)
   ("R" bs/do-nothing)
   ("T" avy-goto-line)
   ("Y" ryo-modal-repeat)
   ("U" bs/do-nothing)
   ("I" backward-paragraph)
   ("O" forward-paragraph)
   ("P" projectile-command-map)
   ("{" move-text-up)
   ("}" move-text-down)
   ("|" bs/shell-command-on-region)

   ("q" keyboard-quit)
   ("w" ace-window)
   ("e" er/expand-region)
   ("r" vr/query-replace)
   ("t" avy-goto-char-timer)
   ("y" ryo-modal-repeat)
   ("u" hydra-miscellaneous/body)
   ("i" previous-line)
   ("o" next-line)
   ("p" bs/helm-projectile-find-file-dwim)
   ("[" bs/do-nothing)
   ("]" bs/do-nothing)
   ("\\" bs/do-nothing)

   ("A" bs/do-nothing)
   ("S" write-file)
   ("D" bs/fuzzy-find-file)
   ("F" swiper-thing-at-point)
   ("G" hydra-helm-ag/body)
   ("H" bs/newline-above)
   ("J" backward-sentence)
   ("K" backward-word)
   ("L" forward-word)
   (":" forward-sentence)
   ("\"" bs/do-nothing)

   ("a" bs/do-nothing)
   ("s" save-buffer)
   ("d" helm-find-files)
   ("f" swiper)
   ("g" rg-dwim)
   ("h" bs/newline-below)
   ("j" bs/beginning-of-indentation-or-line)
   ("k" backward-char)
   ("l" forward-char)
   (";" end-of-line)
   ("'" bs/do-nothing)

   ("Z" undo-tree-redo)
   ("X" kill-whole-line)
   ("C" bs/copy-whole-line)
   ("V" helm-show-kill-ring)
   ("B" bs/do-nothing)
   ("N" bs/do-nothing)
   ("M" bs/deactivate-mark)
   ("<" beginning-of-buffer)
   (">" end-of-buffer)
   ("?" bs/ryo-modal-mode-which-key)

   ("z" undo-tree-undo)
   ("x" kill-region)
   ("c" bs/copy-region-or-symbol-at-point)
   ("v" bs/just-paste-last)
   ("b" cycle-spacing)
   ("n" bs/do-nothing)
   ("m" set-mark-command)
   ("," "M-,")
   ("." "M-.")
   ("/" comment-line)
   )

  (ryo-modal-key
   "SPC"
   '(
     ("M-m" ryo-modal-mode)

     ("q" quit-window)
     ("e" hydra-evals/body)
     ("r" rectangle-mark-mode)
     ("u" hydra-miscellaneous/body)
     ("o" hydra-origami/body)
     ("p" bs/helm-projectile-find-file-dwim)
     ("[" switch-to-prev-buffer)
     ("]" switch-to-next-buffer)

     ("d" hydra-vdiff-entry/body)
     ("f" hydra-swiper/body)
     ("g" hydra-magit/body)
     ("h" hydra-highlight/body)
     ("'" format-all-buffer)

     ("B"
      (
       ("q" bs/kill-this-buffer)
       ("b" kill-buffer)
       ))

     ("z" undo-tree-visualize)
     ("b" helm-mini)
     ("n"
      (
       ("q" nil)
       ("d" narrow-to-defun)
       ("n" narrow-to-region)
       ("p" narrow-to-page)
       ("w" widen)
       ))
     ("m" hydra-multiple-cursors/body)
     ))
  )

;; Garbage collection threshold lower after loading packages
(setq gc-cons-threshold (* 2 1000 1000))
