;;;; Keybindings
;; Keybinding helper
(defun set-keybinding (keybinding action)
  (define-key global-map (kbd keybinding) action)
  )

(defun disable-keybinding (mode keybinding)
  (define-key mode (kbd keybinding) nil)
  )

;; Copy/Cut/Paste
(cua-mode t)

;; Buffers
(set-keybinding "C-s" 'save-buffer)
(set-keybinding "<C-f6>" 'buffer-menu)
(set-keybinding "<M-f6>" 'buffer-menu-other-window)
(set-keybinding "<f6>" 'ido-switch-buffer)
(set-keybinding "M-o" 'split-window-horizontally)
(set-keybinding "M-e" 'split-window-vertically)
(set-keybinding "M-w" 'delete-window)
(set-keybinding "M-q" 'kill-buffer)
(set-keybinding "M-p" 'reposition-window)
; Move to other buffer
(require 'windmove)
(set-keybinding "<M-left>" 'windmove-left)
(set-keybinding "<M-right>" 'windmove-right)
(set-keybinding "<M-up>" 'windmove-up)
(set-keybinding "<M-down>" 'windmove-down)
; Reload buffer
(defun revert-buffer-without-confirmation ()
    (interactive) (revert-buffer t t))
(set-keybinding "<f5>" 'revert-buffer-without-confirmation)

;; Go to line
(set-keybinding "C-l" 'goto-line)

;; Undo/redo
(require 'undo-tree)
(global-undo-tree-mode)
(set-keybinding "C-z" 'undo-tree-undo)
(set-keybinding "C-S-z" 'undo-tree-redo)
(set-keybinding "M-z" 'undo-tree-visualize)
(disable-keybinding undo-tree-map "C-/")

;; Search and replace
(set-keybinding "C-f" 'isearch-forward)
(set-keybinding "C-r" 'query-replace)
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)

; Regexp
(require 'visual-regexp-steroids)
(set-keybinding "M-f" 'vr/isearch-forward)
(set-keybinding "M-r" 'vr/query-replace)

;; Move text
(require 'move-text)
(set-keybinding "<C-S-up>" 'move-text-up)
(set-keybinding "<C-S-down>" 'move-text-down)

;; Comment/uncomment
(set-keybinding "C-/" 'comment-line)

;; Select all
(set-keybinding "C-a" 'mark-whole-buffer)

;; magit
(require 'magit)
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
(global-unset-key (kbd "M-g"))
(set-keybinding "M-g" 'hydra-magit/body)
(define-key magit-blame-read-only-mode-map "c" 'magit-blame-copy-hash)

;; vdiff + magit intergration
(require 'vdiff)
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
(define-key vdiff-3way-mode-map (kbd "C-c") vdiff-mode-prefix-map)

(require 'vdiff-magit)
(define-key magit-mode-map "e" 'vdiff-magit-dwim)
(define-key magit-mode-map "E" 'vdiff-magit-popup)
(setcdr (assoc ?e (plist-get magit-dispatch-popup :actions))
        '("vdiff dwim" 'vdiff-magit-dwim))
(setcdr (assoc ?E (plist-get magit-dispatch-popup :actions))
        '("vdiff popup" 'vdiff-magit-popup))

;; Neotree
(require 'neotree)
(set-keybinding "<f8>" 'neotree-toggle)
(define-key neotree-mode-map (kbd "c") 'neotree-change-root)
(define-key neotree-mode-map (kbd "u") 'neotree-select-up-node)

;; Dashboard
(require 'dashboard)
(set-keybinding "<f7>"
		'(lambda () (interactive) (switch-to-buffer dashboard-buffer-name)))

;; Ag
(require 'ag)
(set-keybinding "C-S-k" 'ag)

;; Projectile
(require 'projectile)
(global-unset-key (kbd "C-k"))
(set-keybinding "C-k" 'projectile-find-file)
(global-unset-key (kbd "M-k"))
(set-keybinding "M-k" 'projectile-ag)

;; Elpy
(require 'elpy)
(define-key elpy-mode-map (kbd "C-<SPC>") 'elpy-company-backend)
; Conflicts with windmove
(defvar elpy-keybindings-to-disable
  '(
    "<M-up>"
    "<M-down>"
    "<M-left>"
    "<M-right>"
    )
  )
(mapc #'(lambda (keybinding) (disable-keybinding elpy-mode-map keybinding)) elpy-keybindings-to-disable)

;; Company
(require 'company)
(define-key company-mode-map (kbd "C-<SPC>") 'company-complete)

;; Origami
(require 'origami)
(define-key origami-mode-map (kbd "<C-tab>") 'origami-recursively-toggle-node)

;; Markdown
(require 'markdown-preview-mode)
(defhydra hydra-markdown ()
  "Markdown"
  ("p" markdown-preview-mode "preview")
  ("c" markdown-preview-cleanup "cleanup")
  )
(global-unset-key (kbd "M-m"))
(set-keybinding "M-m" 'hydra-markdown/body)
