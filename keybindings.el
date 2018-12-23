;;;; Keybindings
;; Keybinding helper
(defun set-keybinding (keybinding action)
  (define-key global-map (kbd keybinding) action)
  )

;; Reload config
(set-keybinding "<f12>" 'reload-emacs-config)

;; Copy/Cut/Paste
(cua-mode t)

;; Save file
(set-keybinding "C-s" 'save-buffer)

;; Close file
(set-keybinding "C-w" 'kill-buffer)

;; Open buffers
(set-keybinding "<f11>" 'buffer-menu-other-window)

;; Go to line
(set-keybinding "C-l" 'goto-line)

;; Undo/redo
(global-undo-tree-mode)
(set-keybinding "C-z" 'undo-tree-undo)
(set-keybinding "C-S-z" 'undo-tree-redo)
(set-keybinding "M-z" 'undo-tree-visualize)

;; Search and replace
(set-keybinding "C-f" 'isearch-forward)
(set-keybinding "C-r" 'query-replace)
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)

;; Regexp
(require 'visual-regexp-steroids)
(set-keybinding "M-f" 'vr/isearch-forward)
(set-keybinding "M-r" 'vr/query-replace)

;; Move text
(require 'move-text)
(set-keybinding "<C-S-up>" 'move-text-up)
(set-keybinding "<C-S-down>" 'move-text-down)

;; Comment/uncomment
(define-key undo-tree-map (kbd "C-/") nil)
(set-keybinding "C-/" 'comment-line)

;;;; Windows
;; Switch windows
(set-keybinding "<M-left>" 'windmove-left)
(set-keybinding "<M-right>" 'windmove-right)
(set-keybinding "<M-up>" 'windmove-up)
(set-keybinding "<M-down>" 'windmove-down)

;; Split window
(set-keybinding "M-o" 'split-window-horizontally)
(set-keybinding "M-e" 'split-window-vertically)

;; Close window
(set-keybinding "M-w" 'delete-window)

;; Kill buffer
(set-keybinding "M-q" 'kill-buffer)

;;;; Git
(global-unset-key (kbd "M-g"))
(set-keybinding "M-g b" 'magit-blame)
(set-keybinding "M-g s" 'magit-status)
(set-keybinding "M-g l" 'magit-log-buffer-file)
(set-keybinding "M-g c" 'magit-blame-copy-hash)

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

;;;; Neotree
(require 'neotree)
(set-keybinding "<f8>" 'neotree-toggle)
(define-key neotree-mode-map (kbd "c") 'neotree-change-root)
(define-key neotree-mode-map (kbd "u") 'neotree-select-up-node)

;;;; Dashboard
(set-keybinding "<f10>"
		'(lambda () (interactive) (switch-to-buffer dashboard-buffer-name)))

;;;; Projectile
(global-unset-key (kbd "C-k"))
(set-keybinding "C-k f" 'projectile-find-file)
