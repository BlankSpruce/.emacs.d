;;;; Keybindings
;; Copy/Cut/Paste
(cua-mode t)

;; Keybinding helper
(defun set-keybinding (keybinding action)
  (global-unset-key (kbd keybinding))
  (global-set-key (kbd keybinding) action)
  )

;; Open/Create file
(global-unset-key (kbd "C-k"))
(set-keybinding "C-k f" 'find-file)

;; Save file
(set-keybinding "C-s" 'save-buffer)

;; Close file
(set-keybinding "C-w" 'kill-buffer)

;; Go to line
(set-keybinding "C-l" 'goto-line)

;; Undo/redo
(global-undo-tree-mode)
(set-keybinding "C-z" 'undo-tree-undo)
(set-keybinding "C-S-z" 'undo-tree-redo)
(set-keybinding "M-z" 'undo-tree-visualize)

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

