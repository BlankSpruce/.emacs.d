;;;; Keybindings
;; Keybinding helper
(defun set-keybinding (keybinding action)
  (define-key global-map (kbd keybinding) action)
  )

;; Copy/Cut/Paste
(cua-mode t)

;; Buffers
(set-keybinding "C-s" 'save-buffer)
(set-keybinding "<C-f6>" 'buffer-menu)
(set-keybinding "<M-f6>" 'buffer-menu-other-window)
(set-keybinding "M-o" 'split-window-horizontally)
(set-keybinding "M-e" 'split-window-vertically)
(set-keybinding "M-w" 'delete-window)
(set-keybinding "M-q" 'kill-buffer)
(set-keybinding "M-p" 'reposition-window)
; Reload buffer
(defun revert-buffer-without-confirmation ()
    (interactive) (revert-buffer t t))
(set-keybinding "<f5>" 'revert-buffer-without-confirmation)

;; Go to line
(set-keybinding "C-l" 'goto-line)

;; Search and replace
(set-keybinding "C-f" 'isearch-forward)
(set-keybinding "C-r" 'query-replace)
(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)

;; Comment/uncomment
(set-keybinding "C-/" 'comment-line)

;; Select all
(set-keybinding "C-a" 'mark-whole-buffer)

