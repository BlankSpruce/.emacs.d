;;;; Keybindings
;; Copy/Cut/Paste
(cua-mode 1)

;; Open/Create file
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k f") 'find-file)

;; Save file
(global-set-key (kbd "C-s") 'save-buffer)

;; Close file
(global-set-key (kbd "C-w") 'kill-buffer)

;; Go to line
(global-set-key (kbd "C-l") 'goto-line)
