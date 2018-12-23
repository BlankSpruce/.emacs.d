;; Custom algorithm
(require 'vdiff)
(setq vdiff-diff-algorithm 'git-diff)
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

;; Colors
(custom-set-faces
 '(diff-added ((t (:background "dark green" :foreground "medium spring green"))))
 '(diff-removed ((t (:background "IndianRed4" :foreground "IndianRed1"))))
 '(diff-changed ((t (:background "midnight blue" :foreground "deep sky blue"))))
)
