(require 'vdiff)
(setq vdiff-diff-algorithm 'git-diff-patience)

;; Custom layout
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

;; Empty lines
(setq vdiff-subtraction-fill-char ? )

;; Colors
(when (display-graphic-p)
  (custom-set-faces
   '(diff-added   ((t (:background "#335533" :foreground "#ddffdd"))))
   '(diff-removed ((t (:background "#553333" :foreground "#ffdddd"))))
   '(diff-changed ((t (:background "#333355" :foreground "#ddddff"))))
   )
  )
