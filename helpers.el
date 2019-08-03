(defun reload-emacs-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(require 'cl)
(defun concat-path (&rest parts)
  (reduce (lambda (a b) (expand-file-name b a)) parts)
  )

(defun ec-path (&rest parts)
  (apply 'concat-path (cons emacs-config parts))
  )

(provide 'helpers)
