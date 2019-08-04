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

(defun revert-buffer-without-confirmation ()
  (interactive)
  (revert-buffer t t)
  )

(defun edit-current-file-as-root ()
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (find-file (concat "/sudo::" file))
      (message "Buffer not associated with file")
      )
    )
  )

(defun open-zsh ()
  (ansi-term "zsh" "zsh")
  )

(provide 'helpers)
