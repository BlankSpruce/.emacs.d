(require 'ansi-color)

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

(defun insert-output-of-executed-line ()
  "executes line at point in default shell and inserts stdout"
  (interactive)
  (insert
   (shell-command-to-string
    (delete-and-extract-region
     (point-at-bol)
     (point-at-eol)
     )
    )
   )
  )

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only)
  )

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename)
      )
    )
  )

(provide 'helpers)
