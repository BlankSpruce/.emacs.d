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

(defun bs/loud-copy (string &optional format-string)
  (kill-new string)
  (message (or format-string "%s") string))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (bs/loud-copy filename "Copied buffer file name '%s' to the clipboard."))))

(defun bs/copy-file-location ()
  (interactive)
  (let ((copy (lambda (result) (bs/loud-copy result "Copied: %s")))
        (resolve-path
         (lambda (path) (file-relative-name path (projectile-project-root)))))
    (if (equal major-mode 'dired-mode)
        (funcall copy (funcall resolve-path default-directory))
      (let ((name (buffer-file-name)))
        (when name
          (funcall
           copy
           (format "%s:%d" (funcall resolve-path name) (line-number-at-pos))))))))

(defun bs/eval-command-and-produce-nice-output (command)
  (interactive "sCommand: ")
  (let* ((command-result (shell-command-to-string command))
         (result (format "$ %s\n%s" command command-result)))
    (bs/loud-copy result)))

(defun eval-and-replace ()
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1
       (eval (read (current-kill 0)))
       (current-buffer)
       )
    (error
     (message "Invalid expression")
     (insert (current-kill 0))
     )
    )
  )

(defun bs/chmod-this (mode)
  (interactive
   (let ((mode (read-file-modes "Mode: " (buffer-file-name))))
     (list mode)))
  (chmod (buffer-file-name) mode))

(defun bs/toggle-to-string (var)
  (if (and (boundp var) (symbol-value var))
      (propertize "✔" 'face '((t :foreground "lime green")))
    (propertize "✘" 'face '((t :foreground "dark red")))))

(provide 'helpers)
