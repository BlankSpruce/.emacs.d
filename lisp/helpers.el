(require 'ansi-color)

(defun reload-emacs-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

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

(defun bs/eval-line-at-point-and-insert-output ()
  (interactive)
  (insert
   (shell-command-to-string
    (delete-and-extract-region (point-at-bol) (point-at-eol)))))

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

(defun bs/eval-s-expression-and-insert-output ()
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1
       (eval (read (current-kill 0)))
       (current-buffer))
    (error
     (message "Invalid expression")
     (insert (current-kill 0)))))

(defun bs/chmod-this (mode)
  (interactive
   (let ((mode (read-file-modes "Mode: " (buffer-file-name))))
     (list mode)))
  (chmod (buffer-file-name) mode))

(defun bs/toggle-to-string (var)
  (if (and (boundp var) (symbol-value var))
      (propertize "✔" 'face '((t :foreground "lime green")))
    (propertize "✘" 'face '((t :foreground "dark red")))))

(defun bs/copy-whole-line ()
  (interactive)
  (save-excursion
    (let ((kill-read-only-ok t)
          (buffer-read-only t))
      (kill-whole-line nil))))

(defun bs/just-paste-last (times)
  (interactive "P")
  (dotimes (counter (or times 1))
    (yank)))
(put 'bs/just-paste-last 'delete-selection 'yank)

(defun bs/beginning-of-indentation-or-line ()
  (interactive)
  (let ((current-point (point)))
    (back-to-indentation)
    (when (equal (point) current-point)
      (beginning-of-line))))

(defun bs/do-nothing ()
  (interactive))

(defun bs/ryo-modal-mode-which-key ()
  (interactive)
  (which-key-show-keymap 'ryo-modal-mode-map))

(defun bs/copy-symbol-at-point ()
  (let ((thing (thing-at-point 'symbol)))
    (when thing
      (kill-new thing)
      (message "Copied: %s" (substring-no-properties thing)))))

(defun bs/copy-region-or-symbol-at-point ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (bs/copy-symbol-at-point)))

(defun bs/universal-argument ()
  (interactive)
  (call-interactively
   (if current-prefix-arg
       'universal-argument-more
     'universal-argument)))

(defun bs/kill-this-buffer ()
  (interactive)
  (when (yes-or-no-p "Kill this buffer? ")
    (kill-this-buffer)))

(defun bs/newline-above ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (indent-according-to-mode))

(defun bs/newline-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun bs/shell-command-on-region ()
  (interactive)
  (if (region-active-p)
      (let ((current-prefix-arg 4))
        (call-interactively 'shell-command-on-region))
    (call-interactively 'shell-command-on-region)))

(defun bs/deactivate-mark ()
  (interactive)
  (deactivate-mark))

(provide 'helpers)
