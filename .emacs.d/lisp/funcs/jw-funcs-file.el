;;; jw-funcs-file.el --- Functions for dealing with files

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (add-to-list 'load-path "../core")
  (require 'jw-core-lib))

(autoload 'projectile-invalidate-cache "projectile")
(autoload 'dired-get-file-for-visit "dired")
(autoload 'recentf-remove-if-non-kept "recentf")

(defun jw/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

(defun jw/delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (fboundp 'projectile-project-p) (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

(defun jw/find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun jw/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used to set ARG then open the
folder containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (jw//open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (jw//open-in-external-app file-path)
        (message "No file associated to this buffer.")))))

(defun jw//open-in-external-app (file-path)
  "Open FILE-PATH in external application."
  (cond
   (IS-LINUX (let ((process-connection-type nil))
               (start-process "" nil "xdg-open" file-path)))
   (IS-DARWIN (shell-command (format "open \"%s\"" file-path)))
   (IS-WINDOWS
    (and (fboundp 'w32-shell-execute)
         (w32-shell-execute
          "open"
          (replace-regexp-in-string "/" "\\\\" file-path))))))

(defun jw/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (when (and (fboundp 'projectile-project-p)
                          (projectile-project-p))
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun jw/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun jw/sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'jw-funcs-file)
;;; jw-funcs-file.el ends here
