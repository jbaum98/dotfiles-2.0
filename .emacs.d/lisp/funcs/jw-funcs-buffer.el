;;; jw-funcs-buffer.el --- Functions for dealing with buffers

;;; Commentary:
;;

;;; Code:

(defun jw/alternate-buffer ()
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (let ((current-buffer (window-buffer))
        (buffer-predicate
         (frame-parameter (window-frame) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun jw/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun jw/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun jw/goto-message-buffer ()
  "Switch to the message buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

;; our own implementation of kill-this-buffer from menu-bar.el
(defun jw/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix ARG is non-nil then also kill the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun jw/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)."
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)
    (prog-mode)))

(defun jw/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun jw/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defun jw/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer.
Create the *scratch* buffer first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(provide 'jw-funcs-buffer)
;;; jw-funcs-buffer.el ends here
