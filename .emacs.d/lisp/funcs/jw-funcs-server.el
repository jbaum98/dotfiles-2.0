;;; jw-funcs-server --- Functions to help manage Emacs server

;;; Commentary:

;;; Code:

(defun jw//is-mac-gui-frame (frame)
  "Return non-nil if FRAME is a Mac GUI frame."
  (memq (framep frame) '(ns mac)))

(defun jw//mac-hide-emacs ()
  "On a Mac, hide Emacs."
  (interactive)
  (do-applescript (mapconcat 'identity '("tell application \"System Events\" to"
                                         "tell process \"Emacs\" to set visible to false") " ")))

(defun jw//frame-is-last-ns-frame (frame)
  "Return t if FRAME is the only NS frame."
  (and
   ;; Frame is ns frame
   (jw//is-mac-gui-frame frame)
   ;; No other frames on same terminal
   (<= (length (filtered-frame-list
                (lambda (frm) (eq (frame-terminal frm)
                             (frame-terminal frame)))))
       1)))

(defun jw//keep-at-least-one-ns-frame (&optional frame force)
  "If FRAME is the last NS frame, open a new hidden NS frame.
This is called immediately prior to FRAME being closed."
  (let ((frame (or frame (selected-frame))))
    (when (jw//frame-is-last-ns-frame frame)
      (let ((sf (selected-frame)))
        (select-frame frame)
        (make-frame)
        (switch-to-buffer "*scratch*")
        (select-frame sf))
      ;; (jw//mac-hide-emacs)
      ;; Create a new frame on same terminal as FRAME, then restore
      ;; selected frame.
      ;; Making a frame might unhide emacs, so hide again
      ;; (sit-for 0.1)
      ;; (ns-hide-emacs t)
      )))

(advice-add 'delete-frame :before 'jw//keep-at-least-one-ns-frame)
(advice-remove 'delete-frame 'jw//keep-at-least-one-ns-frame)

(provide 'jw-funcs-server)
;;; jw-funcs-server.el ends here

(let ((frame (selected-frame)))
  (progn
    (select-frame frame)
    (make-frame)
    (switch-to-buffer "*scratch*")
    (select-frame frame)))
