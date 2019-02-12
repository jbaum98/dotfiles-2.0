;;; jw-core-lib --- Common library stuff

;;; Commentary:

;; This gets loaded first so it better be really fast.

;;; Code:

(defconst IS-DARWIN  (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defmacro jw/set-var-hook (var val)
  "Return a hook that when run will set VAR to VAL."
  `(lambda ()
    (if (boundp ,var)
      (set ,var ,val))))

(provide 'jw-core-lib)
;;; jw-core-lib.el ends here
