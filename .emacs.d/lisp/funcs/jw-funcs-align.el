;;; jw-funcs-align.el --- Functions for alignment

;;; Commentary:

;;; Code:

(autoload 'evil-visual-state-p "evil")
(autoload 'string-empty-p "subr-x")
(autoload 'align-region "align")

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun jw/align-repeat (beg end regexp &optional justify-right after)
  "Repeat alignment with respect to REGEXP on the region from BEG to END.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))
    (message "%S" complete-regexp)
    (align-regexp beg end complete-regexp group 1 t)))

;; Modified answer from
;; http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun jw/align-repeat-decimal (beg end)
  "Align a table of numbers on decimal points and dollar signs (both optional).
Acts on the region from BEG to END."
  (interactive "r")
  (require 'align)
  (align-region beg end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro jw|create-align-repeat-x (name regexp &optional justify-right default-after)
  "Create a function named according to NAME that repeatedly aligns by REGEXP.
JUSTIFY-RIGHT and DEFAULT-AFTER are passed to jw/align-repeat."
  (let ((new-func (intern (concat "jw/align-repeat-" name))))
    `(defun ,new-func (beg end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (jw/align-repeat beg end ,regexp ,justify-right after)))))

(jw|create-align-repeat-x "ampersand" "&")
(jw|create-align-repeat-x "backslash" "\\\\")
(jw|create-align-repeat-x "bar" "|")
(jw|create-align-repeat-x "colon" ":" nil t)
(jw|create-align-repeat-x "comma" "," nil t)
(jw|create-align-repeat-x "equal" "=")
(jw|create-align-repeat-x "left-paren" "(")
(jw|create-align-repeat-x "math-oper" "[+\\-*/]")
(jw|create-align-repeat-x "right-paren" ")" t)
(jw|create-align-repeat-x "semicolon" ";" nil t)

;; from http://www.emacswiki.org/emacs/WordCount
(defun jw/count-words-analysis (beg end)
  "Count how many times each word is used in the region from BEG to END.
Punctuation is ignored."
  (interactive "r")
  (let (words
        alist_words_compare
        (formated "")
        (overview (call-interactively 'count-words)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (defun alist_words_compare (a b)
      "Compare elements from an associative list of words count.
Compare them on count first,and in case of tie sort them alphabetically."
      (let ((a_key (car a))
            (a_val (cdr a))
            (b_key (car b))
            (b_val (cdr b)))
        (if (eq a_val b_val)
            (string-lessp a_key b_key)
          (> a_val b_val))))
    (setq words (cl-sort words 'alist_words_compare))
    (while words
      (let* ((word (pop words))
             (name (car word))
             (count (cdr word)))
        (setq formated (concat formated (format "[%s: %d], " name count)))))
    (when (called-interactively-p 'any)
      (if (> (length formated) 2)
          (message (format "%s\nWord count: %s"
                           overview
                           (substring formated 0 -2)))
        (message "No words.")))
    words))

;; from https://www.emacswiki.org/emacs/CopyingWholeLines
(defun jw/duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ; Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ; Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ; Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ; Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (if (> 0 n)                             ; Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun jw/sort-lines (&optional reverse)
  "Sort lines in a region or the current buffer.
If REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
         (beg (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max))))
    (sort-lines reverse beg end)))

(defun jw/sort-lines-reverse ()
  "Sort lines in reverse order, in a region or the current buffer."
  (interactive)
  (jw/sort-lines -1))

(defun jw/sort-lines-by-column (&optional reverse)
  "Sort lines by the selected column.
If REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
         (beg (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max))))
    (sort-columns reverse beg end)))

(defun jw/sort-lines-by-column-reverse ()
  "Sort lines by the selected column in reverse order."
  (interactive)
  (jw/sort-lines-by-column 't))

(defun jw/uniquify-lines ()
  "Remove duplicate adjacent lines in a region or the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
             (beg (if region-active (region-beginning) (point-min)))
             (end (if region-active (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(provide 'jw-funcs-align)
;;; jw-funcs-align.el ends here
