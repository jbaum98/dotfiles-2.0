;;; jw-core-keybindings.el --- A humble take on Spacemacs  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'general))

(defvar jw-leader-key "SPC"
  "The leader key in Evil normal, visual and motion states.")

(defconst jw-emacs-leader-key "M-SPC"
  "The leader key accessible in the Evil Emacs and insert states.")

(defconst jw-ex-command-key ";"
  "The key used for Vim Ex commands.")

(defconst jw-command-key "SPC"
  "The key used for Emacs commands (after pressing on the leader key).")

(defconst jw-mode-key ","
  "The key used to access mode-specfic bindings in Evil normal, visual, and motion states.")

(defconst jw-emacs-mode-key "M-m"
  "The key used to access mode-specfic bindings in Evil Emacs and insert states.")

(defvar jw-mode-bindings nil
  "A variable that is non-nil if there are mode-specific bindings.")

(defvar-local jw-major-mode-map nil
  "The keymap for major-mode specific keybindings.")

(use-package general
  :demand
  :ensure)

;; Setup definer for leader keys
(general-create-definer jw-leader-def
  :states '(normal insert visual motion emacs)
  :prefix jw-leader-key
  :non-normal-prefix jw-emacs-leader-key
  :prefix-map 'jw-leader-map)

;; Clear out jw-mode-key
(general-define-key
 :states '(normal visual motion)
 jw-mode-key nil)

;; Set jw-ex-command-key to ex mode
(general-define-key
 :states '(normal visual)
 jw-ex-command-key 'evil-ex)

(use-package which-key
  :ensure
  :demand
  :commands
  which-key-mode
  which-key-add-key-based-replacements
  which-key-add-major-mode-key-based-replacements
  :init
  (setf which-key-idle-delay 0.5
        which-key-popup-type 'minibuffer
        which-key-enable-extended-define-key t)
  :config
  (which-key-mode))

;; Instantly show in-progress key combinations.
(setf echo-keystrokes 0.02)

;; Make <ESC> key quit as much as ossible
(cl-dolist (map (list
                 minibuffer-local-map
                 minibuffer-local-ns-map
                 minibuffer-local-completion-map
                 minibuffer-local-must-match-map
                 minibuffer-local-isearch-map))
  (define-key map (kbd "<escape>") 'keyboard-escape-quit))

;; General purpose
(jw-leader-def
  "SPC" '(execute-extended-command :wk "M-x")
  "u" 'universal-argument
  "!" 'shell-command)

;; Buffers
(use-package jw-funcs-buffer
  :general
  (jw-leader-def
    :infix "b"
    "" '(nil :wk "buffers")
    "d" '(jw/kill-this-buffer :wk "kill")
    "b" 'buffer-menu
    "n" 'next-buffer
    "m" '(jw/goto-message-buffer :wk "message-buffer")
    "N" '(jw/new-empty-buffer :wk "new-empty-buffer")
    "P" '(jw/copy-clipboard-to-whole-buffer :wk "clipboard-to-whole-buffer")
    "p" 'previous-buffer
    "R" '(jw/safe-revert-buffer :wk "revert-buffer")
    "s" '(jw/switch-to-scratch-buffer :wk "scratch-buffer")
    "Y" '(jw/copy-whole-buffer-to-clipboard :wk "whole-buffer-to-clipboard")
    "1" 'buffer-to-window-1
    "2" 'buffer-to-window-2
    "3" 'buffer-to-window-3
    "4" 'buffer-to-window-4
    "5" 'buffer-to-window-5
    "6" 'buffer-to-window-6
    "7" 'buffer-to-window-7
    "8" 'buffer-to-window-8
    "9" 'buffer-to-window-9))

;; Errors
(use-package jw-funcs-error
  :general
  (jw-leader-def
    :infix "e"
    "" '(nil :wk "errors")
    "n" '(jw/next-error :wk "next-error")
    "p" '(jw/previous-error :wk "previous-error")))

;; Files
(use-package jw-funcs-file
  :general
  (jw-leader-def
    :infix "f"
    "" '(nil :wk "files")
    "c"  '(jw/copy-file :wk "copy-file")
    "D"  '(jw/delete-current-buffer-file :wk "delete-current-buffer-file")
    "ed" '(jw/find-user-init-file :wk "find-user-init-file")
    "ev" '(jw/display-and-copy-emacs-version :wk "display-and-copy-emacs-version")
    "Cd" '(jw/unix2dos :wk "unix2dos")
    "Cu" '(jw/dos2unix :wk "dos2unix")
    "G"  'rgrep
    "f"  'find-file
    "r"  '(recentf-open-files :wk "recentf")
    "l"  'find-file-literally
    "E"  '(jw/sudo-edit :wk "sudo-edit")
    "o"  '(jw/open-file-or-directory-in-external-app :wk "open")
    "R"  '(jw/rename-current-buffer-file :wk "rename")
    "S"  '(evil-write-all :wk "save-all")
    "s"  'save-buffer
    "vd" 'add-dir-local-variable
    "vf" 'add-file-local-variable
    "vp" 'add-file-local-variable-prop-line
    "y"  '(jw/show-and-copy-buffer-filename :wk "copy-filename")))`

;; Help
(jw-leader-def
  :infix "h"
  "" '(nil :wk "help")
  "db" 'describe-bindings
  "dc" 'describe-char
  "df" 'describe-function
  "dk" 'describe-key
  "dp" 'describe-package
  "dt" 'describe-theme
  "dv" 'describe-variable
  "N"  'view-emacs-news)

;; Navigation and Jumping
(jw-leader-def
  :infix "j"
  "" '(nil :wk "jumping")
  "f" 'find-function
  "v" 'find-variable)

;; Compilation
(use-package jw-funcs-compilation
  :general
  (jw-leader-def
    :infix "c"
    "" '(nil :wk "compilation")
    "C" 'compile
    "k" 'kill-compilation
    "r" 'recompile
    "d" '(jw/close-compilation-window :wk "close-compilation-window")))

;; Narrow and widen
(jw-leader-def
  :infix "n"
  "" '(nil :wk "narrow/widen")
  "r" 'narrow-to-region
  "p" 'narrow-to-page
  "f" 'narrow-to-defun
  "w" 'widen)

;; Windows
(jw-leader-def
  :infix "w"
  "" '(nil :wk "windows")
  "H" 'evil-window-move-far-left
  "h" 'evil-window-left
  "J" 'evil-window-move-very-bottom
  "j" 'evil-window-down
  "K" 'evil-window-move-very-top
  "k" 'evil-window-up
  "L" 'evil-window-move-far-right
  "l" 'evil-window-right
  "o" 'other-frame
  "m" 'delete-other-windows
  "s" 'split-window-below
  "S" 'split-window-below-and-focus
  "-" 'split-window-below
  "v" 'split-window-right
  "V" 'split-window-right-and-focus
  "w" 'other-window
  "/" 'split-window-right
  "=" 'balance-windows)

(use-package winner
  ;; we need to enable winner-mode immediately
  :demand
  :general
  (jw-leader-def
    :infix "w"
    "U" '(winner-redo :wk "redo")
    "u" '(winner-undo :wk "undo"))
  :config
  (winner-mode))

(use-package jw-funcs-window
  :general
  (jw-leader-def "wd" '(jw/delete-window :wd "delete-window")))

;; Alignment
(use-package jw-funcs-align
  :general
  (jw-leader-def
    :infix "x"
    "" '(nil :wk "align")
    "xa&" '(jw/align-repeat-ampersand :wk "align-repeat-ampersand")
    "xa(" '(jw/align-repeat-left-paren :wk "align-repeat-left-paren")
    "xa)" '(jw/align-repeat-right-paren :wk "align-repeat-right-paren")
    "xa," '(jw/align-repeat-comma :wk "align-repeat-comma")
    "xa." '(jw/align-repeat-decimal :wk "align-repeat-decimal")
    "xa:" '(jw/align-repeat-colon :wk "align-repeat-colon")
    "xa;" '(jw/align-repeat-semicolon :wk "align-repeat-semicolon")
    "xa=" '(jw/align-repeat-equal :wk "align-repeat-equal")
    "xa\\" '(jw/align-repeat-backslash :wk "align-repeat-backslash")
    "xaa" 'align
    "xac" 'align-current
    "xam" '(jw/align-repeat-math-oper :wk "align-repeat-math-oper")
    "xar" '(jw/align-repeat :wk "align-repeat")
    "xa|" '(jw/align-repeat-bar :wk "align-repeat-bar")
    "xc"  'count-region
    "xdw" 'delete-trailing-whitespace
    "xjc" 'set-justification-center
    "xjf" 'set-justification-full
    "xjl" 'set-justification-left
    "xjn" 'set-justification-none
    "xjr" 'set-justification-right
    "xlc" '(jw/sort-lines-by-column :wk "sort-lines-by-column")
    "xlC" '(jw/sort-lines-by-column-reverse :wk "sort-lines-by-column-reverse")
    "xld" '(jw/duplicate-line-or-region :wk "duplicate-line-or-region" )
    "xls" '(jw/sort-lines :wk "sort-lines")
    "xlS" '(jw/sort-lines-reverse :wk "sort-lines-reverse")
    "xlu" '(jw/uniquify-lines :wk "uniq")
    "xtc" 'transpose-chars
    "xtl" 'transpose-lines
    "xtw" 'transpose-words
    "xU"  'upcase-region
    "xu"  'downcase-region
    "xwc" '(jw/count-words-analysis :wk "count-words-analysis" )
    "x TAB" 'indent-rigidly))

;; Quiting
(jw-leader-def
  :infix "q"
  "" '(nil :wk "quit")
  "q" 'save-buffers-kill-emacs
  "z" 'save-buffers-kill-terminal)

(provide 'jw-core-keybindings)
;;; jw-core-keybindings.el ends here
