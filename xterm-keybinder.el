;;; xterm-keybinder.el --- Let you keybinds in xterm -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Version: 0.1.0
;; Keywords: Convenient

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package lets you key binds that normally terminal Emacs can not use in XTerm.
;; (i.e., C-M-g)
;;
;; Summary of available key binds:
;;
;;  | Modifiers  | Description                          |
;;  |:-----------|:-------------------------------------|
;;  | S          | space key with shift
;;  | C          | [:;,.'0-9]
;;  | C-S        | [A-Z]
;;  | C-M        | [a-z]
;;  | M-S        | [A-Z]
;;  | C-M-S      | [A-Z]
;;  | s or s-S   | from space to "~" (almost 8 bits characters without control sequences)
;;  | H or H-S   | same as s-S, but use Hyper modifier
;;
;; Usage:
;; Put below configuration to your .emacs
;;
;; -- configuration --
;;   (when (getenv "XTERM_VERSION")
;;     (add-hook 'terminal-init-xterm-hook 'xterm-keybinder-setup))
;; -- configuration end --
;;
;; Then start your emacs with xterm and the option.
;;
;; -- shell script example --
;;   #!/bin/sh
;;   xtermopt=path/to/this-repository/xterm-option
;;   eval "xterm -xrm `${xtermopt}` -e emacsclient -t -a ''"
;; -- shell script example end --
;;
;; Note:
;; You may need following configuration at .Xresources and update it by
;; xrdb command.
;;
;; -- configuration
;;   XTerm*VT100.eightBitInput: false
;;   XTerm*modifyOtherKeys: 1
;;   XTerm*vt100.formatOtherKeys: 1
;; -- end of configuration
;;
;;; Code:

(require 'cl-lib)

(defvar xterm-keybinder-xterm-keybinds
  '("Ctrl ~Alt ~Super ~Hyper <KeyPress> minus: smaller-vt-font()"
    "Ctrl ~Alt ~Super ~Hyper <KeyPress> plus: larger-vt-font()")
  "List of xterm's function keybinds.
This configuration is only used at when you make xterm's key bind option by
‘xterm-keybinder-insert’.  By default, use C-+ and C-- to change font size.")

(defvar xterm-keybinder-key-pairs
  (append
   '((?\s . nil)
     (?`  . ?~)
     (?-  . ?_)
     (?=  . ?+)
     (?\[ . ?\{)
     (?\] . ?\})
     (?\\ . ?|)
     (?\; . ?:)
     (?\' . ?\")
     (?,  . ?<)
     (?.  . ?>)
     (?/  . ?\?)
     (?0  . ?\))
     (?1  . ?!)
     (?2  . ?@)
     (?3  . ?#)
     (?4  . ?$)
     (?5  . ?%)
     (?6  . ?^)
     (?7  . ?&)
     (?8  . ?*)
     (?9  . ?\())
   ;; pair of ([a-z] . [A-Z])
   (cl-loop for c from ?a to ?z
            for C = (string-to-char (capitalize (string c)))
            collect (cons c C)))
  "List of cons (no-shifted-char . shifted-char).
Use standard US layout.  See also https://en.wikipedia.org/wiki/IBM_PC_keyboard.")

(defconst xterm-keybinder-CSI "\033[") ; this means M-[ or ESC [
;; Use private key sequence of CSI
;; Private keys: #x3c, #x3d, #x3e, or #x3f.
;; See also: http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-048.pdf page 26
;; Is there other document? I seem 1998 is too old...
(defconst xterm-keybinder-private-char #x3d)
(defconst xterm-keybinder-prefix
  (format "%s%c" xterm-keybinder-CSI xterm-keybinder-private-char))
(defconst xterm-keybinder-format
  (format "string(\"\\033[%c%%s\")" xterm-keybinder-private-char))
(defconst xterm-keybinder-C-char-list
  '(":" ";" "," "." "'" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(defconst xterm-keybinder-table
  '((C-S   . "")
    (C-M   . "===")
    (C-M-S . "=")
    (M-S   . "==")
    (s-S   . "====")
    (H-S   . "=====")))

;; based on XTerm's keysym.map
(defconst xterm-keybinder-keysym-list
  '((?\s . "space")
    (?!  . "exclam")
    (?\" . "quotedbl")
    (?#  . "numbersign")
    (?$  . "dollar")
    (?%  . "percent")
    (?&  . "ampersand")
    (?\' . "apostrophe")
    (?\( . "parenleft")
    (?\) . "parenright")
    (?*  . "asterisk")
    (?+  . "plus")
    (?,  . "comma")
    (?-  . "minus")
    (?.  . "period")
    (?/  . "slash")
    ;; 0-9
    (?:  . "colon")
    (?\; . "semicolon")
    (?<  . "less")
    (?=  . "equal")
    (?>  . "greater")
    (?\? . "question")
    (?@  . "at")
    ;; A-Z
    (?\[ . "bracketleft")
    (?\\ . "backslash")
    (?\] . "bracketright")
    (?^  . "asciicircum")
    (?_  . "underscore")
    (?`  . "grave")
    ;; a-z
    (?\{ . "braceleft")
    (?|  . "bar")
    (?\} . "braceright")
    (?~  . "asciitilde")))

;;;###autoload
(defun xterm-keybinder-setup ()
  "Enable Emacs keybinds even in the xterm terminal Emacs."
  (interactive)
  (let ((prefix xterm-keybinder-prefix)
        (map input-decode-map)
        (cs  (assoc-default 'C-S   xterm-keybinder-table))
        (cm  (assoc-default 'C-M   xterm-keybinder-table))
        (cms (assoc-default 'C-M-S xterm-keybinder-table))
        (ms  (assoc-default 'M-S   xterm-keybinder-table))
        (sS  (assoc-default 's-S   xterm-keybinder-table))
        (hS  (assoc-default 'H-S   xterm-keybinder-table)))
    ;; C-S-[a-z], C-M-[a-z] and C-M-S-[a-z]
    (cl-loop with defkey = (lambda (props)
                             (cl-loop for (mod c key) in props do
                                      (define-key map (format "%s%s%c" prefix mod c) key)))
             for c from ?\s to ?~
             for char = "SPC" then (downcase (char-to-string c))
             for C-S-key   = (kbd (concat "C-S-"   char))
             for C-M-key   = (kbd (concat "C-M-"   char))
             for C-M-S-key = (kbd (concat "C-M-S-" char))
             for M-S-key   = (kbd (concat "M-S-"   char))
             for s-S-key   = (kbd (concat "s-S-"   char))
             for H-S-key   = (kbd (concat "H-S-"   char))
             if (eq c ?=) do '() ; just ignore
             else if (<= ?A c ?Z) do
             (funcall defkey `((,cs ,c ,C-S-key) (,cms ,c ,C-M-S-key) (,ms ,c ,M-S-key)
                               (,sS ,c ,s-S-key) (,hS ,c ,H-S-key)))
             else if (<= ?a c ?z) do
             (funcall defkey `((,cm ,c ,C-M-key)))
             else if (eq c ?\s) do ; for space
             (funcall defkey `((,cs ,c ,C-S-key) (,cm ,c ,C-M-key) (,cms ,c ,C-M-S-key)))
             else
             do (funcall defkey `((,hS ,c ,H-S-key) (,sS ,c ,s-S-key))))))

(defun xterm-keybinder-insert ()
  "Insert configuration for XTerm.
You can use this to insert xterm configuration by yourself."
  (interactive)
  (let ((ins (lambda (list &optional end)
               (insert (concat (mapconcat 'identity list "\n") (or end "\n"))))))
    (insert "XTerm.VT100.translations: #override \\n\\\n")
    ;; XTerm's functions
    (when xterm-keybinder-xterm-keybinds
      (funcall ins (mapcar (lambda (str) (format "  %s \\n\\" str))
                           xterm-keybinder-xterm-keybinds)))
    ;; Control keybinds
    (cl-loop with fmt-ctrl = (xterm-keybinder-make-base-format 'ctrl)
             for char in xterm-keybinder-C-char-list
             for c = (string-to-char char)
             ;; xrdb occur warning if it uses "'" as xresource
             ;; configuration, so this conversion has to do.
             if (assoc-default c xterm-keybinder-keysym-list)
             collect (format fmt-ctrl it c) into C-keys
             else collect (format fmt-ctrl char c) into C-keys
             finally (funcall ins C-keys))
    ;; Control, Alt, Shift
    (cl-loop with cs and cm and cms and ms
             with fmt-C-S   = (xterm-keybinder-make-base-format 'C-S)
             with fmt-C-M   = (xterm-keybinder-make-base-format 'C-M)
             with fmt-C-M-S = (xterm-keybinder-make-base-format 'C-M-S)
             with fmt-M-S   = (xterm-keybinder-make-base-format 'M-S)
             for c from ?\s to ?~
             for char = (or (assoc-default c xterm-keybinder-keysym-list)
                            (char-to-string c))
             if (<= ?a c ?z) do
             (push (format fmt-C-M char c) cm)
             else if (<= ?A c ?Z) do
             (push (format fmt-C-S char c) cs)
             (push (format fmt-C-M-S char c) cms)
             (push (format fmt-M-S char c) ms)
             else if (eq c ?\s) do
             (push (format fmt-C-S char c) cs)
             (push (format fmt-C-M-S char c) cms)
             (push (format fmt-C-M char c) cm)
             finally (funcall ins (reverse (append ms cms cm cs))))
    ;; Super and Hyper
    (cl-loop with super and hyper
             with fmt-s   = (xterm-keybinder-make-base-format 'super)
             with fmt-s-S = (xterm-keybinder-make-base-format 's-S)
             with fmt-H   = (xterm-keybinder-make-base-format 'hyper)
             with fmt-H-S = (xterm-keybinder-make-base-format 'H-S)
             for (c . C) in xterm-keybinder-key-pairs
             ;; normal char
             for char = (or (assoc-default c xterm-keybinder-keysym-list)
                            (char-to-string c))
             ;; capitalized char
             for Char = (and C
                             (or (assoc-default C xterm-keybinder-keysym-list)
                                 (char-to-string C)))
             do (progn (push (format fmt-s char c) super)
                       (push (format fmt-H char c) hyper))
             if C do
             (let ((Shift (if (<= ?A C ?Z) "Shift " "")))
               (push (format fmt-s-S Shift Char C) super)
               (push (format fmt-H-S Shift Char C) hyper))
             finally (funcall ins (reverse (append hyper super))))
    ;; Shift Space
    (let* ((last (format (xterm-keybinder-make-base-format 'shift)
                         "space" ?\s)))
      (insert (format "%s" (substring last 0 (- (length last) 4)))))))

(defun xterm-keybinder-make-base-format (sym)
  ;; See also ‘event-apply-XXX-modifier’
  (let ((C-x@ "string(0x18) string(0x40) "))
    (format "  %s <KeyPress> %%s: %s string(0x%%x) \\n\\"
            (cl-case sym
              (shift "Shift ~Ctrl ~Alt ~Super ~Hyper")
              (ctrl  "Ctrl ~Shift ~Alt ~Super ~Hyper")
              (super "Super ~Ctrl ~Alt ~Shift ~Hyper")
              (hyper "Hyper ~Ctrl ~Alt ~Shift ~Super")
              (C-S   "Ctrl Shift  ~Alt ~Super ~Hyper")
              (C-M   "Ctrl Alt ~Shift  ~Super ~Hyper")
              (C-M-S "Ctrl Alt  Shift  ~Super ~Hyper")
              (M-S   "Alt Shift ~Ctrl ~Super ~Hyper")
              (s-S   "Super %s~Alt ~Ctrl ~Hyper")
              (H-S   "Hyper %s~Alt ~Ctrl ~Super"))
            (if (member sym '(shift ctrl super hyper))
                ;; event modifier
                (format "%s%s" C-x@ (format "string(%s)"
                                            (cl-case sym
                                              (shift "0x53")
                                              (ctrl  "0x63")
                                              (super "0x73")
                                              (hyper "0x68"))))
              ;; \033[=
              (format xterm-keybinder-format
                      (assoc-default sym xterm-keybinder-table))))))

;; For debug
;;  (message (key-description (read-key-sequence "input: ")))

(provide 'xterm-keybinder)
;;; xterm-keybinder.el ends here
