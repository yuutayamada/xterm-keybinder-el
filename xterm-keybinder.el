;;; xterm-keybinder.el --- Let your terminal emacs to control keybinds in xterm -*- lexical-binding: t; -*-

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
;; This package let your terminal Emacs C-[;:',.0-9]-keys,
;; C-S-[a-z]-keys, C-M-[a-z]-keys and C-M-S-[a-z]-keys on xterm.
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
;; See also: https://github.com/yuutayamada/xterm-keybinder-el/blob/master/README.md
;;; Code:

(require 'cl-lib)

(defvar xterm-keybinder-xterm-keybinds
  '("Ctrl ~Alt ~Super ~Hyper <KeyPress> minus: smaller-vt-font()"
    "Ctrl ~Alt ~Super ~Hyper <KeyPress> plus: larger-vt-font()")
  "List of xterm's function keybinds.
This configuration is only used at when you make xterm's key bind option by
‘xterm-keybinder-insert’.  By default, use C-+ and C-- to change font size.")

(defconst xterm-keybinder-CSI "\033[") ; this means M-[ or ESC [
;; Use private key sequence of CSI
;; Private keys: #x3c, #x3d, #x3e, or #x3f.
;; See also: http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-048.pdf page 26
;; Is there other document? I seem 1998 is too old...
(defconst xterm-keybinder-private-char #x3d)
(defconst xterm-keybinder-prefix
  (format "%s%c" xterm-keybinder-CSI xterm-keybinder-private-char))
(defconst xterm-keybinder-format
  (format "  %%s <KeyPress> %%s: string(\"\\033[%c%%s\") %%s \\n\\"
          xterm-keybinder-private-char))
(defconst xterm-keybinder-C-char-list
  '(":" ";" "," "." "'" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

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
        (map input-decode-map))
    ;; C-S-[a-z], C-M-[a-z] and C-M-S-[a-z]
    (cl-loop for c from ?a to ?z
             for char = (char-to-string c)
             for C-S-key   = (kbd (concat "C-S-"   char))
             for C-M-key   = (kbd (concat "C-M-"   char))
             for C-M-S-key = (kbd (concat "C-M-S-" char))
             do (define-key map (concat prefix (capitalize char)) C-S-key)
             do (define-key map (concat prefix char) C-M-key)
             do (define-key map (concat prefix "=" char) C-M-S-key))
    ;; Treat irregular keybinds
    (define-key map (concat prefix " ")  (kbd "C-S-SPC"))
    (define-key map (concat prefix "=== ") (kbd "C-M-SPC"))
    (define-key map (concat prefix "= ")   (kbd "C-M-S-SPC"))))

(defun xterm-keybinder-insert ()
  "Insert configuration for XTerm.
You can use this to insert xterm configuration by yourself."
  (interactive)
  (let ((ins (lambda (list &optional end)
               (insert (concat (mapconcat 'identity list "\n") (or end "\n"))))))
    (insert "\nXTerm.VT100.translations: #override \\n\\\n")
    ;; XTerm's functions
    (when xterm-keybinder-xterm-keybinds
      (funcall ins (mapcar (lambda (str) (format "  %s \\n\\" str))
                           xterm-keybinder-xterm-keybinds)))
    ;; Control, Alt and Shift keybinds
    (cl-loop with fmt-ctrl = (xterm-keybinder-get-modifier-event 'ctrl)
             for char in xterm-keybinder-C-char-list
             for c = (string-to-char char)
             ;; xrdb occur warning if it uses "'" as xresource
             ;; configuration, so this conversion has to do.
             if (assoc-default c xterm-keybinder-keysym-list)
             collect (format fmt-ctrl it c) into C-keys
             else collect (format fmt-ctrl char c) into C-keys
             finally (funcall ins C-keys))
    (cl-loop with fmt-super = (xterm-keybinder-get-modifier-event 'super)
             with fmt-hyper = (xterm-keybinder-get-modifier-event 'hyper)
             for c from ?a to ?z
             for char = (char-to-string c)
             collect (xterm-keybinder-make-format 'C-S char (capitalize char)) into cs
             collect (xterm-keybinder-make-format 'C-M char char) into cm
             collect (xterm-keybinder-make-format 'C-M-S char (concat "=" char)) into cms
             collect (format fmt-super char c) into super
             collect (format fmt-hyper char c) into hyper
             finally (funcall ins (append cs cm cms super hyper)))
    ;; Space
    (let* ((last (xterm-keybinder-make-format 'C-M-S "space" "= "))
           (spc (funcall ins
                         (list (format (xterm-keybinder-get-modifier-event 'shift)
                                       "space" ?\s)
                               (xterm-keybinder-make-format 'C-S "space" " ")
                               (xterm-keybinder-make-format 'C-M "space" "=== ")
                               ;; Omit \n\ on the last ?\
                               (substring last 0 (- (length last) 5)))
                         "")))
      (insert spc))))

(defun xterm-keybinder-convert (str)
  "Convert STR to list of Hex expression for xterm configuration."
  (cl-loop for c in (delq "" (split-string str ""))
           for char = (string-to-char c)
           collect (format "string(0x%x)" char) into chars
           finally return (mapconcat 'identity chars "")))

(defun xterm-keybinder-make-format (prefix c1 c2 &optional c3)
  "Make adapt format string from PREFIX, C1, and C2."
  (let ((p (cl-case prefix
             (C-S   "Ctrl Shift  ~Alt ~Super ~Hyper")
             (C-M   "Ctrl Alt ~Shift  ~Super ~Hyper")
             (C-M-S "Ctrl Alt  Shift  ~Super ~Hyper")))
        (s (if c3 (xterm-keybinder-convert c3) "")))
    (format xterm-keybinder-format p c1 c2 s)))

(defun xterm-keybinder-get-modifier-event (sym)
  ;; See also ‘event-apply-XXX-modifier’
  (let ((base "string(0x18) string(0x40) "))
    (format "  %s <KeyPress> %%s: %s string(0x%%x) \\n\\"
            (cl-case sym
              (shift "Shift ~Ctrl ~Alt ~Super ~Hyper")
              (ctrl  "Ctrl ~Shift ~Alt ~Super ~Hyper")
              (super "Super ~Ctrl ~Alt ~Shift ~Hyper")
              (hyper "Hyper ~Ctrl ~Alt ~Shift ~Super"))
            (format "%s%s" base (format "string(%s)"
                                        (cl-case sym
                                          (shift "0x53")
                                          (ctrl  "0x63")
                                          (super "0x73")
                                          (hyper "0x68")))))))

(provide 'xterm-keybinder)
;;; xterm-keybinder.el ends here
