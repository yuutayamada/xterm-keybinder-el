;;; xterm-keybinder.el --- Let you extra keybinds in xterm -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5") (let-alist "1.0.1"))
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
;;  | C          | [2-8] (note: xterm.el supports other key binds)
;;  | C-S        | [A-Z]
;;  | C-M        | "g" and space keys
;;  | M-S        | [A-Z]
;;  | C-M-S      | [A-Z]
;;  | s or s-S   | from space to "~" (almost 8 bits characters without control sequences)
;;  | H or H-S   | same as s-S, but use Hyper modifier
;;
;; Prerequisite:
;;
;; 1. XTerm (of course)
;; 2. XTerm's configuration
;;
;;    You may need to configure your .Xresources file if you don't use XTerm yet.
;;    (You can update by xrdb command)
;;
;;    -- configuration
;;    XTerm*VT100.eightBitInput: false
;;    XTerm*vt100.formatOtherKeys: 1
;;    -- end of configuration
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
;;; Code:

(require 'cl-lib)

(defvar xterm-keybinder-direction-name "XTerm.VT100.translations:")

(defvar xterm-keybinder-key-pairs
  (append
   '((?\s . ?\s)
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

;; xterm.el supports many control keys if users set modifyOtherKeys to 1, so
;; I don't implement same keybinds.
;; xterm.el supporting keys(my memo):
;;   Control: C-tab,   C-return,   C-{ ',-./019;=\!"#$%&()*+:<>? }
;;   C-M    : C-M-tab, C-M-return, C-M-SPC, C-M-{ ',-./0-9;=\!"#$%&()*+:<>? }
;;   Shift  : S-tab, S-return
;;   C-S    : C-S-tab, C-S-return
(defvar xterm-keybinder-table
  (let ((A-Z (cl-loop for c from ?A to ?Z collect c))
        (chars (cl-loop for (c . _) in xterm-keybinder-key-pairs collect c))
        (S-chars (cl-loop for (_ . C) in xterm-keybinder-key-pairs
                          if (<= ?A C ?Z)
                          collect C into skeys
                          else collect C into skeys?
                          finally return (cons skeys skeys?))))
    `((S     . ((xtmod    . "Shift ~Ctrl ~Alt ~Super ~Hyper")
                (suffix . "0x53")))
      (C     . ((xtmod    . "Ctrl ~Shift ~Alt ~Super ~Hyper")
                (suffix . "0x63")
                (keys   . (?2 ?3 ?4 ?5 ?6 ?7 ?8))))
      (s     . ((xtmod    . "Super ~Ctrl ~Alt ~Shift ~Hyper")
                (suffix . "0x73")
                (keys   .  ,chars)))
      (H     . ((xtmod    . "Hyper ~Ctrl ~Alt ~Shift ~Super")
                (suffix . "0x68")
                (keys   .  ,chars)))
      (C-S   . ((xtmod    . "Ctrl Shift  ~Alt ~Super ~Hyper")
                (spacer . "")
                (Shift-keys   . ,(append '(?\s) A-Z))))
      (C-M   . ((xtmod    . "Ctrl Alt ~Shift  ~Super ~Hyper")
                (spacer . "===")
                (keys   . (?\s ?g))))
      (C-M-S . ((xtmod    . "Ctrl Alt  Shift  ~Super ~Hyper")
                (spacer . "=")
                (Shift-keys . ,(append '(?\s) A-Z))))
      (M-S   . ((xtmod    . "Alt Shift ~Ctrl ~Super ~Hyper")
                (spacer . "==")
                (Shift-keys   . ,(append A-Z))))
      (s-S   . ((xtmod    . "Super %s~Alt ~Ctrl ~Hyper")
                (spacer . "====")
                (Shift-keys   . ,(car S-chars))
                (Shift-keys?  . ,(cdr S-chars))))
      (H-S   . ((xtmod    . "Hyper %s~Alt ~Ctrl ~Super")
                (spacer . "=====")
                (Shift-keys   . ,(car S-chars))
                (Shift-keys?  . ,(cdr S-chars))))
      (A     . ((xtmod    . "Ctrl ~Shift ~Alt ~Super ~Hyper")
                (suffix . "0x61")
                (keys   . (?m ?i)))))))

(defvar xterm-keybinder-enable-C-i-C-m nil
  "Whether this package enables C-i and C-m keys.")

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

(defun xterm-keybinder-get-desc (keydef)
  "Return pair of key and modifier from KEYDEF."
  (let ((key-sec (cl-loop with keys = (reverse (string-to-list (key-description (kbd keydef))))
                          for i from 0 to (length keys) by 2
                          collect (nth i keys) into key-sec
                          finally return (cons (car key-sec) (reverse (cdr key-sec))))))
    ;; key . modifier
    key-sec))

(defun xterm-keybinder-format (keydef func-or-keysequence)
  "Make key definition for xterm option from KEYDEF and FUNC-OR-KEYSEQUENCE string."
  (let* ((pair (xterm-keybinder-get-desc keydef))
         (char (car pair))
         (no-shift (car (assoc char xterm-keybinder-key-pairs)))
         (shifted  (cdr (rassoc char xterm-keybinder-key-pairs)))
         (xtmod
          (assoc-default 'xtmod (assoc-default (intern-soft (mapconcat 'string (cdr pair) "-"))
                                               xterm-keybinder-table))))
    (format "%s <KeyPress> %s: %s"
            (if shifted
                ;; Shifted key like C-+, hides no-shift key (in this case, C-=).
                ;; To work around, omit Shift modifier. (but, [A-Z] work fine)
                (replace-regexp-in-string "~?Shift " "" xtmod)
              xtmod)
            (assoc-default (or no-shift shifted) xterm-keybinder-keysym-list)
            func-or-keysequence)))

(defvar xterm-keybinder-xterm-keybinds
  '(("C--" . "smaller-vt-font()")
    ("C-=" . "string(0x18) string(0x40) string(0x63) string(0x3d)")
    ("C-+" . "larger-vt-font()"))
  "List of xterm's function keybinds.
This configuration is only used at when you make xterm's key bind option by
‘xterm-keybinder-insert’.  By default, this package adds C-+ and C--
to change font size.")

(defconst xterm-keybinder-CSI "\033["
  "The xterm-keybinder uses CSI key to make pseudo key bindings.
Note that this variable can not be \\e[ because I failed to bind
xterm-option.  (for future me) Also you can not set M-[ or ESC [ as
keybind if you want to use this package.  Those keybinds conflict with
escape sequence.")

(defconst xterm-keybinder-private-char #x3d "Use private key sequence of CSI.")
;; Private keys: #x3c, #x3d, #x3e, and #x3f.
;;   See also: http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-048.pdf page 26
;;   Is there other document? I seem 1998 is too old...
;; Note: On xt-mouse.el(this pacakge provides xterm-mouse-mode)
;;       it's already used \e[< and \e[? which mean CSI #x3c and CSI #x3f.

(defconst xterm-keybinder-prefix
  (format "%s%c" xterm-keybinder-CSI xterm-keybinder-private-char))
(defconst xterm-keybinder-format
  (format "string(\"\\033[%c%%s\")" xterm-keybinder-private-char))

;;;###autoload
(defun xterm-keybinder-setup ()
  "Enable Emacs keybinds even in the xterm terminal Emacs."
  (interactive)
  (cl-mapcar 'xterm-keybinder-set-keybinds '(C-S C-M C-M-S M-S H-S s-S))
  ;; Still work in progress
  (when xterm-keybinder-enable-C-i-C-m
    (unless (lookup-key global-map (kbd "A-i"))
      (global-set-key (kbd "A-i") (kbd "C-i")))
    (unless (lookup-key global-map (kbd "A-m"))
      (global-set-key (kbd "A-m") (kbd "C-m")))))

(defun xterm-keybinder-set-keybinds (modifier)
  "Set keybinds which correspond to MODIFIER."
  (let-alist (assoc-default modifier xterm-keybinder-table)
    (let* ((prefix xterm-keybinder-prefix)
           (defkey
             (lambda (keys)
               (cl-loop for c in keys
                        for char = (downcase (char-to-string c))
                        do (define-key input-decode-map
                             (format "%s%s%c" prefix .spacer c)
                             (kbd (format "%s-%s" modifier
                                          (if (eq ?\s c) "SPC" char))))))))
      (if .keys
          (funcall defkey .keys)
        (when (or .Shift-keys .Shift-keys?)
          (funcall defkey (append .Shift-keys .Shift-keys?)))))))

(defun xterm-keybinder-insert ()
  "Insert configuration for XTerm.
You can use this to insert xterm configuration by yourself."
  (interactive)
  (let* ((ins (lambda (list)
                (insert (concat (mapconcat 'identity list "\n") "\n"))))
         (put-keydef
          (lambda (sym)
            (let-alist (assoc-default sym xterm-keybinder-table)
              (cl-loop with result
                       with fmt = (xterm-keybinder-make-base-format sym)
                       for (c . C) in xterm-keybinder-key-pairs
                       ;; no-shift char
                       for char = (and c
                                       (or (assoc-default c xterm-keybinder-keysym-list)
                                           (char-to-string c)))
                       ;; capitalized char
                       for Char = (and C
                                       (or (assoc-default C xterm-keybinder-keysym-list)
                                           (char-to-string C)))
                       if (and .keys (member c .keys))
                       do (push (format fmt char c) result)
                       if (and .Shift-keys (member C .Shift-keys))
                       do (push (if .Shift-keys?
                                    (format fmt "Shift " Char C)
                                  (format fmt Char C))
                                result)
                       if (and .Shift-keys? (member C .Shift-keys?))
                       do (push (format fmt "" Char C) result)
                       finally (funcall ins (reverse result)))))))
    (insert (format "%s #override \\n\\\n" xterm-keybinder-direction-name))
    ;; XTerm's functions
    (when xterm-keybinder-xterm-keybinds
      (funcall ins (mapcar (lambda (str) (format "  %s \\n\\" str))
                           (cl-loop for (key . def) in xterm-keybinder-xterm-keybinds
                                    collect (xterm-keybinder-format key def)))))
    ;; C, C-S, C-M, C-M-S, M-S, s, s-S, H and H-S
    (cl-mapcar put-keydef '(C C-S C-M C-M-S M-S s s-S H H-S))
    ;; Set C-m and C-i to A-m and A-i (and change later on)
    (when xterm-keybinder-enable-C-i-C-m
      (funcall ins
               (cl-loop with fmt = (xterm-keybinder-make-base-format 'A)
                        for c in '(?m ?i) collect (format fmt (string c) c))))
    ;; Shift Space
    (let* ((last (format (xterm-keybinder-make-base-format 'S) ; shift
                         "space" ?\s)))
      (insert (format "%s" (substring last 0 (- (length last) 4)))))))

(defun xterm-keybinder-make-base-format (sym)
  ;; See also ‘event-apply-XXX-modifier’
  (let ((C-x@ "string(0x18) string(0x40)"))
    (let-alist (assoc-default sym xterm-keybinder-table)
      (format "  %s <KeyPress> %%s: %s string(0x%%x) \\n\\"
              .xtmod
              (if (member sym '(S C s H A))
                  ;; event modifier
                  (format "%s string(%s)" C-x@ .suffix)
                ;; \033[=
                (format xterm-keybinder-format .spacer))))))

;; For debug
;;  (message (key-description (read-key-sequence-vector "input: ")))

(provide 'xterm-keybinder)
;;; xterm-keybinder.el ends here
