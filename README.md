# xterm-keybinder

This package lets you key binds that normally terminal Emacs can not use.
(i.e., C-M-g)

## Summary of available key binds

| Modifiers  | Description                          |
|:-----------|:-------------------------------------|
| S          | Space key with shift
| C          | [2-8] (note: xterm.el supports other key binds)
| C-S        | [A-Z]
| C-M        | "g" and space keys
| M-S        | [A-Z]
| C-M-S      | [A-Z]
| s or s-S   | from space to "~" (almost 8 bits characters without control sequences)
| H or H-S   | same as s-S, but use Hyper modifier

## Usage for XTerm
You may need following configuration in your .Xresources file:

```sh
XTerm*VT100.eightBitInput: false
XTerm*vt100.formatOtherKeys: 1
```


Put below configuration to your .emacs

```lisp
(add-hook
 'tty-setup-hook
 '(lambda ()
    (cl-case (alist-get 'terminal-initted (terminal-parameters))
      (terminal-init-xterm
       (xterm-keybinder-setup)))))
```

Then start your emacs with xterm and the option

```sh
#!/bin/sh
xtermopt=path/to/this-repository/xterm-option
eval "xterm -xrm `${xtermopt}` -e emacsclient -t -a ''"
```

On your started Emacs, you can use C-M prefix and C-S prefix keybinds.

FYI, I'm configuring like [this](https://github.com/yuutayamada/emacs.d/blob/master/emacs.sh)

## Usage for URxvt

Put below configuration to your .emacs

```lisp
(require 'cl-lib)
(add-hook
 'tty-setup-hook
 '(lambda ()
    (cl-case (alist-get 'terminal-initted (terminal-parameters))
      (terminal-init-rxvt
       (when (getenv "COLORTERM" (selected-frame))
         (urxvt-keybinder-setup))))))
```

Start URxvt daemon:

```sh
urxvtd -q -o -f
```

Then start your emacs using emacs-urxvt-client file.

```sh
emacs_urxvt_client=/path/to/emacs-urxvt-client \
${emacs-urxvt-client} -e emacscliet -t &
```

(The main content of emacs-urxvt-client file is just a configuration
to use emacs keybindings.)

## Note

If you put xterm-option of this package in your xterm resource,
you may have trouble because some applications already used overridden
key sequences.

## Contribution

I'm really beginner of xterm, so let me know if you have better idea.
Pull Requests are always welcome :)
