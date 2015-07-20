# xterm-keybinder

This package let your terminal Emacs C-[;:',.0-9]-keys,
C-S-[a-z]-keys, C-M-[a-z]-keys and C-M-S-[a-z]-keys in xterm.

## Usage

Put below configuration to your .emacs

```lisp
(when (getenv "XTERM_VERSION")
  (add-hook 'terminal-init-xterm-hook 'xterm-keybinder-setup))
```

Then start your emacs with xterm and the option

```sh
#!/bin/sh
xtermopt=path/to/this-repository/xterm-option
eval "xterm -xrm `${xtermopt}` -e emacsclient -t -a ''"
```

On your started Emacs, you can use C-M prefix and C-S prefix keybinds.

FYI, I'm configuring like [this](https://github.com/yuutayamada/emacs.d/blob/master/emacs.sh)

## Note

If you put xterm-option of this package in your xterm resource,
you may have trouble because some applications already used overridden
key sequences.

Also you may need following configuration at .Xresources and update it by
xrdb command.

```sh
XTerm*VT100.eightBitInput: false
XTerm*modifyOtherKeys: 1
XTerm*vt100.formatOtherKeys: 1
```

## Contribution

I'm really beginner of xterm, so let me know if you have better idea.
Pull Requests are always welcome :)
