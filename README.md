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

## Prerequisite
1. XTerm (of course)
2. XTerm's configuration

   You may need to configure your .Xresources file if you don't use XTerm yet.
   (You can update by xrdb command)

   ```sh
   XTerm*VT100.eightBitInput: false
   XTerm*vt100.formatOtherKeys: 1
   ```

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

## Contribution

I'm really beginner of xterm, so let me know if you have better idea.
Pull Requests are always welcome :)
