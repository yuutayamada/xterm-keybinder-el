# xterm-keybinder

This package lets you key binds that normally terminal Emacs can not use.
(i.e., C-M-g)

## Summary of available key binds

| Modifiers  | Description                          |
|:-----------|:-------------------------------------|
| S          | Space key with shift
| C          | [2-8] (note: xterm.el supports other key binds)
| C-S        | [A-Z]
| C-M        | [a-z]
| M-S        | [A-Z]
| C-M-S      | [A-Z]
| s or s-S   | from space to "~" (almost 8 bits characters without control sequences)
| H or H-S   | same as s-S, but use Hyper modifier

Also this package enables C-m and C-i keybinds as A-m and A-i, but
if you don't set A-m and A-i keys, automatically those keys refer
to original keybinds. So basically you don't need care about that until
you want to configure those keys. (See also enable C-m and C-i keys section)

## Prerequisite
1. XTerm (of course)
2. XTerm's configuration

   You may need to configure your .Xresources file if you don't use XTerm yet.
   (You can update by xrdb command)

   ```sh
   XTerm*VT100.eightBitInput: false
   XTerm*modifyOtherKeys: 1
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

## Enable C-m and C-i keys
Like GUI Emacs, this package provides similar feature.
To activate, this feature you just use "A-" prefix to bind keys.

```lisp
(global-set-key (kbd "A-i") '(lambda () (interactive) (print "C-i key"))
(global-set-key (kbd "A-m") '(lambda () (interactive) (print "C-m key"))
```

Also below configuration might help to use either terminal Emacs and
GUI Emacs.

```lisp
(global-set-key (kbd "C-i")
                (lambda () (interactive)
                  (call-interactively
                   (if (display-graphic-p)
                       (lookup-key global-map (kbd "A-i"))
                     (lookup-key global-map [tab])))))
(global-set-key (kbd "C-m")
                (lambda () (interactive)
                  (call-interactively
                   (if (display-graphic-p)
                       (lookup-key global-map (kbd "A-m"))
                     (lookup-key global-map [return])))))
```

You can bind keys return and tab keys by using [return] and [tab].

## Contribution

I'm really beginner of xterm, so let me know if you have better idea.
Pull Requests are always welcome :)
