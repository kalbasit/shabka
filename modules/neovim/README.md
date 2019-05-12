<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [NeoVim config files](#neovim-config-files)
- [Keyboard layout and movement](#keyboard-layout-and-movement)
- [Developement](#developement)
  - [Auto completion](#auto-completion)
  - [Languages](#languages)
    - [Golang](#golang)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# NeoVim config files

This is NeoVim configuration module.

# Keyboard layout and movement

NeoVim is configured around the Colemak keyboard layout. From the [Colemak
README][6]:

```
Colemak layout:                  |                 QWERTY layout:
`12345 67890-=     Move around:  |  (instead of)   `12345 67890-=
 qwfpg jluy;[]\         i        |       k          qwert yuiop[]\
 arstd hNEIO'         n   o      |     h   l        asdfg HJKL;'
 zxcvb km,./            e        |       j          zxcvb nm,./

(  novx)  n = h (Left)     o = l (Right)     i = k (Up)     e = j (Down)

(  novx)  l = b (Back word)            L = B (Back WORD)
(  novx)  y = w (Forward word)         Y = W (Forward WORD)
(  novx)  u = e (Forward end of word)  U = E (Forward end of WORD)

(c     )  <C-L> = <C-Left> (Back WORD)
(c     )  <C-Y> = <C-Right> (Seems to equal forward WORD minus 1 character)

(  n  x)  a = v (Visual)                  A = V (Visual line)
(  n   )  r = r (Replace)                 R = R (Replace)
(  n   )  s = i (Insert)                  S = I (Insert before first non-blank of line)
(  n   )  h = o (Insert new line below)   H = O (Insert new line above)
(  n   )  t = a (Append)                  T = A (Append at end of line)
(  n   )  w = c (Change)                  W = C (Change to end of line)  ww = cc (Change line)

(  n  x)  z = u (Undo)    Z = <C-R> (Redo)  gz = U (Undo all latest changes on line)
(  n  x)  x = x (Cut)     X = dd (Cut line)
(  n  x)  c = y (Copy)    C = yy (Copy line)
(  n  x)  v = p (Paste)   V = P (Paste)
(  n  x)  gv = gp (Paste) gV = gP (Paste)

(   o  )  r = i (Example: dip -> drp (Delete inner paragraph))

(  no x)  p = t{char} (Before next {char})  P = T{char} (After previous {char})
(  no x)  b = ; (Repeat latest f or t)  B = , (Repeat latest f or t reversed)
(  no x)  k = n (Repeat latest / or ?)  K = N (Repeat latest / or ? reversed)

(  n  x)  j = z
(  n  x)  jn = zj (Next fold) [Also jj = zj]
(  n  x)  je = zk (Previous fold) [Also jk = zk]

(  n   )  ga = gv (Reselect last visual selection)
(  n  x)  gK = K (Lookup)
(  n  x)  gL = L (To line [count] from bottom of window)

(  n  x)  <C-W>n = <C-W>h (Window left)
(  n  x)  <C-W>e = <C-W>j (Window down)
(  n  x)  <C-W>i = <C-W>k (Window up)
(  n  x)  <C-W>o = <C-W>l (Window right)

Lost:
(  n  x)  H (To line [count] from top of window)
(  n  x)  s (Substitute [count] characters) [Use wi = cl]
(  n  x)  S (Substitute [count] lines) [Use ww = cc]
(  n  x)  X (Cut [count] characters backwards) [Use dh = dh]
(  n   )  Z (Quit)
(  n  x)  <C-W>n (Window down) [Use <C-W><C-N> = <C-W><C-N>]
(  n  x)  <C-W>i (Window down) [Use <C-W><C-I> = <C-W><C-I>]

Legend:
<C-X>     Ctrl-X
(c     )  Command-line mode
( i    )  Insert mode
(  n   )  Normal mode
(   o  )  Operator pending
(    v )  Visual+Select mode
(     x)  Visual mode
```

# Developement

## Auto completion

Auto completion is provided by [deoplete](2). Supported languages:

- Golang
- Typescript
- Ruby
- ZSH

## Languages

### Golang

Golang support is provided by [fatih/vim-go](1), auto-completion is
as explained above, syntax check, linting and vetting is provided by
[Ale](4).

[1]: https://github.com/fatih/vim-go
[2]: https://github.com/Shougo/deoplete.nvim
[3]: https://github.com/Valloric/YouCompleteMe
[4]: https://github.com/w0rp/ale
[5]: https://github.com/
[6]: https://github.com/kalbasit/vim-colemak
