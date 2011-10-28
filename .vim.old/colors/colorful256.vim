" Vim color file:  colorful256.vim
" Last Change: 03 Oct, 2007
" License: public domain
" Maintainer:: Jagpreet<jagpreetc AT gmail DOT com>
"
" for a 256 color capable terminal
" "{{{
" You must set t_Co=256 before calling this colorscheme
"
" Color numbers (0-255) see:
" http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html

if &t_Co != 256
    echomsg ""
    echomsg "colors not loaded first please set t_Co=256 in your .vimrc"
    echomsg ""
    finish
endif

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name = "colorful256"

 highlight Normal          ctermfg=249 ctermbg=16
 highlight Special         ctermfg=105 ctermbg=16
 highlight Comment         ctermfg=3   ctermbg=16
 highlight Constant        ctermfg=9   ctermbg=16
 highlight LineNr          ctermfg=48  ctermbg=16
 highlight Number          ctermfg=209 ctermbg=16
 highlight PreProc         ctermfg=10  ctermbg=16
 highlight Statement       ctermfg=51  ctermbg=16
 highlight Type            ctermfg=39  ctermbg=16
 highlight Error           ctermfg=9   ctermbg=16
 highlight Identifier      ctermfg=207 ctermbg=16
 highlight SpecialKey      ctermfg=36  ctermbg=16
 highlight NonText         ctermfg=164 ctermbg=16
 highlight Directory       ctermfg=34  ctermbg=16
 highlight ErrorMsg        ctermfg=9   ctermbg=16
 highlight MoreMsg         ctermfg=34  ctermbg=16
 highlight Title           ctermfg=199 ctermbg=16
 highlight WarningMsg      ctermfg=9   ctermbg=16
 highlight DiffDelete      ctermfg=207 ctermbg=16
 highlight Search          ctermfg=15  ctermbg=160
 highlight Visual          ctermfg=16  ctermbg=50
 highlight Cursor          ctermfg=16  ctermbg=33
 highlight StatusLine      ctermfg=58  ctermbg=15
 highlight Question        ctermfg=16  ctermbg=226
 highlight Todo            ctermfg=160 ctermbg=184
 highlight Folded          ctermfg=15  ctermbg=90
 highlight ModeMsg         ctermfg=16  ctermbg=46
 highlight VisualNOS       ctermfg=16  ctermbg=28
 highlight WildMenu        ctermfg=16  ctermbg=226
 highlight FoldColumn      ctermfg=15  ctermbg=58
 highlight SignColumn      ctermfg=16  ctermbg=28
 highlight DiffText        ctermfg=16  ctermbg=34
 highlight StatusLineNC    ctermfg=131 ctermbg=15
 highlight VertSplit       ctermfg=172 ctermbg=15
 highlight User1           ctermbg=20  ctermfg=15
 highlight User2           ctermbg=20  ctermfg=46
 highlight User3           ctermbg=20  ctermfg=46
 highlight User4           ctermbg=20  ctermfg=50
 highlight User5           ctermbg=20  ctermfg=46

" for groups introduced in version 7
if v:version >= 700
   highlight Pmenu           ctermfg=16  ctermbg=165
   highlight PmenuSel        ctermfg=16  ctermbg=220
   highlight tablinesel      ctermfg=15  ctermbg=58
   highlight tabline         ctermfg=7   ctermbg=58
   highlight tablinefill     ctermfg=58  ctermbg=58
endif

"for taglist plugin
"
if exists('loaded_taglist')
   highlight TagListTagName  ctermfg=16  ctermbg=28
   highlight TagListTagScope ctermfg=16  ctermbg=28
   highlight TagListTitle    ctermfg=199 ctermbg=16
   highlight TagListComment  ctermfg=16  ctermbg=28
   highlight TagListFileName ctermfg=15  ctermbg=90
endif
