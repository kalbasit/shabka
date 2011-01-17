" Vim color file based on bluegreen
" Maintainer:   Sergey Khorev
" Last Change:
" URL:


" cool help screens
" :he group-name
" :he highlight-groups
" :he cterm-colors

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="northsky"

hi Normal	guifg=white guibg=#061A3E ctermfg=lightgray ctermbg=black

" Search
"hi Search guibg=#A28D68 guifg=bg gui=none
"hi Search guibg=#4668A1 guifg=bg gui=none
hi Search guibg=#3D5B8C guifg=yellow gui=bold ctermbg=darkblue ctermfg=yellow
hi IncSearch	guifg=bg guibg=cyan gui=bold ctermfg=bg ctermbg=cyan

" highlight groups
hi Cursor	guibg=#D74141 guifg=#e3e3e3 ctermfg=white ctermbg=red " ???
hi VertSplit guibg=#C0FFFF guifg=#075554 gui=none ctermfg=darkblue ctermbg=cyan
hi Folded	 guifg=plum1 guibg=#061A3E ctermfg=lightmagenta ctermbg=bg
hi FoldColumn	guibg=#800080 guifg=tan ctermfg=lightgray ctermbg=darkmagenta
hi ModeMsg guifg=#404040 guibg=#C0C0C0 ctermfg=black ctermbg=gray
hi MoreMsg guifg=darkturquoise guibg=#188F90 ctermfg=cyan ctermbg=darkcyan
hi NonText guibg=#334C75 guifg=#9FADC5 ctermfg=gray ctermbg=darkblue
hi Question	guifg=#F4BB7E ctermfg=yellow

hi SpecialKey	guifg=#BF9261 ctermfg=brown
hi StatusLine	guibg=#067C7B guifg=cyan gui=none ctermfg=cyan ctermbg=darkcyan
hi StatusLineNC	guibg=#004443 guifg=DimGrey gui=none ctermbg=darkgray ctermfg=lightgray
hi Title	guifg=#8DB8C3 ctermfg=blue
"hi Visual gui=bold guifg=black guibg=#C0FFC0
hi Visual gui=bold guifg=black guibg=#84AF84 ctermbg=darkgreen ctermfg=black
hi WarningMsg	guifg=#F60000 gui=underline ctermfg=red

" syntax highlighting groups
hi Comment guifg=DarkGray ctermfg=darkgray
hi Constant guifg=#72A5E4 gui=bold ctermfg=lightcyan
hi Number guifg=chartreuse2 gui=bold ctermfg=green
hi Identifier	guifg=#ADCBF1 ctermfg=gray "???
hi Statement guifg=yellow ctermfg=yellow
hi PreProc guifg=#14967C ctermfg=darkgreen
hi Type	guifg=#FFAE66 ctermfg=white "brown
hi Special guifg=#EEBABA ctermfg=brown "darkmagenta
hi Ignore	guifg=grey60 ctermfg=gray "???
hi Todo	guibg=#9C8C84 guifg=#244C0A ctermfg=darkblue ctermbg=darkgray
hi Label guifg=#ffc0c0 ctermfg=darkmagenta

" Vim defaults
hi ErrorMsg guifg=White guibg=Red
hi DiffAdd    guibg=DarkBlue
hi DiffChange guibg=aquamarine4
hi DiffDelete gui=bold guifg=Yellow guibg=DarkBlue
hi DiffText   gui=bold guibg=firebrick3
hi Directory  guifg=Cyan
hi LineNr     guifg=DarkGreen
hi WildMenu   guifg=Black guibg=Yellow
" hi lCursor    guifg=bg guibg=fg
hi lCursor guibg=SeaGreen1 guifg=NONE ctermbg=lightgreen ctermfg=NONE " ???
hi Underlined gui=underline guifg=#80a0ff
hi Error      guifg=White guibg=Red
