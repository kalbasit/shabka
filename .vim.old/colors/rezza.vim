" local syntax file - set colors on a per-machine basis:
" vim: tw=0 ts=4 sw=4
" Vim color file
" Based on zellner, by Ron Aaron
" Maintainer:   Doug Whiteley <dougwhiteley@gmail.com>
" Last Change:  Mon 12 Dec 2005

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "rezza"

hi Comment term=bold cterm=Italic ctermfg=DarkRed
hi Normal guifg=black guibg=white
hi Constant term=underline ctermfg=DarkMagenta
hi Special term=bold cterm=bold ctermfg=Magenta
hi Identifier term=underline ctermfg=DarkBlue
hi Statement term=bold cterm=bold ctermfg=DarkRed
hi PreProc term=underline ctermfg=DarkGreen
hi Type term=underline ctermfg=Blue
hi Visual term=reverse ctermfg=Yellow ctermbg=Red
hi Search term=reverse ctermfg=Black ctermbg=Cyan
hi Tag term=bold cterm=bold ctermfg=DarkGreen
hi Error term=reverse ctermfg=15 ctermbg=9
hi Todo term=standout ctermbg=Yellow ctermfg=Black
hi StatusLine term=bold,reverse cterm=bold ctermfg=Yellow ctermbg=Black
hi StatusLineNC term=reverse cterm=none ctermfg=Yellow ctermbg=Black
hi VertSplit term=reverse cterm=none ctermfg=Yellow ctermbg=Black
hi LineNR ctermfg=DarkYellow
hi TabLine ctermfg=White ctermbg=DarkGreen
hi link TabLineSel Normal
hi TabLineFill ctermfg=White ctermbg=DarkGreen
hi! link MoreMsg Comment
hi! link ErrorMsg Visual
hi! link WarningMsg ErrorMsg
hi! link Question Comment
hi link String  Constant
hi link Character   Constant
hi link Number  Constant
hi link Boolean Constant
hi link Float       Number
hi link Function    Identifier
hi link Conditional Statement
hi link Repeat  Statement
hi link Label       Statement
hi link Operator    Statement
hi link Keyword Statement
hi link Exception   Statement
hi link Include PreProc
hi link Define  PreProc
hi link Macro       PreProc
hi link PreCondit   PreProc
hi link StorageClass    Type
hi link Structure   Type
hi link Typedef Type
hi link SpecialChar Special
hi link Delimiter   Special
hi link SpecialComment Special
hi link Debug       Special
