" vim: filetype=vim

" Paste toggle on key F12!
set pastetoggle=<F12>

" ,----
" | Windows
" `---
:set splitbelow   " Always split under
:set splitright   " Always split on the right

" ,----
" | files / backup
" `----
set backupdir=~/.vim/_backup    " where to put backup file
set directory=~/.vim/_temp      " directory is the directory for temp file
set makeef=error.err            " When using make, where should it dump the file
set noautowrite                 " safe automacially content
set modeline                    " activate modelines
set modelines=5                 " the first/last 5 lines can be a modeline
set autoread                    " Automatically read a file that has changed on disk

" ,----
" | viminfo
" `----
" Remember things between sessions
"
" '20  - remember marks for 20 previous files
" \"50 - save 50 lines for each register
" :20  - remember 20 items in command-line history
" %    - remember the buffer list (if vim started without a file arg)
" n    - set name of viminfo file
set viminfo='20,\"50,:20,%,n~/.viminfo,!

" ,----
" | UI / visual cues
" `----
color xoria256          " Xoria256 is good theme for Terminal mode.
set lsp=0               " space it out a little more (easier to read)
set wildmenu            " turn on wild menu
set wildignorecase      " Make it easier to complete buffers, open files, etc...
set ruler               " Always show current positions along the bottom
set cmdheight=1         " the command bar is 1 line high
set relativenumber      " turn on line numbers but display them as relative to the current line
set lz                  " do not redraw while running macros (much faster) (LazyRedraw)
set hidden              " you can change buffer without saving
set backspace=indent,eol,start " allow backspacing over everything in insert mode
set whichwrap+=<,>,h,l  " backspace and cursor keys wrap to
set shortmess=atI       " shortens messages to avoid 'press a key' prompt
set report=1            " tell us when anything is changed via :...
set complete=.,w,b,t    " Same as default except that I remove the 'u' option
set noerrorbells        " don't make noise
set fillchars=vert:\ ,stl:\ ,stlnc:\   " make the splitters between windows be blank
set showmatch           " show matching brackets
set matchtime=2         " how many tenths of a second to blink matching brackets for
set nohlsearch          " do not highlight searched for phrases
set incsearch           " BUT do highlight as you type you search phrase
set list                " Show invisible characters
set listchars=trail:.,extends:>,precedes:< " what to show when I hit :set list
set so=5                " Keep 10 lines (top/bottom) for scope
set novisualbell        " don't blink
set showmode            " show mode in statusline
set startofline         " Move the cursor to the first non-blank of the line
set esckeys             " allow cursor keys in insert mode
set showmatch           " show matching brackets
set wildchar=<TAB>      " Which character activates the wildmenu
set showfulltag         " When completing by tag, show the whole tag, not just the function name
if v:version >= 700
    set nocursorcolumn      " no cursor column highlighting
    set nocursorline        " no cursor line highlighting
endif
if has("gui_running")
  "color railscasts+     " Lovely theme but sucks when in diff mode
  color solarized       " Truely amazing theme.
  set mouse=""          " I hate using the mouse for other than copying/pasting.
  set guioptions=cei    " Set the guioptions I like
  set guifont=Courier\ 10\ Pitch\ 10
endif

" ,----
" | Shell
" `----
if has("gui_macvim")
  set shell = bash        " Always use bash for the shell on mvim
                          " See https://github.com/carlhuda/janus/pull/229
endif

" ,----
" | Diff
" `----
set diffopt+=iwhite     " Add ignorance of whitespace to diff

" ,----
" | snipMate
" `----
let g:snips_author = 'Wael Nasreddine <wael.nasreddine@gmail.com>'

" ,----
" | NERDTree
" `----
"let g:loaded_nerd_tree=1  " Disable NERDTree, it is driving me mad.




" Setting this below makes it sow that error messages don't disappear after
" one second on startup.
" set debug=msg

" Make the 'cw' and like commands put a $ at the end instead of just deleting
" the text and replacing it
" set cpoptions=ces$
