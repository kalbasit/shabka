"" TODO: Clean up this mess!


""
"" Vundle
""

set nocompatible    " be iMproved, required
filetype off        " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Colorschemes
Plugin 'altercation/vim-colors-solarized'
Plugin 'fatih/molokai'

" Coffescript
Plugin 'kchmck/vim-coffee-script'

" Golang
Plugin 'fatih/vim-go'

" Ruby/Rails
if has("ruby")
  Plugin 'vim-ruby/vim-ruby'
endif
Plugin 'skwp/vim-rspec'
Plugin 'skalnik/vim-vroom'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-endwise'

" Tools
Plugin 'kien/ctrlp.vim'
Plugin 'jeetsukumaran/vim-buffergator'
if executable("curl")
  Plugin 'mattn/gist-vim'
endif
if has("python")
  Plugin 'sjl/gundo.vim'
endif
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'ervandew/supertab'
Plugin 'itspriddle/ZoomWin'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

""
"" Functions
""

function! OpenTestAlternate()
  let new_file = AlternateForCurrentFile()
  exec ':e ' . new_file
endfunction
function! AlternateForCurrentFile()
  let current_file = expand("%")
  let new_file = current_file
  let in_spec = match(current_file, '^spec/') != -1
  let going_to_spec = !in_spec
  let rakefile = match(current_file, '\.rake$') != -1
  let in_app = match(current_file, '\<controllers\>') != -1 || match(current_file, '\<models\>') != -1 || match(current_file, '\<views\>') != -1
  if going_to_spec
    if in_app
      let new_file = substitute(new_file, '^app/', '', '')
    end
    if rakefile
      let new_file = substitute(new_file, '\.rake$', '_spec.rb', '')
    else
      let new_file = substitute(new_file, '\.rb$', '_spec.rb', '')
    end
    let new_file = 'spec/' . new_file
  else
    let new_file = substitute(new_file, '_spec\.rb$', '.rb', '')
    let new_file = substitute(new_file, '^spec/', '', '')
    if in_app
      let new_file = 'app/' . new_file
    end

    if !filereadable(new_file)
      let spec_file = substitute(new_file, '\.rb$', '.rake', '')
      if filereadable(spec_file)
        let new_file = spec_file
      endif
    endif
  endif
  return new_file
endfunction

""
"" Defaults
""

" Use bash, this fixes issues with using a non-POSIX shell like fish.
set shell=/bin/bash

" Paste toggle on key F12!
set pastetoggle=<F12>

" Remap F1 to ESC
:map <F1> <ESC>
:vmap <F1> <ESC>
:nmap <F1> <ESC>
:imap <F1> <ESC>

" ,----
" | Windows
" `---
:set splitbelow   " Always split under
:set splitright   " Always split on the right

" ,----
" | files / backup
" `----
set makeef=error.err            " When using make, where should it dump the file
set noautowrite                 " safe automacially content
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
let mapleader = ","
color molokai           " Use solarized colorscheme.
set background=light    " Use a light background.
set lsp=0               " space it out a little more (easier to read)
set wildmenu            " turn on wild menu
"set wildignorecase     " Make it easier to complete buffers, open files, etc...
set ruler               " Always show current positions along the bottom
set cmdheight=1         " the command bar is 1 line high
set number              " turn on line numbers but display them as relative to the current line
set winwidth=79         " Set the minimum window width
set colorcolumn=80      " Display a color column

" We have to have a winheight bigger than we want to set winminheight. But if
" we set winheight to be huge before winminheight, the winminheight set will
" fail.
set winheight=5
set winminheight=5
set winheight=999

set lz                  " do not redraw while running macros (much faster) (LazyRedraw)
set hidden              " you can change buffer without saving
set whichwrap+=<,>,h,l  " backspace and cursor keys wrap to
set shortmess=atI       " shortens messages to avoid 'press a key' prompt
set report=1            " tell us when anything is changed via :...
set complete=.,w,b,t    " Same as default except that I remove the 'u' option
set noerrorbells        " don't make noise
set fillchars=vert:\ ,stl:\ ,stlnc:\   " make the splitters between windows be blank
set showmatch           " show matching brackets
set matchtime=2         " how many tenths of a second to blink matching brackets for
set hlsearch            " do not highlight searched for phrases
set incsearch           " BUT do highlight as you type you search phrase
set so=5                " Keep 10 lines (top/bottom) for scope
set novisualbell        " don't blink
set showmode            " show mode in statusline
set startofline         " Move the cursor to the first non-blank of the line
set esckeys             " allow cursor keys in insert mode
set showmatch           " show matching brackets
hi User1 term=inverse,bold cterm=inverse,bold ctermfg=red
set wildchar=<TAB>      " Which character activates the wildmenu
set showfulltag         " When completing by tag, show the whole tag, not just the function name
if v:version >= 700
    set nocursorcolumn      " no cursor column highlighting
    set nocursorline        " no cursor line highlighting
endif
if has("gui_running")
  set mouse=""          " I hate using the mouse for other than copying/pasting.
  set guioptions=cei    " Set the guioptions I like
  set guifont=Inconsolata-dz:h12
endif

""
"" Status line
""

if has("statusline") && !&cp
  set laststatus=2  " always show the status bar

  " Start the status line
  set statusline=%f\ %m\ %r
  set statusline+=Line:%l/%L[%p%%]
  set statusline+=Col:%v
  set statusline+=Buf:#%n
  set statusline+=[%b][0x%B]
endif

""
"" Basic Setup
""

set nocompatible      " Use vim, no vi defaults
set number            " Show line numbers
set ruler             " Show line and column number
syntax enable         " Turn on syntax highlighting allowing local overrides
set encoding=utf-8    " Set default encoding to UTF-8

""
"" Whitespace
""

set nowrap                        " don't wrap lines
set tabstop=2                     " a tab is two spaces
set shiftwidth=2                  " an autoindent (with <<) is two spaces
set expandtab                     " use spaces, not tabs
set list                          " Show invisible characters
set backspace=indent,eol,start    " backspace through everything in insert mode

if exists("g:enable_mvim_shift_arrow")
  let macvim_hig_shift_movement = 1 " mvim shift-arrow-keys
endif

" List chars
set listchars=""                  " Reset the listchars
set listchars=tab:\ \             " a tab should display as "  ", trailing whitespace as "."
set listchars+=trail:.            " show trailing spaces as dots
set listchars+=extends:>          " The character to show in the last column when wrap is
                                  " off and the line continues beyond the right of the screen
set listchars+=precedes:<         " The character to show in the last column when wrap is
                                  " off and the line continues beyond the left of the screen

""
"" Searching
""

set hlsearch    " highlight matches
set incsearch   " incremental searching
set ignorecase  " searches are case insensitive...
set smartcase   " ... unless they contain at least one capital letter

""
"" Wild settings
""

" TODO: Investigate the precise meaning of these settings
" set wildmode=list:longest,list:full

" Disable output and VCS files
set wildignore+=*.o,*.out,*.obj,.git,*.rbc,*.rbo,*.class,.svn,*.gem

" Disable archive files
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz

" Ignore bundler and sass cache
set wildignore+=*/vendor/gems/*,*/vendor/cache/*,*/.bundle/*,*/.sass-cache/*

" Ignore librarian-chef, vagrant, test-kitchen and Berkshelf cache
set wildignore+=*/tmp/librarian/*,*/.vagrant/*,*/.kitchen/*,*/vendor/cookbooks/*

" Ignore rails temporary asset caches
set wildignore+=*/tmp/cache/assets/*/sprockets/*,*/tmp/cache/assets/*/sass/*

" Disable temp and backup files
set wildignore+=*.swp,*~,._*

""
"" Backup and swap files
""

set backupdir^=~/.vim/_backup//    " where to put backup files.
set directory^=~/.vim/_temp//      " where to put swap files.

""
"" Auto Commands
""

if has("autocmd")
  " In Makefiles, use real tabs, not tabs expanded to spaces
  au FileType make setlocal noexpandtab

  " Set the Ruby filetype for a number of common Ruby files without .rb
  au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,Procfile,Guardfile,config.ru,*.rake} set ft=ruby

  " Make sure all mardown files have the correct filetype set and setup wrapping
  au BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn,txt} setf markdown
  au FileType markdown setlocal wrap linebreak textwidth=72 nolist

  " Treat JSON files like JavaScript
  au BufNewFile,BufRead *.json set ft=javascript

  " make Python follow PEP8 for whitespace ( http://www.python.org/dev/peps/pep-0008/ )
  au FileType python setlocal tabstop=4 shiftwidth=4

  " Remember last location in file, but not for commit messages.
  " see :help last-position-jump
  au BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g`\"" | endif

  if has("gui_running")
    " Automatically resize splits when resizing MacVim window
    autocmd VimResized * wincmd =
  endif
endif

" ,----
" | Diff
" `----
set diffopt+=iwhite     " Add ignorance of whitespace to diff

" ,----
" | CtrlP
" `----
let g:ctrlp_working_path_mode = 0
let g:ctrlp_max_height = 100

" ,----
" | Vroom
" `----
let g:vroom_write_all = 1

""
"" Command-Line Mappings
""

" After whitespace, insert the current directory into a command-line path
cnoremap <expr> <C-P> getcmdline()[getcmdpos()-2] ==# ' ' ? expand('%:p:h') : "\<C-P>"

""
"" General Mappings (Normal, Visual, Operator-pending)
""

" Toggle paste mode
nmap <silent> <F4> :set invpaste<CR>:set paste?<CR>
imap <silent> <F4> <ESC>:set invpaste<CR>:set paste?<CR>

" format the entire file
nnoremap <leader>fef :normal! gg=G``<CR>

" upper/lower word
nmap <leader>u mQviwU`Q
nmap <leader>l mQviwu`Q

" upper/lower first char of word
nmap <leader>U mQgewvU`Q
nmap <leader>L mQgewvu`Q

" cd to the directory containing the file in the buffer
nmap <silent> <leader>cd :lcd %:h<CR>

" Create the directory containing the file in the buffer
nmap <silent> <leader>md :!mkdir -p %:p:h<CR>

" Some helpers to edit mode
" http://vimcasts.org/e/14
nmap <leader>ew :e <C-R>=expand('%:h').'/'<cr>
nmap <leader>es :sp <C-R>=expand('%:h').'/'<cr>
nmap <leader>ev :vsp <C-R>=expand('%:h').'/'<cr>
nmap <leader>et :tabe <C-R>=expand('%:h').'/'<cr>

" Swap two words
nmap <silent> gw :s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR>`'

" Underline the current line with '='
nmap <silent> <leader>ul :t.<CR>Vr=

" set text wrapping toggles
nmap <silent> <leader>tw :set invwrap<CR>:set wrap?<CR>

" find merge conflict markers
nmap <silent> <leader>fc <ESC>/\v^[<=>]{7}( .*\|$)<CR>

" Map the arrow keys to be based on display lines, not physical lines
map <Down> gj
map <Up> gk

" Toggle hlsearch with <leader>hs
nmap <leader>hs :set hlsearch! hlsearch?<CR>

" Adjust viewports to the same size
map <Leader>= <C-w>=

if has("gui_macvim") && has("gui_running")
  " Map command-[ and command-] to indenting or outdenting
  " while keeping the original selection in visual mode
  vmap <D-]> >gv
  vmap <D-[> <gv

  nmap <D-]> >>
  nmap <D-[> <<

  omap <D-]> >>
  omap <D-[> <<

  imap <D-]> <Esc>>>i
  imap <D-[> <Esc><<i

  " Bubble single lines
  nmap <D-Up> [e
  nmap <D-Down> ]e
  nmap <D-k> [e
  nmap <D-j> ]e

  " Bubble multiple lines
  vmap <D-Up> [egv
  vmap <D-Down> ]egv
  vmap <D-k> [egv
  vmap <D-j> ]egv

  " Map Command-# to switch tabs
  map  <D-0> 0gt
  imap <D-0> <Esc>0gt
  map  <D-1> 1gt
  imap <D-1> <Esc>1gt
  map  <D-2> 2gt
  imap <D-2> <Esc>2gt
  map  <D-3> 3gt
  imap <D-3> <Esc>3gt
  map  <D-4> 4gt
  imap <D-4> <Esc>4gt
  map  <D-5> 5gt
  imap <D-5> <Esc>5gt
  map  <D-6> 6gt
  imap <D-6> <Esc>6gt
  map  <D-7> 7gt
  imap <D-7> <Esc>7gt
  map  <D-8> 8gt
  imap <D-8> <Esc>8gt
  map  <D-9> 9gt
  imap <D-9> <Esc>9gt
else
  " Map command-[ and command-] to indenting or outdenting
  " while keeping the original selection in visual mode
  vmap <A-]> >gv
  vmap <A-[> <gv

  nmap <A-]> >>
  nmap <A-[> <<

  omap <A-]> >>
  omap <A-[> <<

  imap <A-]> <Esc>>>i
  imap <A-[> <Esc><<i
 
  " Bubble single lines
  nmap <C-Up> [e
  nmap <C-Down> ]e
  nmap <C-k> [e
  nmap <C-j> ]e

  " Bubble multiple lines
  vmap <C-Up> [egv
  vmap <C-Down> ]egv
  vmap <C-k> [egv
  vmap <C-j> ]egv

  " Make shift-insert work like in Xterm
  map <S-Insert> <MiddleMouse>
  map! <S-Insert> <MiddleMouse>

  " Map Control-# to switch tabs
  map  <C-0> 0gt
  imap <C-0> <Esc>0gt
  map  <C-1> 1gt
  imap <C-1> <Esc>1gt
  map  <C-2> 2gt
  imap <C-2> <Esc>2gt
  map  <C-3> 3gt
  imap <C-3> <Esc>3gt
  map  <C-4> 4gt
  imap <C-4> <Esc>4gt
  map  <C-5> 5gt
  imap <C-5> <Esc>5gt
  map  <C-6> 6gt
  imap <C-6> <Esc>6gt
  map  <C-7> 7gt
  imap <C-7> <Esc>7gt
  map  <C-8> 8gt
  imap <C-8> <Esc>8gt
  map  <C-9> 9gt
  imap <C-9> <Esc>9gt
endif

" Wipe out all buffers
nmap <silent> <leader>wa :1,9000bd<cr>

" clear the search buffer when hitting return
nnoremap <CR> :nohlsearch<cr>

" Don't use Ex mode, use Q for formatting
map Q gq

" Source vim files
map <leader>svb :source ~/.vimrc.before<cr>
map <leader>sva :source ~/.vimrc.after<cr>

" Remap the tab key to do autocompletion or indentation depending on the
" context (from http://www.vim.org/tips/tip.php?tip_id=102)
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>

" spelling
nmap <leader>sn ]s
nmap <leader>sp [s
nmap <leader>s= z=
nmap <leader>sg zG
nmap <leader>sm zW
nmap <leader>se :set spelllang=en<CR>
nmap <leader>sf :set spelllang=fr<CR>
nmap <C-X>s wi<C-X>s

" Insert my name and email
nmap <silent> <leader>me aWael Nasreddine <wael.nasreddine@gmail.com><ESC>

" The following beast is something i didn't write... it will return the
" syntax highlighting group that the current "thing" under the cursor
" belongs to -- very useful for figuring out what to change as far as
" syntax highlighting goes.
nmap <silent> <leader>qq :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" Maps to make handling windows a bit easier
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" Make horizontal scrolling easier
nmap <silent> <C-o> 10zl
nmap <silent> <C-i> 10zh

" Use CTRL-E to replace the original ',' mapping
nnoremap <C-E> ,

" ,----
" | Modifying defaults options
" `----
nmap <leader>elc :set listchars+=eol:$<CR>
nmap <leader>rlc :set listchars-=eol:$<CR>

" ,----
" | Par
" `----
if executable("par")
  nnoremap <silent> <leader>fp vip:!par -w<c-r>=&tw<cr><cr>
  xnoremap <silent> <leader>fp :!par -w<c-r>=&tw<cr><cr>
endif

""
"" Abbreviations
""

" pry
ab pryme require "pry"; binding.pry

" footnotes
ab ~0 [0]<esc>m`:/^--\s*/-2/<CR>o<CR>Footnotes:<CR>----------<CR>[0]
ab ~1 [1]<esc>m`:/^Footnotes\:/+2/<CR>o[1]
ab ~2 [2]<esc>m`:/^Footnotes\:/+3/<CR>o[2]
ab ~3 [3]<esc>m`:/^Footnotes\:/+4/<CR>o[3]
ab ~4 [4]<esc>m`:/^Footnotes\:/+5/<CR>o[4]
ab ~5 [5]<esc>m`:/^Footnotes\:/+6/<CR>o[5]
ab ~6 [6]<esc>m`:/^Footnotes\:/+7/<CR>o[6]
ab ~7 [7]<esc>m`:/^Footnotes\:/+8/<CR>o[7]
ab ~8 [8]<esc>m`:/^Footnotes\:/+9/<CR>o[8]
ab ~9 [9]<esc>m`:/^Footnotes\:/+10/<CR>o[9]

""
"" Typos
""

"" simple corrections
iab alos        also
iab aslo        also
iab bianry      binary
iab bianries    binaries
iab charcter    character
iab charcters   characters
iab exmaple     example
iab exmaples    examples
iab shoudl      should
iab seperate    separate
iab teh         the
iab Srever      Server
iab tpyo        typo

"" greetings

iab rr Regards,<cr>Wael Nasreddine
iab grr Greetings,<cr>Wael Nasreddine

""
"" Commands
""

" Seriously, guys. It's not like :W is bound to anything anyway.
command! W :w
command! Wa :wa
command! Xa :xa

""
"" Functions and their bindings
""

function! ExtractVariable()
  let name = input("Variable name: ")
  if name == ''
    return
  endif
  " Enter visual mode (not sure why this is needed since we're already in
  " visual mode anyway)
  normal! gv

  " Replace selected text with the variable name
  exec "normal c" . name
  " Define the variable on the line above
  exec "normal! O" . name . " = "
  " Paste the original selected text to be the variable value
  normal! $p
endfunction

function! InsertTabWrapper()
  let col = col('.') - 1
  if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
  else
    return "\<c-p>"
  endif
endfunction

function! InlineVariable()
  " Copy the variable under the cursor into the 'a' register
  :let l:tmp_a = @a
  :normal "ayiw
  " Delete variable and equals sign
  :normal 2daW
  " Delete the expression into the 'b' register
  :let l:tmp_b = @b
  :normal "bd$
  " Delete the remnants of the line
  :normal dd
  " Go to the end of the previous line so we can start our search for the
  " usage of the variable to replace. Doing '0' instead of 'k$' doesn't
  " work; I'm not sure why.
  normal k$
  " Find the next occurence of the variable
  exec '/\<' . @a . '\>'
  " Replace that occurence with the text we yanked
  exec ':.s/\<' . @a . '\>/' . @b
  :let @a = l:tmp_a
  :let @b = l:tmp_b
endfunction

vnoremap <leader>rv :call ExtractVariable()<cr>
nnoremap <leader>ri :call InlineVariable()<cr>

" Map keys to go to specific files
map <leader>gr :topleft :split config/routes.rb<cr>
function! ShowRoutes()
  " Requires 'scratch' plugin
  :topleft 100 :split __Routes__
  " Make sure Vim doesn't write __Routes__ as a file
  :set buftype=nofile
  " Delete everything
  :normal 1GdG
  " Put routes output in buffer
  :0r! rake -s routes
  " Size window to number of lines (1 plus rake output length)
  :exec ":normal " . line("$") . _ "
  " Move cursor to bottom
  :normal 1GG
  " Delete empty trailing line
  :normal dd
endfunction
map <leader>gR :call ShowRoutes()<cr>
map <leader>ga :CtrlP app/assets<cr>
map <leader>gC :CtrlP contao<cr>
map <leader>gv :CtrlP app/views<cr>
map <leader>gc :CtrlP app/controllers<cr>
map <leader>gm :CtrlP app/models<cr>
map <leader>gh :CtrlP app/helpers<cr>
map <leader>gl :CtrlP lib<cr>
map <leader>gp :CtrlP public<cr>
map <leader>gs :CtrlP public/stylesheets/sass<cr>
map <leader>gf :CtrlP features<cr>
map <leader>gg :topleft 100 :split Gemfile<cr>
map <leader>gt :CtrlPTag<cr>
map <leader>F :CtrlP %%<cr>

nnoremap <leader>. :call OpenTestAlternate()<cr>
nnoremap <leader><leader> <c-^>

" Force myself to learn the hjkl
map <Left> :echo "no!"<cr>
map <Right> :echo "no!"<cr>
map <Up> :echo "no!"<cr>
map <Down> :echo "no!"<cr>

" CoffeeScript
vmap <leader>c <esc>:'<,'>:CoffeeCompile<CR>
map <leader>c :CoffeeCompile<CR>
command! -nargs=1 C CoffeeCompile | :<args>

" Convert 1.8 hash to 1.9
command! ConvertHashStyle :%s/\([^:]\):\([a-zA-Z_]*\)\s*=>/\1\2:/g
