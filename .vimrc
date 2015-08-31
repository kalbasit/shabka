" vim:foldmethod=marker:foldlevel=0:

"" Plug{{{
""

 " Note: Skip initialization for vim-tiny or vim-small.
if !1 | finish | endif

if has('vim_starting')
  if &compatible
    set nocompatible               " Be iMproved
  endif
endif

" Required:
call plug#begin(expand('~/.vim/bundle/'))

" Colorschemes
Plug 'altercation/vim-colors-solarized'
Plug 'junegunn/seoul256.vim'
Plug 'fatih/molokai'
Plug 'vim-scripts/summerfruit256.vim'

" Coffescript
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }

" Golang
Plug 'fatih/vim-go', { 'for': 'go' }

" JSON
Plug 'elzr/vim-json', { 'for': 'json' }

" CSV
Plug 'chrisbra/csv.vim', { 'for': 'csv' }

" Ruby/Rails
if has("ruby")
  Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
endif
Plug 'skwp/vim-rspec', { 'for': 'ruby' }
Plug 'tpope/vim-rails', { 'for': 'ruby' }
Plug 'tpope/vim-endwise', { 'for': 'ruby' }

" Tools
Plug 'bling/vim-airline'
Plug 'scrooloose/syntastic'
Plug 'junegunn/vim-easy-align'
Plug 'jeetsukumaran/vim-buffergator', { 'on': 'BuffergatorOpen' }
if executable("curl")
  Plug 'mattn/webapi-vim' | Plug 'mattn/gist-vim', { 'on': 'Gist' }
endif
if has("python")
  Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
endif
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'itspriddle/ZoomWin', { 'on': 'ZoomWin' }
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'ervandew/screen', { 'on': 'ScreenShell' }
Plug 'ervandew/supertab'
Plug 'Valloric/YouCompleteMe', { 'do': 'python install.py --clang-completer --system-libclang --gocode-completer' }
Plug 'editorconfig/editorconfig-vim'
Plug 'vim-scripts/PreserveNoEOL'
Plug 'bronson/vim-trailing-whitespace'
Plug 'tpope/vim-eunuch', { 'on': ['Remove', 'Unlink', 'Move', 'Rename', 'Chmod', 'Mkdir', 'Find', 'Locate', 'Wall', 'SudoWrite', 'SudoEdit'] }
Plug 'janko-m/vim-test'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
Plug 'terryma/vim-multiple-cursors'

" All of your Plugins must be added before the following line
call plug#end()

" Required:
filetype plugin on
filetype plugin indent on

" }}}
"" Settings{{{
""

color seoul256

let mapleader = ","     " set the mapleader
set backupdir^=~/.vim/_backup//     " where to put backup files.
set directory^=~/.vim/_temp//       " where to put swap files.
set undodir^=~/.vim/_undo//         " where to put undo files.
set encoding=utf-8      " Set default encoding to UTF-8
set wildmenu            " turn on wild menu
set hlsearch            " highlight matches
set incsearch           " incremental searching
set ignorecase          " searches are case insensitive...
set smartcase           " ... unless they contain at least one capital letter
set wildchar=<TAB>      " Which character activates the wildmenu
set ruler               " Always show current positions along the bottom
set cmdheight=1         " the command bar is 1 line high
set number              " turn on line numbers but display them as relative to the current line
set winwidth=79         " Set the minimum window width
set colorcolumn=80      " Display a color column
set lz                  " do not redraw while running macros (much faster) (LazyRedraw)
set hidden              " you can change buffer without saving
set whichwrap+=<,>,h,l  " backspace and cursor keys wrap to
set shortmess=atI       " shortens messages to avoid 'press a key' prompt
set report=1            " tell us when anything is changed via :...
set complete=.,w,b,t,i  " Same as default except that I remove the 'u' option
set noerrorbells        " don't make noise
set showmatch           " show matching brackets
set matchtime=2         " how many tenths of a second to blink matching brackets for
set so=5                " Keep 10 lines (top/bottom) for scope
set novisualbell        " don't blink
if has("statusline") && !&cp
  set laststatus=2  " always show the status bar
endif
set startofline         " Move the cursor to the first non-blank of the line
set esckeys             " allow cursor keys in insert mode
set showfulltag         " When completing by tag, show the whole tag, not just the function name
set shell=/bin/bash     " Use bash no matter what shell are we running
set diffopt+=iwhite     " Add ignorance of whitespace to diff
set pastetoggle=<F12>   " Paste toggle on key F12!
set makeef=error.err    " When using make, where should it dump the file
set noautowrite         " safe automacially content
set autoread            " Automatically read a file that has changed on disk
set spell               " Turn on spellcheck.
set splitbelow          " Always split under
set splitright          " Always split on the right
if v:version >= 703
  set undofile          " remember undo chains between sessions
  set nocursorcolumn    " no cursor column highlighting
  set cursorline        " cursor line highlighting
endif

" Whitespace
set nowrap                        " don't wrap lines
set tabstop=2                     " a tab is two spaces
set shiftwidth=2                  " an autoindent (with <<) is two spaces
set expandtab                     " use spaces, not tabs
set list                          " Show invisible characters
set backspace=indent,eol,start    " backspace through everything in insert mode

" We have to have a winheight bigger than we want to set winminheight. But if
" we set winheight to be huge before winminheight, the winminheight set will
" fail.
set winheight=5
set winminheight=5
set winheight=999

" Remember things between sessions
"
" '20  - remember marks for 20 previous files
" \"50 - save 50 lines for each register
" :20  - remember 20 items in command-line history
" %    - remember the buffer list (if vim started without a file arg)
" n    - set name of viminfo file
set viminfo='20,\"50,:20,%,n~/.vim/_info

" }}}
"" Gui Settings{{{
""

if has("gui_running")
  set mouse=""          " I hate using the mouse for other than copying/pasting.
  set guioptions=cei    " Set the guioptions I like
  set guifont=Monospace,Fixed\ 11
endif

" }}}
"" Wild settings{{{
""

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

" Disable Godeps workspace
set wildignore+=*/Godeps/_workspace/*

" }}}
"" List chars {{{
""

set listchars=""                  " Reset the listchars
set listchars=tab:\ \             " a tab should display as "  ", trailing whitespace as "."
set listchars+=trail:.            " show trailing spaces as dots
set listchars+=extends:>          " The character to show in the last column when wrap is
                                  " off and the line continues beyond the right of the screen
set listchars+=precedes:<         " The character to show in the last column when wrap is
                                  " off and the line continues beyond the left of the screen

" }}}
"" Auto Commands{{{
""

if has("autocmd")
  " See http://stackoverflow.com/a/3787326/301730
  au BufEnter ?* call PreviewHeightWorkAround()

  " In Makefiles, use real tabs, not tabs expanded to spaces
  au FileType make setlocal noexpandtab

  " In emails allow footnotes
  au FileType mail ab ~0 [0]<esc>m`:/^--\s*/-2/<CR>o<CR>Footnotes:<CR>----------<CR>[0]
  au FileType mail ab ~1 [1]<esc>m`:/^Footnotes\:/+2/<CR>o[1]
  au FileType mail ab ~2 [2]<esc>m`:/^Footnotes\:/+3/<CR>o[2]
  au FileType mail ab ~3 [3]<esc>m`:/^Footnotes\:/+4/<CR>o[3]
  au FileType mail ab ~4 [4]<esc>m`:/^Footnotes\:/+5/<CR>o[4]
  au FileType mail ab ~5 [5]<esc>m`:/^Footnotes\:/+6/<CR>o[5]
  au FileType mail ab ~6 [6]<esc>m`:/^Footnotes\:/+7/<CR>o[6]
  au FileType mail ab ~7 [7]<esc>m`:/^Footnotes\:/+8/<CR>o[7]
  au FileType mail ab ~8 [8]<esc>m`:/^Footnotes\:/+9/<CR>o[8]
  au FileType mail ab ~9 [9]<esc>m`:/^Footnotes\:/+10/<CR>o[9]

  " Set the Ruby filetype for a number of common Ruby files without .rb
  au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,Procfile,Guardfile,config.ru,*.rake} set ft=ruby

  " Make sure all mardown files have the correct filetype set and setup wrapping
  au BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn,txt} set ft=markdown
  au FileType markdown setlocal wrap linebreak textwidth=72 nolist

  " make Python follow PEP8 for whitespace.
  " http://www.python.org/dev/peps/pep-0008/
  au FileType python setlocal tabstop=4 shiftwidth=4

  " Remember last location in file, but not for commit messages.
  " see :help last-position-jump
  au BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g`\"" | endif

  " Delete certain buffers in order to not cluttering up the buffer list
  au BufReadPost fugitive://* set bufhidden=delete

  if has("gui_running")
    " Automatically resize splits when resizing MacVim window
    autocmd VimResized * wincmd =
  endif

  " Go
  au FileType go nmap gd <Plug>(go-def)
  au FileType go nmap <Leader>i <Plug>(go-info)
  au FileType go nmap <Leader>gd <Plug>(go-doc)
  au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
  au FileType go nmap <Leader>gb <Plug>(go-doc-browser)
  au FileType go nmap <leader>r <Plug>(go-run)
  au FileType go nmap <Leader>s <Plug>(go-implements)
  au FileType go nmap <leader>b <Plug>(go-build)
  au FileType go nmap <leader>c <Plug>(go-coverage)
  au FileType go nmap <Leader>ds <Plug>(go-def-split)
  au FileType go nmap <Leader>dv <Plug>(go-def-vertical)
  au FileType go nmap <Leader>dt <Plug>(go-def-tab)
endif

" }}}
"" Go{{{
""

let g:go_fmt_command = "goimports"  " What to run on save.
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
autocmd BufWritePost,FileWritePost *.go execute 'GoLint' | cwindow

" }}}
"" UltiSnips{{{
""

let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1

" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

" :UltiSnipsEdit Split vertically
let g:UltiSnipsEditSplit="vertical"

" }}}
"" ScreenShell{{{
""

let g:ScreenImpl = "Tmux"
let g:ScreenShellTmuxInitArgs = '-2'

" }}}
"" Test{{{
map <silent> <leader>t :TestNearest<CR>
map <silent> <leader>T :TestFile<CR>
map <silent> <leader>a :TestSuite<CR>
map <silent> <leader>l :TestLast<CR>
map <silent> <leader>g :TestVisit<CR>
"}}}
"" EasyAlign{{{
vmap ga <Plug>(EasyAlign)
"" }}}
"" Airline{{{
let g:airline#extensions#tabline#enabled = 1
"" }}}
"" FZF {{{
nnoremap <silent><c-p> :<c-u>FZF!<cr>
"" }}}
"" EditorConfig {{{
let g:EditorConfig_exclude_patterns = ['fugitive://.*']
"" }}}
"" Command-Line Mappings {{{
""

" <c-p> insert the current directory into a command-line, requires at least a space.
cnoremap <expr> <c-p> getcmdline()[getcmdpos()-2] ==# ' ' ? expand('%:p:h') : "\<C-P>"

" W should write the same as w
command! W :w
command! Wa :wa
command! Xa :xa

" }}}
"" General Mappings (Normal, Visual, Operator-pending) {{{
""

vnoremap <leader>rv :call ExtractVariable()<cr>
nnoremap <leader>ri :call InlineVariable()<cr>

nnoremap <leader>. :call OpenTestAlternate()<cr>
nnoremap <leader><leader> <c-^>

" Force myself to learn the hjkl
map <Left> :echo "no!"<cr>
map <Right> :echo "no!"<cr>
map <Up> :echo "no!"<cr>
map <Down> :echo "no!"<cr>

" Remap F1 to ESC
:map <F1> <ESC>
:vmap <F1> <ESC>
:nmap <F1> <ESC>
:imap <F1> <ESC>

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
nmap <silent>gw :s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR>`'

" Underline the current line with '='
nmap <silent> <leader>ul :t.<CR>Vr=

" find merge conflict markers
nmap <silent> <leader>fc <ESC>/\v^[<=>]{7}( .*\|$)<CR>

" Toggle hlsearch with <leader>hs
nmap <leader>hs :set hlsearch! hlsearch?<CR>

" Map command-[ and command-] to indenting or outdenting
" while keeping the original selection in visual mode
vmap <A-]> >gv
vmap <A-[> <gv

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

" Wipe out all buffers
if has("patch-7.4.585")
  nmap <silent> <leader>wa :enew \| 1,$bd<cr>
else
  nmap <silent> <leader>wa :1,9000bd<cr>
endif

" clear the search buffer when hitting return
nnoremap <CR> :nohlsearch<cr>

" Don't use Ex mode, use Q for formatting
map Q gq

" spelling
nmap <leader>sn ]s
nmap <leader>sp [s
nmap <leader>s= z=
nmap <leader>sg zG
nmap <leader>sm zW
nmap <leader>se :set spelllang=en<CR>
nmap <leader>sf :set spelllang=fr<CR>
nmap <C-X>s wi<C-X>s

" make handling windows a bit easier
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" make horizontal scrolling easier
nmap <silent> <C-o> 10zl
nmap <silent> <C-i> 10zh

" Add/Remove lineend from listchars
nmap <leader>sle :set listchars+=eol:$<CR>
nmap <leader>hle :set listchars-=eol:$<CR>

" }}}
"" Functions {{{
""

function! PreviewHeightWorkAround()
  if &previewwindow
    " See http://stackoverflow.com/a/30771487/301730
    exec 'wincmd K'
    exec 'setlocal winheight='.&previewheight
  endif
endfunction

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

function! OpenTestAlternate()
  let current_file = expand("%")
  let new_file = current_file

  if match(current_file, '\.go$') != -1
    let new_file = AlternateGoFile(current_file)
  elseif match(current_file, '\.py$') != -1
    let new_file = AlternatePythonFile(current_file)
  elseif match(current_file, '\.rb$') != -1 || match(current_file, '\.rake$') != -1
    let new_file = AlternateRubyFile(current_file)
  endif

  " Open the alternate file or self if the rules don't match
  exec ':e ' . new_file
endfunction

function! AlternateGoFile(current_file)
  let new_file = a:current_file
  if match(a:current_file, '_test\.go$') != -1
    " We are in the test file
    let new_file = substitute(a:current_file, '_test\.go$', '.go', '')
  else
    " We are in the production code file
    let new_file = substitute(a:current_file, '\.go$', '_test.go', '')
  endif

  return new_file
endfunction

function! AlternatePythonFile(current_file)
  let new_file = a:current_file
  if match(a:current_file, '_test\.py$') != -1
    " We are in the test file
    let new_file = substitute(a:current_file, '_test\.py$', '.py', '')
  else
    " We are in the production code file
    let new_file = substitute(a:current_file, '\.py$', '_test.py', '')
  endif

  return new_file
endfunction

function! AlternateRubyFile(current_file)
  let new_file = a:current_file
  let in_spec = match(a:current_file, '^spec/') != -1
  let going_to_spec = !in_spec
  let rakefile = match(a:current_file, '\.rake$') != -1
  let in_app = match(a:current_file, '\<controllers\>') != -1 || match(a:current_file, '\<models\>') != -1 || match(a:current_file, '\<views\>') != -1
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
endfunction

" }}}
