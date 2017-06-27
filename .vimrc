" vim:foldmethod=marker:foldlevel=0:

"" Profiles{{{

if !exists('g:kb_profile')
  let g:kb_profile = 'colemak'
endif
let g:kb_profiles = { 'qwerty': expand("~/.vim/profiles/qwerty"), 'colemak': expand("~/.vim/profiles/colemak") }

" }}}
"" Plug{{{
""

 " Note: Skip initialization for vim-tiny or vim-small.
if !1 | finish | endif

if has('vim_starting') && !has('nvim') && &compatible
  set nocompatible               " Be iMproved
endif

" Required:
call plug#begin(expand('~/.vim/bundle/'))

""""""""""""""
" Languages  "
""""""""""""""

" Multi-lang plugin
Plug 'sheerun/vim-polyglot'
" Terraform
Plug 'hashivim/vim-terraform'
" Go
Plug 'fatih/vim-go', { 'for': 'go' } | Plug 'majutsushi/tagbar'
" CSV
Plug 'chrisbra/csv.vim', { 'for': 'csv' }
" Ruby/Rails
Plug 'tpope/vim-rails', { 'for': 'ruby' }
" Pig
Plug 'motus/pig.vim', { 'for': 'pig' }
" HTML
Plug 'mattn/emmet-vim', { 'for': ['html', 'css'] }

""""""""""""""""
" Colorschemes "
""""""""""""""""
Plug 'altercation/vim-colors-solarized'
Plug 'junegunn/seoul256.vim'

""""""""""""""""""""""""""
" AutoComplete & Snippet "
""""""""""""""""""""""""""

" deoplete for nvim, YouCompleteMe for vim
if has('nvim')
  " the main plugin
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  " Golang support
  Plug 'zchee/deoplete-go', { 'do': 'make && mkdir -p ~/.cache/deoplete/go'}
  " Typescript support
  Plug 'mhartington/nvim-typescript'
  " Ruby support
  Plug 'fishbullet/deoplete-ruby'
  " ZSH support
  Plug 'zchee/deoplete-zsh'
else
  Plug 'Valloric/YouCompleteMe', { 'dir': '~/.vim/bundle/vim-YouCompleteMe', 'do': 'python2 install.py --clang-completer --system-libclang --gocode-completer --racer-completer' }
  autocmd! User YouCompleteMe if !has('vim_starting') | call youcompleteme#Enable() | endif
endif

"""""""""""""""
" Look & Feel "
"""""""""""""""

" airline is a status bar
Plug 'bling/vim-airline'

" fzf for fuzzy-search a file
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all --no-update-rc' } | Plug 'junegunn/fzf.vim'

"""""""""""""
" Externals "
"""""""""""""

if executable('task')
  " revert to the original fork when
  " https://github.com/blindFS/vim-taskwarrior/pull/137 is merged
  " Plug 'blindFS/vim-taskwarrior'
  Plug 'kalbasit/vim-taskwarrior', { 'tag': 'add_option_disable_mappings' }
endif

if executable('curl')
  Plug 'mattn/webapi-vim' | Plug 'mattn/gist-vim', { 'on': 'Gist' }
endif

if executable('ag')
  Plug 'mileszs/ack.vim'
endif

"""""""""""
" Editing "
"""""""""""

if has("python")
  Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
endif

" load editorconfig if available
Plug 'editorconfig/editorconfig-vim'

" this script will explode arrays, functions arguments into a multi-line
Plug 'FooSoft/vim-argwrap'

" show trailing whitespace in red. It also strips whitespace on save (See
" settings for it below). To disable it, use :ToggleStripWhitespaceOnSave and
" to strip manually do :StripWhiteSpace
Plug 'ntpeters/vim-better-whitespace'

" show git status in the sign column next to every line
Plug 'mhinz/vim-signify'

" auto insert the closing pair
Plug 'jiangmiao/auto-pairs'

" align visual blocks
Plug 'junegunn/vim-easy-align', { 'on': ['<Plug>(EasyAlign)', 'EasyAlign'] }

" sort visual block
Plug 'navicore/vissort.vim', { 'on': 'Vissort' }

" file browser
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

" check file for errors
Plug 'scrooloose/syntastic'

" multiple cursors
Plug 'terryma/vim-multiple-cursors'

" auto insert the end of a block, for example in Ruby tying `do<CR>` will
" automatically insert `end` on the third line
Plug 'tpope/vim-endwise'

" Commands for file management and editing
Plug 'tpope/vim-eunuch', { 'on': ['Remove', 'Unlink', 'Move', 'Rename', 'Chmod', 'Mkdir', 'Find', 'Locate', 'Wall', 'SudoWrite', 'SudoEdit'] }

" Git!
" NOTE: vim-fugitive cannot be loaded lazily
" https://github.com/junegunn/vim-plug/issues/164#issuecomment-73621232 There
" is a way to do it
" (https://github.com/junegunn/vim-plug/issues/525#issuecomment-256169881),
" however, I chose to keep it simple
Plug 'tpope/vim-fugitive' | Plug 'tpope/vim-rhubarb'

" surround
Plug 'tpope/vim-surround' | Plug 'tpope/vim-repeat'

" C-a and C-x to increment/decrement dates/times
Plug 'tpope/vim-speeddating' | Plug 'tpope/vim-repeat'

" like tmux prefix + z. Zoom on one buffer
Plug 'troydm/zoomwintab.vim', { 'on': 'ZoomWinTabToggle' }

" Comment plugin:
" - gci / gcui to comment/uncomment the current line
" - gco to insert a new line below the current line and
"   start a comment.
" - gcO to insert a new line above the current line and
"   start a comment.
" - gcw / gcuw to comment / uncomment a block (/* ... */)
Plug 'tyru/caw.vim'

" Remove EOL on save
Plug 'vim-scripts/PreserveNoEOL'

exe 'source ' . g:kb_profiles[g:kb_profile] . '/plug.vim'

" All of your Plugins must be added before the following line
call plug#end()

" Required:
filetype plugin on
if !has('nvim')
  filetype plugin indent on
endif

" }}}
"" Settings{{{
""

" set background=dark
colorscheme seoul256

let mapleader = ","                    " set the mapleader
set backup                             " enable backup, written to backupdir set below
set backupdir^=~/.vim/_backup//        " where to put backup files.
set cmdheight=2                        " the height of the command line, giving it a high
                                       " number can prevent the "Hit ENTER to continue" but
                                       " will shorten the editor.
set colorcolumn=80                     " Display a color column
set complete=.,w,b,t,i                 " Same as default except that I remove the 'u' option
set completeopt=menu,noinsert,noselect " Enable completion menu and disable insert/select
set directory^=~/.vim/_swap//          " where to put swap files.
set hidden                             " you can change buffer without saving
set ignorecase                         " searches are case insensitive...
set lz                                 " do not redraw while running macros (much faster) (LazyRedraw)
set matchtime=2                        " how many tenths of a second to blink matching brackets for
set noerrorbells                       " don't make noise
set novisualbell                       " don't blink
set number                             " turn on line numbers but display them as relative to the current line
set report=1                           " tell us when anything is changed via :...
set ruler                              " Always show current positions along the bottom
set shortmess=atTIc                    " shortens messages to avoid 'press a key' prompt
set showmatch                          " show matching brackets
set smartcase                          " ... unless they contain at least one capital letter
set scrolloff=5                        " Keep 10 lines (top/bottom) for scope
set undodir^=~/.vim/_undo//            " where to put undo files.
set whichwrap+=<,>,h,l                 " backspace and cursor keys wrap to
set wildchar=<TAB>                     " Which character activates the wildmenu
set winwidth=79                        " Set the minimum window width
set diffopt+=iwhite                    " Add ignorance of whitespace to diff
set makeef=error.err                   " When using make, where should it dump the file
set noautowrite                        " safe automacially content
set pastetoggle=<F12>                  " Paste toggle on key F12!
set shell=/bin/bash                    " Use bash no matter what shell are we running
set showfulltag                        " When completing by tag, show the whole tag, not just the function name
set spell                              " Turn on spellcheck.
set splitbelow                         " Always split under
set splitright                         " Always split on the right
set startofline                        " Move the cursor to the first non-blank of the line

if v:version >= 703
  set cursorline     " cursor line highlighting
  set nocursorcolumn " no cursor column highlighting
  set undofile       " remember undo chains between sessions
endif

" Whitespace
set expandtab    " use spaces, not tabs
set list         " Show invisible characters
set nowrap       " don't wrap lines
set shiftwidth=2 " an autoindent (with <<) is two spaces
set tabstop=2    " a tab is two spaces

" We have to have a winheight bigger than we want to set winminheight. But if
" we set winheight to be huge before winminheight, the winminheight set will
" fail.
set winheight=5
set winminheight=5   " must come before setting winheight to 999
set winheight=999

" Remember things between sessions
"
" '20  - remember marks for 20 previous files
" <50 - save 50 lines for each register
" :20  - remember 20 items in command-line history
" %    - remember the buffer list (if vim started without a file arg)
" n    - set name of viminfo file
if has('nvim')
  set shada='20,<50,:20,%,n~/.nvim/_nviminfo
else
  set viminfo='20,\"50,:20,%,n~/.vim/_viminfo
endif

" }}}
"" ViM only settings, these are default in nvim. See nvim-defaults{{{
if !has('nvim')
  set esckeys                     " allow cursor keys in insert mode
  set autoread                   " Automatically read a file that has changed on disk
  set backspace=indent,eol,start " backspace through everything in insert mode
  set hlsearch                   " highlight matches
  set incsearch                  " incremental searching
  set wildmenu                   " turn on wild menu

  if has('vim_starting')
    " The default encoding for nvim is utf-8. It's not possible to change it
    " either. See http://neovim.io/doc/user/options.html#%27encoding%27
    set encoding=utf-8      " Set default encoding to UTF-8
  endif

  if has("statusline")
    set laststatus=2  " always show the status bar
  endif
endif
" }}}
"" NeoVim Settings{{{
if has('nvim')
  set mouse=  " I hate using the mouse for other than copying/pasting.
endif
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

" Disable the vendor directory Go 1.5+
" TODO: This setting makes it impossible for me to go ahead and open files
" inside the vendor directory. Does removing it affects my workflow? Does fzf
" still ignores based on .gitignore?
" set wildignore+=*/vendor/*

" Disable node/TypeScript
set wildignore+=*/node_modules/*,*/typings/*,*/dist/*

" Disable the build folder, usually used by java
set wildignore+=*/build/*

" }}}
"" List chars {{{
""

set listchars=""                  " Reset the listchars
set listchars=tab:\ \             " a tab should display as "  "
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
  au FileType go nmap <Leader>gc <Plug>(go-doc)
  au FileType go nmap <Leader>gd <Plug>(go-def)
  au FileType go nmap <Leader>sgd <Plug>(go-def-split)
  au FileType go nmap <Leader>vgd <Plug>(go-def-vertical)
  au FileType go nmap <Leader>gi <Plug>(go-info)
endif

" }}}
"" Terraform{{{
""

let g:terraform_fmt_on_save = 1

""}}}
"" Golang{{{
""

let g:go_fmt_command = "goimports"  " What to run on save.
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_fields = 1
let g:go_highlight_types = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
" configure vim-go to show errors in the quickfix window and not the location list.
let g:go_list_type = "quickfix"

" }}}
"" TaskWarrior{{{
""

" set the size to 30%. Default: 15%
let g:task_info_size = 30

" disable the default mappings. Default: 0
let g:task_disable_mappings = 1

" mappings
augroup TaskwarriorMapping
    autocmd!
    autocmd FileType taskreport nmap <silent> <buffer> A        <Plug>(taskwarrior_annotate)
    autocmd FileType taskreport nmap <silent> <buffer> x        <Plug>(taskwarrior_denotate)
    autocmd FileType taskreport nmap <silent> <buffer> p        <Plug>(taskwarrior_open_annotate)
    autocmd FileType taskreport nmap <silent> <buffer> D        <Plug>(taskwarrior_remove)
    autocmd FileType taskreport nmap <silent> <buffer> <Del>    <Plug>(taskwarrior_delete)
    autocmd FileType taskreport nmap <silent> <buffer> a        <Plug>(taskwarrior_new)
    autocmd FileType taskreport nmap <silent> <buffer> c        <Plug>(taskwarrior_command)
    autocmd FileType taskreport nmap <silent> <buffer> d        <Plug>(taskwarrior_done)
    autocmd FileType taskreport nmap <silent> <buffer> r        <Plug>(taskwarrior_report)
    autocmd FileType taskreport nmap <silent> <buffer> R        <Plug>(taskwarrior_refresh)
    autocmd FileType taskreport nmap <silent> <buffer> X        <Plug>(taskwarrior_clear_completed)
    autocmd FileType taskreport nmap <silent> <buffer> u        <Plug>(taskwarrior_undo)
    autocmd FileType taskreport nmap <silent> <buffer> U        <Plug>(taskwarrior_urgency)
    autocmd FileType taskreport nmap <silent> <buffer> S        <Plug>(taskwarrior_sync)
    autocmd FileType taskreport nmap <silent> <buffer> m        <Plug>(taskwarrior_modify_field)
    autocmd FileType taskreport nmap <silent> <buffer> M        <Plug>(taskwarrior_modify_task)
    autocmd FileType taskreport nmap <silent> <buffer> v        <Plug>(taskwarrior_paste)
    autocmd FileType taskreport nmap <silent> <buffer> +        <Plug>(taskwarrior_start_task)
    autocmd FileType taskreport nmap <silent> <buffer> -        <Plug>(taskwarrior_stop_task)
    autocmd FileType taskreport nmap <silent> <buffer> <Space>  <Plug>(taskwarrior_select)
    autocmd FileType taskreport nmap <silent> <buffer> <C-A>    <Plug>(taskwarrior_increase)
    autocmd FileType taskreport nmap <silent> <buffer> <C-X>    <Plug>(taskwarrior_decrease)
    autocmd FileType taskreport vmap <silent> <buffer> d        <Plug>(taskwarrior_visual_done)
    autocmd FileType taskreport vmap <silent> <buffer> D        <Plug>(taskwarrior_visual_delete)
    autocmd FileType taskreport vmap <silent> <buffer> <Del>    <Plug>(taskwarrior_visual_delete)
    autocmd FileType taskreport vmap <silent> <buffer> <Space>  <Plug>(taskwarrior_visual_select)
augroup END

" }}}
"" Polyglot{{{
""

let g:polyglot_disabled = ['go', 'terraform', 'csv', 'ruby']

" }}}
"" EasyAlign{{{
vmap ga <Plug>(EasyAlign)
"" }}}
"" Gundo{{{
nmap <Leader>go :GundoToggle<CR>
"" }}}
"" ZoomWinTab{{{
nmap <Leader>zo :ZoomWinTabToggle<CR>
"" }}}
"" Airline{{{

let g:airline#extensions#tabline#enabled = 1

" use seoul256 theme
let g:airline_theme='seoul256'

"" }}}
"" Deoplete{{{

if has('nvim')
  " Run deoplete.nvim automatically
  let g:deoplete#enable_at_startup = 1

  " deoplete-go settings
  let g:deoplete#sources#go#gocode_binary = '~/code/bin/gocode'
  let g:deoplete#sources#go#json_directory = '~/.cache/deoplete/go/$GOOS_$GOARCH'
  let g:deoplete#sources#go#pointer = 1
  let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
  let g:deoplete#sources#go#use_cache = 1
endif

"" }}}
"" Ack{{{

let g:ackprg = 'ag --vimgrep --smart-case'
cnoreabbrev ag Ack
cnoreabbrev aG Ack
cnoreabbrev Ag Ack
cnoreabbrev AG Ack

map <Leader>/ :Ack<space>


"" }}}
"" ArgWrap{{{

nnoremap <silent> <leader>a :ArgWrap<CR>

"" }}}
"" Syntastic{{{
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }
""}}}
"" TagBar{{{
let g:tagbar_type_go = {
    \ 'ctagstype' : 'go',
    \ 'kinds'     : [
        \ 'p:package',
        \ 'i:imports:1',
        \ 'c:constants',
        \ 'v:variables',
        \ 't:types',
        \ 'n:interfaces',
        \ 'w:fields',
        \ 'e:embedded',
        \ 'm:methods',
        \ 'r:constructor',
        \ 'f:functions'
    \ ],
    \ 'sro' : '.',
    \ 'kind2scope' : {
        \ 't' : 'ctype',
        \ 'n' : 'ntype'
    \ },
    \ 'scope2kind' : {
        \ 'ctype' : 't',
        \ 'ntype' : 'n'
    \ },
    \ 'ctagsbin'  : 'gotags',
    \ 'ctagsargs' : '-sort -silent'
\ }
""}}}
"" AutoPairs{{{

" do not jump to the next line if there's only whitespace after the closing
" pair
let g:AutoPairsMultilineClose = 0

" disable shortcuts, <A-n> conflicts with Colemak movement
let g:AutoPairsShortcutJump = ''

"}}}
"" BetterWhitespace{{{

autocmd BufEnter * EnableStripWhitespaceOnSave

"}}}
"" Surround{{{
let g:surround_no_mappings = 1
"}}}
"" FZF {{{

" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1

let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit' }

if has('nvim')
  function! s:fzf_statusline()
    " Override statusline as you like
    highlight fzf1 ctermfg=161 ctermbg=251
    highlight fzf2 ctermfg=23 ctermbg=251
    highlight fzf3 ctermfg=237 ctermbg=251
    setlocal statusline=%#fzf1#\ >\ %#fzf2#fz%#fzf3#f
  endfunction

  autocmd! User FzfStatusLine call <SID>fzf_statusline()
endif

"" }}}
"" EditorConfig {{{
let g:EditorConfig_exclude_patterns = ['fugitive://.*']
"" }}}
"" Profile settings {{{
exe 'source ' . g:kb_profiles[g:kb_profile] . '/settings.vim'
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

exe 'source ' . g:kb_profiles[g:kb_profile] . '/mappings.vim'

" }}}
"" Functions {{{
""

exe 'source ' . g:kb_profiles[g:kb_profile] . '/functions.vim'

" }}}
