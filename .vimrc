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
Plug 'garyburd/go-explorer', { 'for': 'go' }
" CSV
Plug 'chrisbra/csv.vim', { 'for': 'csv' }
" Ruby/Rails
Plug 'tpope/vim-rails', { 'for': 'ruby' }
" Pig
Plug 'motus/pig.vim', { 'for': 'pig' }
" HTML
Plug 'mattn/emmet-vim', { 'for': ['html', 'css'] }
" Vimperator
Plug 'vimperator/vimperator.vim'

""""""""""""""""
" Colorschemes "
""""""""""""""""
Plug 'altercation/vim-colors-solarized'
Plug 'junegunn/seoul256.vim'
Plug 'fatih/molokai'
Plug 'vim-scripts/summerfruit256.vim'
Plug 'nanotech/jellybeans.vim'

"""""""""
" Tools "
"""""""""

" YouCompleteMe
" Install the plugin separately for nvim and vim.
if has('nvim')
  Plug 'Valloric/YouCompleteMe', { 'dir': '~/.vim/bundle/nvim-YouCompleteMe', 'do': 'python2 install.py --clang-completer --system-libclang --gocode-completer --tern-completer --racer-completer' }
else
  Plug 'Valloric/YouCompleteMe', { 'dir': '~/.vim/bundle/vim-YouCompleteMe', 'do': 'python2 install.py --clang-completer --system-libclang --gocode-completer --tern-completer --racer-completer' }
endif
autocmd! User YouCompleteMe if !has('vim_starting') | call youcompleteme#Enable() | endif

if executable("task")
  Plug 'blindFS/vim-taskwarrior'
endif
if executable("curl")
  Plug 'mattn/webapi-vim' | Plug 'mattn/gist-vim', { 'on': 'Gist' }
endif
if has("python")
  Plug 'sjl/gundo.vim', { 'on': 'GundoToggle' }
endif
if has("ruby")
  Plug 'felipec/notmuch-vim'
endif
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'bling/vim-airline'
Plug 'tpope/vim-obsession'
Plug 'bronson/vim-trailing-whitespace'
Plug 'christoomey/vim-tmux-navigator'
Plug 'editorconfig/editorconfig-vim'
Plug 'ervandew/screen', { 'on': 'ScreenShell' }
Plug 'ervandew/supertab'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all --no-update-rc' } | Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align', { 'on': ['<Plug>(EasyAlign)', 'EasyAlign'] }
Plug 'junegunn/vim-github-dashboard', { 'on': ['GHDashboard', 'GHActivity']      }
Plug 'navicore/vissort.vim', { 'on': 'Vissort' }
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'scrooloose/syntastic'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch', { 'on': ['Remove', 'Unlink', 'Move', 'Rename', 'Chmod', 'Mkdir', 'Find', 'Locate', 'Wall', 'SudoWrite', 'SudoEdit'] }
Plug 'tpope/vim-fugitive', { 'on': ['Git', 'Gcd', 'Glcd', 'Gstatus', 'Gcommit', 'Gmerge', 'Gpull', 'Ggrep', 'Glgrep', 'Glog', 'Gllog', 'Ge', 'Gedit', 'Gpedit', 'Gsplit', 'Gvsplit', 'Gtabedit', 'Gread', 'Gwrite', 'Gw', 'Gwq', 'Gpush', 'Gfetch', 'Gdiff', 'Gvdiff', 'Gsdiff', 'Gbrowse'] }
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
" disable unimpaired as there is no way to disable mappings and it's messing
" up with my colemak mappins. (mapped yo and yO).
" Plug 'tpope/vim-unimpaired'
Plug 'troydm/zoomwintab.vim', { 'on': 'ZoomWinTabToggle' }
Plug 'tyru/caw.vim' " Comment plugin:
                    " - gci / gcui to comment/uncomment the current line
                    " - gco to insert a new line below the current line and
                    "   start a comment.
                    " - gcO to insert a new line above the current line and
                    "   start a comment.
                    " - gcw / gcuw to comment / uncomment a block (/* ... */)
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

color seoul256

let mapleader = ","             " set the mapleader
set backup                      " enable backup, written to backupdir set below
set backupdir^=~/.vim/_backup// " where to put backup files.
set cmdheight=2                 " the height of the command line, giving it a high
                                " number can prevent the "Hit ENTER to continue" but
                                " will shorten the editor.
set colorcolumn=80              " Display a color column
set complete=.,w,b,t,i          " Same as default except that I remove the 'u' option
set directory^=~/.vim/_swap//   " where to put swap files.
set hidden                      " you can change buffer without saving
set ignorecase                  " searches are case insensitive...
set lz                          " do not redraw while running macros (much faster) (LazyRedraw)
set matchtime=2                 " how many tenths of a second to blink matching brackets for
set noerrorbells                " don't make noise
set novisualbell                " don't blink
set number                      " turn on line numbers but display them as relative to the current line
set report=1                    " tell us when anything is changed via :...
set ruler                       " Always show current positions along the bottom
set shortmess=atTIc             " shortens messages to avoid 'press a key' prompt
set showmatch                   " show matching brackets
set smartcase                   " ... unless they contain at least one capital letter
set scrolloff=5                 " Keep 10 lines (top/bottom) for scope
set undodir^=~/.vim/_undo//     " where to put undo files.
set whichwrap+=<,>,h,l          " backspace and cursor keys wrap to
set wildchar=<TAB>              " Which character activates the wildmenu
set winwidth=79                 " Set the minimum window width
set diffopt+=iwhite             " Add ignorance of whitespace to diff
set makeef=error.err            " When using make, where should it dump the file
set noautowrite                 " safe automacially content
set pastetoggle=<F12>           " Paste toggle on key F12!
set shell=/bin/bash             " Use bash no matter what shell are we running
set showfulltag                 " When completing by tag, show the whole tag, not just the function name
set spell                       " Turn on spellcheck.
set splitbelow                  " Always split under
set splitright                  " Always split on the right
set startofline                 " Move the cursor to the first non-blank of the line

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
set wildignore+=*/vendor/*

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
  au FileType go nmap <Leader>gd <Plug>(go-def)
  au FileType go nmap <Leader>gi <Plug>(go-info)
  au FileType go nmap <Leader>gds <Plug>(go-def-split)
  au FileType go nmap <Leader>gdv <Plug>(go-def-vertical)
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
"" Polyglot{{{
""

let g:polyglot_disabled = ['go', 'terraform', 'csv', 'ruby']

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
let g:AutoPairsMultilineClose = 0
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

" https://github.com/junegunn/fzf.vim/issues/133
if executable("ag")
  function! s:with_agignore(bang, args)
    let agignore = '/tmp/agignore-for-fzf'
    let entries = split(&wildignore, ',')
    let source = 'ag --path-to-agignore '.agignore.' -g ""'
    call writefile(entries, agignore)
    call fzf#vim#files(a:args, extend(fzf#vim#layout(a:bang), {'source': source}))
  endfunction

  autocmd VimEnter * command! -bang -nargs=? -complete=dir Files
        \ call s:with_agignore(<bang>0, <q-args>)<Paste>
endif

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
