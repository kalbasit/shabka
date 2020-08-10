"" Settings{{{
""

" set the mapleader
let mapleader = ","

" the height of the command line, giving it a high number can prevent the "Hit
" ENTER to continue" but will shorten the editor.
set cmdheight=2

set backup                             " enable backup, written to backupdir set below
set colorcolumn=80                     " Display a color column
set complete=.,w,b,t,i                 " Same as default except that I remove the 'u' option
set completeopt=menu,noinsert,noselect " Enable completion menu and disable insert/select
set diffopt+=iwhite                    " Add ignorance of whitespace to diff
set hidden                             " you can change buffer without saving
set ignorecase                         " searches are case insensitive...
set lz                                 " do not redraw while running macros (much faster) (LazyRedraw)
set makeef=error.err                   " When using make, where should it dump the file
set matchtime=2                        " how many tenths of a second to blink matching brackets for
set mouse=                             " I hate using the mouse for other than copying/pasting.
set noautowrite                        " safe automacially content
set nobackup                           " Turn off backup
set noerrorbells                       " don't make noise
set novisualbell                       " don't blink
set number                             " turn on line numbers but display them as relative to the current line
set pastetoggle=<F12>                  " Paste toggle on key F12!
set report=1                           " tell us when anything is changed via :...
set ruler                              " Always show current positions along the bottom
set scrolloff=5                        " Keep 10 lines (top/bottom) for scope
set shortmess=atTIc                    " shortens messages to avoid 'press a key' prompt
set showfulltag                        " When completing by tag, show the whole tag, not just the function name
set showmatch                          " show matching brackets
set smartcase                          " ... unless they contain at least one capital letter
set spell                              " Turn on spellcheck.
set splitbelow                         " Always split under
set splitright                         " Always split on the right
set startofline                        " Move the cursor to the first non-blank of the line
set whichwrap+=<,>,h,l                 " backspace and cursor keys wrap to
set wildchar=<TAB>                     " Which character activates the wildmenu
set winwidth=79                        " Set the minimum window width

if v:version >= 703
  set cursorline     " cursor line highlighting
  set nocursorcolumn " no cursor column highlighting
  set undofile       " remember undo chains between sessions
endif

" Whitespace
set noexpandtab  " use tabs
set list         " Show invisible characters
set nowrap       " don't wrap lines
set shiftwidth=2 " Number of spaces to use for each step of (auto)indent.
set tabstop=2    " Number of spaces that a <Tab> in the file counts for.

" Remember things between sessions
"
" '20  - remember marks for 20 previous files
" <50 - save 50 lines for each register
" :20  - remember 20 items in command-line history
" %    - remember the buffer list (if vim started without a file arg)
" n    - set name of viminfo file
set shada='20,<50,:20,%,n~/.config/nvim/_nviminfo

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
"" Ack{{{

let g:ackprg = '@ag_bin@ --vimgrep --smart-case'
cnoreabbrev ag Ack
cnoreabbrev aG Ack
cnoreabbrev Ag Ack
cnoreabbrev AG Ack

map <Leader>/ :Ack<space>

"" }}}
"" Airline{{{

" show tabline
let g:airline#extensions#tabline#enabled = 1

" show ALE
let g:airline#extensions#ale#enabled = 1

"" }}}
"" Ale{{{

nmap <silent> <C-i> <Plug>(ale_previous_wrap)
nmap <silent> <C-e> <Plug>(ale_next_wrap)

" change the linters
let g:ale_linters = {'go': ['gometalinter', 'gofmt']}

" run only fast linters
let g:ale_go_gometalinter_options = "--fast"

""}}}
"" Auto Commands{{{
""

if has("autocmd")
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

  " dbg
  au FileType javascript.jsx ab dbg debugger; // eslint-disable-line
  au FileType typescript ab dbg debugger; // tslint-disable-line

  " Set the envrc filetype to shell
  au BufRead,BufNewFile .envrc set ft=sh

  " Set the Ruby filetype for a number of common Ruby files without .rb
  au BufRead,BufNewFile {Gemfile,Rakefile,Vagrantfile,Thorfile,Procfile,Guardfile,config.ru,*.rake} set ft=ruby

  " make Python follow PEP8 for whitespace.
  " http://www.python.org/dev/peps/pep-0008/
  au FileType python setlocal tabstop=4 shiftwidth=4 expandtab

  " Remember last location in file, but not for commit messages.
  " see :help last-position-jump
  au BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g`\"" | endif

  " Delete certain buffers in order to not cluttering up the buffer list
  au BufReadPost fugitive://* set bufhidden=delete
endif

" }}}
"" AutoPairs{{{

" do not jump to the next line if there's only whitespace after the closing
" pair
let g:AutoPairsMultilineClose = 0

"}}}
"" BetterWhitespace{{{

let g:better_whitespace_enabled=1
let g:strip_whitespace_on_save=1
let g:strip_whitespace_confirm=0
let g:better_whitespace_filetypes_blacklist=['gitsendemail', 'diff', 'gitcommit', 'unite', 'qf', 'help']

"}}}
"" Deoplete{{{

" Run deoplete automatically
let g:deoplete#enable_at_startup = 1

" deoplete-go settings
let g:deoplete#sources#go#json_directory = '~/.cache/deoplete/go/$GOOS_$GOARCH'
let g:deoplete#sources#go#pointer = 1
let g:deoplete#sources#go#package_dot = 1
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
let g:deoplete#sources#go#use_cache = 1
let g:deoplete#sources#go#gocode_binary = '@gocode_bin@'

"" }}}
"" EasyAlign{{{
vmap ga <Plug>(EasyAlign)
"" }}}
"" EasyMotion{{{
""

" change the default prefix to \\
map \\ <Plug>(easymotion-prefix)

" }}}
"" EditorConfig {{{
let g:EditorConfig_exclude_patterns = ['fugitive://.*']
"" }}}
"" FZF {{{

" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1

let g:fzf_action = {
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-s': 'split',
      \ 'ctrl-v': 'vsplit' }

function! s:fzf_statusline()
  " Override statusline as you like
  highlight fzf1 ctermfg=161 ctermbg=251
  highlight fzf2 ctermfg=23 ctermbg=251
  highlight fzf3 ctermfg=237 ctermbg=251
  setlocal statusline=%#fzf1#\ >\ %#fzf2#fz%#fzf3#f
endfunction

autocmd! User FzfStatusLine call <SID>fzf_statusline()

"" }}}
"" Fugitive {{{

" ask Fugitive to not set any mappings
let g:fugitive_no_maps = 1

"" }}}
"" Gist{{{
""

let g:gist_clip_command = '@xsel_bin@ -bi'
let g:gist_show_privates = 1
let g:gist_post_private = 1

" }}}
"" Golang{{{
""

let g:go_def_mode='gopls'
let g:go_fmt_command = "goimports"  " What to run on save.
let g:go_highlight_build_constraints = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_types = 1
let g:go_info_mode='gopls'

" }}}
"" Gundo{{{
nmap <Leader>go :GundoToggle<CR>
"" }}}
"" Markdown{{{

let g:vim_markdown_folding_disabled = 1

"" }}}
"" Multiple cursors{{{

let g:multi_cursor_use_default_mapping=0

" Default mapping
let g:multi_cursor_start_word_key      = '<C-n>'
let g:multi_cursor_start_key           = 'g<C-n>'
let g:multi_cursor_next_key            = '<C-n>'
let g:multi_cursor_prev_key            = '<C-p>'
let g:multi_cursor_skip_key            = '<C-x>'
let g:multi_cursor_quit_key            = '<Esc>'

" Prevent conflict with deoplete
" Called once right before you start selecting multiple cursors
func! Multiple_cursors_before()
  if deoplete#is_enabled()
    call deoplete#disable()
    let g:deoplete_is_enable_before_multi_cursors = 1
  else
    let g:deoplete_is_enable_before_multi_cursors = 0
  endif
endfunc
" Called once only when the multiple selection is canceled (default <Esc>)
func! Multiple_cursors_after()
  if g:deoplete_is_enable_before_multi_cursors
    call deoplete#enable()
  endif
endfunc

"" }}}
"" Polyglot{{{
""

let g:polyglot_disabled = [
  \   'csv',
  \   'go',
  \   'latex',
  \   'markdown',
  \   'ruby',
  \   'scala',
  \   'terraform',
  \   'typescript',
  \ ]

" }}}
"" Scala{{{

" https://github.com/derekwyatt/vim-scala#scaladoc-comment-indentation
let g:scala_scaladoc_indent = 1

"}}}
"" Surround{{{
let g:surround_no_mappings = 1
"}}}
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
"" Terraform{{{
""

let g:terraform_align=1
let g:terraform_fmt_on_save = 1

""}}}
"" ZoomWinTab{{{
nmap <Leader>zo :ZoomWinTabToggle<CR>
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

""""""""""""
" Surround "
""""""""""""

" Copied from https://github.com/tpope/vim-surround/blob/e49d6c2459e0f5569ff2d533b4df995dd7f98313/plugin/surround.vim#L578-L596
" TODO: complete as needed
nmap ws  <Plug>Csurround

"""""""
" FZF "
"""""""

" mapping for files and buffers
nmap <Leader>f :Files<CR>
nmap <Leader>b :Buffers<CR>

" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

""""""""""
" Latex "
""""""""""

let g:tex_flavor = 'latex'

""""""""""
" Golang "
""""""""""

" map the textobj mappings: See settings.vim
" See https://github.com/fatih/vim-go/blob/eb739e185e4729a0ef172da3afed4777d8f64ee6/ftplugin/go.vim#L43
au FileType go onoremap <buffer> <silent> af :<c-u>call go#textobj#Function('a')<cr>
au FileType go onoremap <buffer> <silent> rf :<c-u>call go#textobj#Function('i')<cr>

au FileType go xnoremap <buffer> <silent> af :<c-u>call go#textobj#Function('a')<cr>
au FileType go xnoremap <buffer> <silent> rf :<c-u>call go#textobj#Function('i')<cr>

" Remap ]] and [[ to jump betweeen functions as they are useless in Go
au FileType go nnoremap <buffer> <silent> ]] :<c-u>call go#textobj#FunctionJump('n', 'next')<cr>
au FileType go nnoremap <buffer> <silent> [[ :<c-u>call go#textobj#FunctionJump('n', 'prev')<cr>

au FileType go onoremap <buffer> <silent> ]] :<c-u>call go#textobj#FunctionJump('o', 'next')<cr>
au FileType go onoremap <buffer> <silent> [[ :<c-u>call go#textobj#FunctionJump('o', 'prev')<cr>

au FileType go xnoremap <buffer> <silent> ]] :<c-u>call go#textobj#FunctionJump('v', 'next')<cr>
au FileType go xnoremap <buffer> <silent> [[ :<c-u>call go#textobj#FunctionJump('v', 'prev')<cr>

""""""""""
" Custom "
""""""""""

vnoremap <leader>rv :call ExtractVariable()<cr>
nnoremap <leader>ri :call InlineVariable()<cr>

nnoremap <leader>. :call OpenTestAlternate("")<cr>
nnoremap g<leader>. :call OpenTestAlternate("vsplit")<cr>
nnoremap <leader><leader> <c-^>

" Remap F1 to ESC
map <F1> <ESC>
vmap <F1> <ESC>
nmap <F1> <ESC>
imap <F1> <ESC>

" split navigation
nnoremap <A-n> <C-W><C-H>
nnoremap <A-e> <C-W><C-J>
nnoremap <A-i> <C-W><C-K>
nnoremap <A-o> <C-W><C-L>

" format the entire file
nnoremap <leader>= :normal! gg=G``<CR>

" upper/lower word
" TODO: fix these mappings
nmap <leader>u mQviwU`Q
nmap <leader>l mQviwu`Q

" upper/lower first char of word
" TODO: fix these mappings
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
nmap <silent> <leader>ul :t.<CR>Ar=

" find merge conflict markers
nmap <silent> \fc <ESC>/\v^[<=>]{7}( .*\|$)<CR>

" Toggle hlsearch with <leader>hs
nmap <leader>hs :set hlsearch! hlsearch?<CR>

" save all buffers
nmap <silent> <leader>ww :wall<cr>

" Wipe out all buffers
nmap <silent> <leader>wa :execute 'bdelete' join(filter(range(1, bufnr('$')), 'bufexists(v:val) && getbufvar(v:val, "&buftype") isnot# "terminal"'))<cr>

" clear the search buffer when hitting return
nnoremap <CR> :nohlsearch<cr>

" Add/Remove lineend from listchars
nmap <leader>sle :set listchars+=eol:$<CR>
nmap <leader>hle :set listchars-=eol:$<CR>

" }}}
"" Functions {{{
""

function! ExtractVariable()
  let name = input("Variable name: ")
  if name == ""
    return
  endif
  " Enter visual mode (not sure why this is needed since we're already in
  " visual mode anyway)
  normal! ga

  " Replace selected text with the variable name
  exec "normal c" . name
  " Define the variable on the line above
  if &ft = "go"
    exec "normal! H" . name . " := "
  else
    exec "normal! H" . name . " = "
  endif
  " Paste the original selected text to be the variable value
  normal! $p
endfunction

function! InlineVariable()
  " Copy the variable under the cursor into the 'a' register
  :let l:tmp_a = @a
  :normal "acrw
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
  normal i$
  " Find the next occurence of the variable
  exec '/\<' . @a . '\>'
  " Replace that occurence with the text we yanked
  exec ':.s/\<' . @a . '\>/' . @b
  :let @a = l:tmp_a
  :let @b = l:tmp_b
endfunction

" TODO: Extract this logic into an open-source module
function! OpenTestAlternate(position)
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
  if a:position == "split"
    exec ':sp ' . new_file
  elseif a:position == "vsplit"
    exec ':vsp ' . new_file
  else
    exec ':e ' . new_file
  endif
endfunction

function! AlternateGoFile(current_file)
  let new_file = a:current_file
  if match(a:current_file, '_test\.go$') != -1
    " We are in the test file
    let new_file = substitute(a:current_file, '_test\.go$', '.go', "")
  else
    " We are in the production code file
    let new_file = substitute(a:current_file, '\.go$', '_test.go', "")
  endif

  return new_file
endfunction

function! AlternatePythonFile(current_file)
  let new_file = a:current_file
  if match(a:current_file, '_test\.py$') != -1
    " We are in the test file
    let new_file = substitute(a:current_file, '_test\.py$', '.py', "")
  else
    " We are in the production code file
    let new_file = substitute(a:current_file, '\.py$', '_test.py', "")
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
      let new_file = substitute(new_file, '^app/', "", "")
    end
    if rakefile
      let new_file = substitute(new_file, '\.rake$', '_spec.rb', "")
    else
      let new_file = substitute(new_file, '\.rb$', '_spec.rb', "")
    end
    let new_file = 'spec/' . new_file
  else
    let new_file = substitute(new_file, '_spec\.rb$', '.rb', "")
    let new_file = substitute(new_file, '^spec/', "", "")
    if in_app
      let new_file = 'app/' . new_file
    end

    if !filereadable(new_file)
      let spec_file = substitute(new_file, '\.rb$', '.rake', "")
      if filereadable(spec_file)
        let new_file = spec_file
      endif
    endif
  endif
endfunction

" }}}
