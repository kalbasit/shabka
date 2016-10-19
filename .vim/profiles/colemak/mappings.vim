" Surround plugin
" Copied from https://github.com/tpope/vim-surround/blob/e49d6c2459e0f5569ff2d533b4df995dd7f98313/plugin/surround.vim#L578-L596
" TODO: complete as needed
nmap ws  <Plug>Csurround

vnoremap <leader>rv :call ExtractVariable()<cr>
nnoremap <leader>ri :call InlineVariable()<cr>

nnoremap <leader>. :call OpenTestAlternate()<cr>
nnoremap <leader><leader> <c-^>

" Remap F1 to ESC
map <F1> <ESC>
vmap <F1> <ESC>
nmap <F1> <ESC>
imap <F1> <ESC>

" format the entire file
nnoremap <leader>fef :normal! gg=G``<CR>

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
nmap <silent> <leader>fc <ESC>/\v^[<=>]{7}( .*\|$)<CR>

" Toggle hlsearch with <leader>hs
nmap <leader>hs :set hlsearch! hlsearch?<CR>

" Bubble single lines
nmap <C-i> [e
nmap <C-e> ]e

" Bubble multiple lines
vmap <C-i> [egv
vmap <C-e> ]egv

" Wipe out all buffers
if has('nvim')
  nmap <silent> <leader>wa :execute 'bdelete' join(filter(range(1, bufnr('$')), 'bufexists(v:val) && getbufvar(v:val, "&buftype") isnot# "terminal"'))<cr>
elseif has("patch-7.4.585")
  nmap <silent> <leader>wa :enew \| 1,$bd<cr>
else
  nmap <silent> <leader>wa :1,9000bd<cr>
endif

" clear the search buffer when hitting return
nnoremap <CR> :nohlsearch<cr>

" Don't use Ex mode, use Q for formatting
map Q gq

" make horizontal scrolling easier
nmap <silent> <C-o> 10jl
nmap <silent> <C-i> 10jh

" Add/Remove lineend from listchars
nmap <leader>sle :set listchars+=eol:$<CR>
nmap <leader>hle :set listchars-=eol:$<CR>
