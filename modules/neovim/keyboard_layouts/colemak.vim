""
"" Colemak keyboard bindings
""

"" AutoPairs{{{

" disable shortcuts, <A-n> conflicts with Colemak movement
let g:AutoPairsShortcutJump = ""

" }}}
"" Golang{{{
""

" configure vim-go to show errors in the quickfix window and not the location list.
let g:go_list_type = "quickfix"

" disable the default mapping {if} and {af}, conflicts with Colemak
" See mappings.vim for remapping
let g:go_textobj_enabled = 0

" disable go doc mappings
let g:go_doc_keywordprg_enabled = 0

" disable go def mappings
let g:go_def_mapping_enabled = 0

if has("autocmd")
  " Go
  au FileType go nmap <Leader>gc <Plug>(go-doc)
  au FileType go nmap <Leader>gd <Plug>(go-def)
  au FileType go nmap <Leader>sgd <Plug>(go-def-split)
  au FileType go nmap <Leader>vgd <Plug>(go-def-vertical)
  au FileType go nmap <Leader>gi <Plug>(go-info)
endif

" }}}
"" Ruby{{{
""

" disable the default mapping {if} and {af}, conflicts with Colemak
" See mappings.vim for remapping
let g:no_ruby_maps = 1

" }}}
