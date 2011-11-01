" vim: filetype=vim

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

" Ruby
if has("autocmd")
  augroup ruby
    au!
    au FileType ruby call MyRubyAddons()
  augroup end
endif
