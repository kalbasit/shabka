" vim: filetype=vim

" Ruby addons
function! MyRubyAddons()
  " Call MyRubyAbbreviations
  call MyRubyAbbreviations()
endfunction

" Define my own abbreviations
function! MyRubyAbbreviations()
  ab pry require 'pry'<CR>binding.pry
endfunction
