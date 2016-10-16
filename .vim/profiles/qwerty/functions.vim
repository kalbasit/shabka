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
  if &ft = "go"
    exec "normal! O" . name . " := "
  else
    exec "normal! O" . name . " = "
  endif
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
