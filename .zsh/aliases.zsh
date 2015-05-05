[[ -x "`which ack-grep 2> /dev/null`" ]] && alias ack="ack-grep -il"
alias pw="ps aux | grep -v grep | grep -e"
alias serve_this="python -m SimpleHTTPServer"
alias rserve_this="ruby -rrack -e \"Rack::Handler::WEBrick.run Rack::Directory.new('.')\""
alias email="emacsclient -a '' -s mail -t"
alias history='fc -fl 1'
alias xcopy='xclip -selection clipboard'
alias zs='zssh'
alias backup_code='rsync -auz --progress --delete ~/code/src/ wmn@192.168.1.13:code/src/'
alias tmx='tmux -f "${TMUXDOTDIR:-$HOME}/.tmux.conf"'
alias vi=vim
alias e="${EDITOR:-vim}"
alias comp=docker-compose
alias mach=docker-machine
alias fuck='$(thefuck $(fc -ln -1))'
alias http='http --print=HhBb'
alias blaze=bazel
