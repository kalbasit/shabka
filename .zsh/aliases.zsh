# Conditional aliases
[[ -x "`which ack-grep 2> /dev/null`" ]] && alias ack="ack-grep -il"
[[ -x "`which nvim 2> /dev/null`" ]] && alias vim="nvim"

# Aliases
alias vi=vim
alias e="${EDITOR:-vim}"
alias pw="ps aux | grep -v grep | grep -e"
alias serve_this="python -m SimpleHTTPServer"
alias rserve_this="ruby -rrack -e \"Rack::Handler::WEBrick.run Rack::Directory.new('.')\""
alias history='fc -fl 1'
alias comp=docker-compose
alias mach=docker-machine
alias fuck='eval $(thefuck $(fc -ln -1 | tail -n 1)); fc -R'
alias http='http --print=HhBb'
alias blaze=bazel
alias rot13="tr '[A-Za-z]' '[N-ZA-Mn-za-m]'"
alias kp='swp kill'
alias dm='swp dailymotion'
alias tt='swp talentoday'
# Always enable colored `grep` output
# Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# General aliases
alias -g rocker_auth="--auth kalbasit:\$(lpass show --password 4984935876)"
