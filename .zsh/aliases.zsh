# Conditional aliases
[[ -x "`which ack-grep 2> /dev/null`" ]] && alias ack="ack-grep -il"
[[ -x "`which nvim 2> /dev/null`" ]] && alias vim="nvim"

# Aliases
alias vi=vim
alias e="${EDITOR:-vim}"
alias pw="ps aux | grep -v grep | grep -e"
alias serve_this="python2 -m SimpleHTTPServer"
alias rserve_this="ruby -rrack -e \"Rack::Handler::WEBrick.run Rack::Directory.new('.')\""
alias history='fc -fl 1'
alias comp=docker-compose
alias mach=docker-machine
alias http='http --print=HhBb'
alias blaze=bazel
alias rot13="tr '[A-Za-z]' '[N-ZA-Mn-za-m]'"
alias kp='sp kill'
alias dmx='sp dmx'
alias tt='sp talentoday'
# Always enable colored `grep` output
# Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias remove_dead_containers="docker rm -v \$(docker ps -a -q -f status=exited)"
alias remove_created_containers="docker rm -v \$(docker ps -a -q -f status=created)"
alias remove_dangling_images="docker rmi \$(docker images -f "dangling=true" -q)"
alias irc='tmux attach -t irc || tmux new -s irc irssi'
alias gl='github_commit_link'
alias utf8test='curl -L https://github.com/tmux/tmux/raw/master/tools/UTF-8-demo.txt'

# General aliases
alias -g rocker_auth="--auth kalbasit:\$(lpass show --password 4984935876)"

# Mac only
if [[ "$(uname)" = "Darwin" ]]; then
  alias mac_install_cert='sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain'
  alias upup='brew upgrade'
fi

# Linux only
if [[ "$(uname)" = "Linux" ]]; then
  alias upup='yaourt -Syu --aur'
fi
