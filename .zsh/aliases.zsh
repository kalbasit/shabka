[[ -x "`which ack-grep 2> /dev/null`" ]] && alias ack="ack-grep -il"
alias pw="ps aux | grep -v grep | grep -e"
alias serve_this="python -m SimpleHTTPServer"
alias rserve_this="ruby -rrack -e \"Rack::Handler::WEBrick.run Rack::Directory.new('.')\""
alias run_xvfb="Xvfb :4 -screen 0 1280x1024x24"
alias xr="xvfb-run --server-args='-screen 0 1280x1024x24'"
alias xrake="xr bundle exec rake"
alias xrspec="xr bundle exec rspec"
alias xspec="xrake parallel:prepare parallel:spec"
alias e="subl"
alias email="emacsclient -a '' -s mail -t"
alias history='fc -fl 1'
alias xcopy='xclip -selection clipboard'
alias zs='zssh'
alias backup_code='rsync -auz --progress --delete ~/code/src/ 192.168.1.13:code/src/'
