# Attach or Create
alias ac="tmx main"

# ack
if [[ -x "`which ack-grep 2> /dev/null`" ]]; then
  alias ack="ack-grep -il"
fi

# PW
alias pw="ps aux | grep -v grep | grep -e"

# Serve this
alias serve_this="python -m SimpleHTTPServer"
alias rserve_this="ruby -rrack -e \"Rack::Handler::WEBrick.run Rack::Directory.new('.')\""

# XVFB
alias run_xvfb="Xvfb :4 -screen 0 1280x1024x24"
alias xr="xvfb-run --server-args='-screen 0 1280x1024x24'"
alias xrake="xr bundle exec rake"
alias xrspec="xr bundle exec rspec"
alias xspec="xrake parallel:prepare parallel:spec"

# Emacs
alias enw="emacs -nw"
alias e="emacsclient -a '' -t"
alias email="emacsclient -s mail -t"
alias chat="emacsclient -s chat -t"

alias history='fc -fl 1'
