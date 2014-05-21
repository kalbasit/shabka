export VMAIL_HTML_PART_READER='elinks -dump'
export GOPATH=$HOME/code/go
export EDITOR=vim
export MY_FS=$HOME/.filesystem
export BROWSER=/usr/bin/google-chrome-beta

if [[ -x `which cask 2> /dev/null` ]]; then
  export CASK_PATH="$(dirname $(dirname $(which cask)))"
fi
