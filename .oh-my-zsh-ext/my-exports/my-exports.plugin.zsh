export VMAIL_HTML_PART_READER='elinks -dump'
export GOPATH=$HOME/code/go
export EDITOR=vim
export MYFS=$HOME/.filesystem

if [[ -x /usr/bin/google-chrome-beta ]]; then
  export BROWSER=/usr/bin/google-chrome-beta
else
  export BROWSER=/usr/bin/google-chrome
fi

if [[ -x `which cask 2> /dev/null` ]]; then
  export CASK_PATH="$(dirname $(dirname $(which cask)))"
fi
