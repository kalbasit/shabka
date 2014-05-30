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

local mysql_credentials_path="$HOME/.my.cnf"

if [[ -r "${mysql_credentials_path}" ]]; then
  local user="`cat "$mysql_credentials_path" | grep "user" | cut -d= -f2 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'`"
  local pass="`cat "$mysql_credentials_path" | grep "password" | cut -d= -f2 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'`"

  if [ "x${user}" != "x" ]; then
    export MYSQL_USERNAME="${user}"
    export MYSQL_PASSWORD="${pass}"
  fi
fi

# Color grep results
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'
