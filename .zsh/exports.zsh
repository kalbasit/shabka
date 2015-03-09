export BROWSER=/usr/bin/google-chrome-unstable

local mysql_credentials_path="$HOME/.my.cnf"
local github_token_path="$HOME/.github_token"

if [[ -r "${github_token_path}" ]]; then
  export HOMEBREW_GITHUB_API_TOKEN=`head -1 ${github_token_path}`
fi

if [[ -r "${mysql_credentials_path}" ]]; then
  local user="`cat "$mysql_credentials_path" | grep "user" | cut -d= -f2 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'`"
  local pass="`cat "$mysql_credentials_path" | grep "password" | cut -d= -f2 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'`"

  if [ "x${user}" != "x" ]; then
    export MYSQL_USERNAME="${user}"
    export MYSQL_PASSWORD="${pass}"
  fi
fi

if [[ -f "${MYFS}/opt/stderred/build/libstderred.so" ]]; then
  export LD_PRELOAD="${MYFS}/opt/stderred/build/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
fi
