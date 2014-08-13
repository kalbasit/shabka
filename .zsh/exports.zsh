export BROWSER=/usr/bin/google-chrome-stable

local mysql_credentials_path="$HOME/.my.cnf"

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
