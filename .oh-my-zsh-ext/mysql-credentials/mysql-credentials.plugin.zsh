local mysql_credentials_path="$HOME/.mysql_password"

if [[ -r "${mysql_credentials_path}" ]]; then
  local user="`cat "$mysql_credentials_path" | grep 'username:' | cut -d' ' -f2`"
  local pass="`cat "$mysql_credentials_path" | grep 'password:' | cut -d' ' -f2`"

  if [ "x${user}" != "x" -a "x${pass}" != "x" ]; then
    export MYSQL_USERNAME="${user}"
    export MYSQL_PASSWORD="${pass}"
  fi
fi
