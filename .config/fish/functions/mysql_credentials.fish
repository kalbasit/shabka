function mysql_credentials
  set -l mysql_credentials_path "$HOME/.mysql_password"

  if not test -e "$mysql_credentials_path"
    return
  end

  set -l user (cat "$mysql_credentials_path" | grep 'username:' | cut -d' ' -f2)
  set -l pass (cat "$mysql_credentials_path" | grep 'password:' | cut -d' ' -f2)

  if test -n "$user"; and test -n "$pass"
    set -xg MYSQL_USERNAME $user
    set -xg MYSQL_PASSWORD $pass
  end
end
