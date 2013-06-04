function wf_mysql_credentials
  # Load the credentials
  mysql_credentials

  # Set them for wildfire
  if test -n "$MYSQL_USERNAME"
    set -xg WF_DBUSERNAME $MYSQL_USERNAME
    set -xg WF_DBPASSWORD $MYSQL_PASSWORD
  end
end
