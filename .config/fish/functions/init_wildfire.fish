function init_wildfire
  # Export mysql credentials
  wf_mysql_credentials

  if test -e /var/run/mysqld/mysqld.sock
    set -xg WF_DBSOCKET /var/run/mysqld/mysqld.sock
  end
end
