if [[ -e "/var/run/mysqld/mysqld.sock" ]]; then
  export WF_DBSOCKET="/var/run/mysqld/mysqld.sock"
fi
export WF_DBUSERNAME="${MYSQL_USERNAME}"
export WF_DBPASSWORD="${MYSQL_PASSWORD}"
