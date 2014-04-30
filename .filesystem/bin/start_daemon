#!/bin/sh

set -efu

ionice_class=
ionice_priority=
nice=

while getopts c:p:n: f; do
  case $f in
  c) ionice_class=$OPTARG;;
  p) ionice_priority=$OPTARG;;
  n) nice=$OPTARG;;
  *) exit 2;;
  esac
done
shift $((OPTIND - 1))

cmd=$*
io=

if pgrep -u "$(id -u)" -xf -- "$cmd" >/dev/null 2>&1; then
  exit 0
fi

if type ionice >/dev/null 2>&1; then
  [ -n "$ionice_class" ]    && { io=1; cmd="-c $ionice_class $cmd"; }
  [ -n "$ionice_priority" ] && { io=1; cmd="-n $ionice_priority $cmd"; }
  [ -n "$io" ] && cmd="ionice $cmd"
fi

if type nice >/dev/null 2>&1; then
  [ -n "$nice" ] && cmd="nice -n $nice $cmd"
fi

exec $cmd
