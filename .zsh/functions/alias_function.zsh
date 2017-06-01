#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Credit: http://mivok.net/2009/09/20/bashfunctionoverrist.html

# Usage: alias_function new_name old_name
alias_function() {
  if [[ "${1}" = "-h" ]] || [[ "${1}" = "--help" ]]; then
    echo "Usage: alias_function new_name old_name"
    return 0
  fi
  local ORIG_FUNC=$(declare -f $2)
  local NEWNAME_FUNC="$1${ORIG_FUNC#$2}"
  eval "$NEWNAME_FUNC"
}
