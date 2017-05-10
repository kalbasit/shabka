#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#

function pathmunge() {
  [[ ! -d "${1}" ]] && return
  if ! [[ $PATH =~ (^|:)$1($|:) ]]; then
     if [ "$2" = "after" ] ; then
        PATH=$PATH:$1
     else
        PATH=$1:$PATH
     fi
  fi
}
