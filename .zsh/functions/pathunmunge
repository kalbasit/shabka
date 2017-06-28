#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#

function pathunmunge() {
  oldpath=("${(@s/:/)PATH}")
  newpath=""
  sep=""
  for p in ${oldpath[*]}; do
    if [[ "x${p}" != "x${1}" ]]; then
      newpath="${newpath}${sep}${p}"
      sep=":"
    fi
  done
  export PATH="${newpath}"
}
