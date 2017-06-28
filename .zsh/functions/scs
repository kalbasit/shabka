#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# scs()#{{{
function scs() {
  if [[ "${#}" -ne 1 ]]; then
    print_error 0 "Usage: scs <session>"
    return 1
  fi

  session="${1}"

  # U=utf8, R=reattach, q=quiet, x=multiplex
  screen_cmd="screen -x -q -U -R ${session} -t ${session}"

  if [[ -f "${HOME}/.screen/sessions/${session}" ]]; then
    screen_cmd="${screen_cmd} -c '${HOME}/.screen/sessions/${session}'"
  fi

  eval "${screen_cmd}"
}
#}}}
