#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Credit to @cowboy https://gist.github.com/cowboy/3118588

function sudo-session() {
  ####
  # Functions

  # If you want to define a specific help for this script
  function help()
  {
      print_info "${log_depth}" "USAGE: sudo-session [duration]"
      print_info "${log_depth}" "\t -h, --help                - this message"
      print_info "${log_depth}" ""
      print_info "${log_depth}" "The default duration is 15 minutes"
  }

  #
  ####

  ####
  # Main

  log_depth=0
  duration=15m

  while [[ $# -ge 1 ]]; do
      case "${1}" in
          -h|--help)
              help
              return 0
              ;;
          *)
              duration="${1}"
              shift
              ;;
      esac
  done

  # Might as well ask for password up-front, right?
  sudo -v

  # Keep-alive: update existing sudo time stamp if set, otherwise do nothing.
  (while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done &>/dev/null) &

  # then add a session killer to stop the sudo session. Basically running
  # `sudo -K` will kill the sudo session and will stop the above loop. But
  # for performance reasons, the sudo session will be available for a minute
  # more than requested, which is fine. I could sleep for `$(( $duration - 1
  # ))`. If accuracy is needed, neither solution is adequate so I chose to be
  # like, a minute whatever.
  (sleep "${duration}" && sudo -K &>/dev/null) &
}
