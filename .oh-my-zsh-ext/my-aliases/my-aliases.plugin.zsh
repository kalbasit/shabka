# Attach or Create
alias ac=tmux-attach-or-create

# ack
if [[ -x "`which ack-grep 2> /dev/null`" ]]; then
  alias ack=ack-grep
fi

# PW
alias pw="ps aux | grep -v grep | grep -e"

# Serve this
alias serve_this="python -m SimpleHTTPServer"
alias rserve_this="ruby -rrack -e \"Rack::Handler::WEBrick.run Rack::Directory.new('.')\""

# Google Specific
if [[ -x /usr/local/git/current/bin/git ]]; then
  alias Ggit=/usr/local/git/current/bin/git
fi

# XVFB
alias run_xvfb="Xvfb :4 -screen 0 1280x1024x24"
alias xr="xvfb-run --server-args='-screen 0 1280x1024x24'"
alias xrake="xr bundle exec rake"
alias xrspec="xr bundle exec rspec"
alias xspec="xrake parallel:prepare parallel:spec"

# Chef servers
alias officelist="knife node list -c ~/.chef/knife.office.rb"
alias staginglist="knife node list -c ~/.chef/knife.staging.rb"
alias productionlist="knife node list -c ~/.chef/knife.production.rb"

# scprsa()#{{{
function scprsa()
{
  if [[ -z "$1" ]]; then
    print_error 0 "!! You need to enter a hostname in order to send your public key !!"
  else
    print_info 0 "Copying SSH public key to server..."
    ssh ${1} "mkdir -p ~/.ssh && touch ~/.ssh/authorized_keys && cat - >> ~/.ssh/authorized_keys" < "${HOME}/.ssh/id_rsa.pub"
    print_info 0 "All done!"
  fi
}
#}}}

# x ()#{{{
function x () {
  local file cmd program
  if [ "${#}" -lt "1" ]; then
    print_error 0 "Usage: x compressed archive."
    return 1
  fi
  for file in "${@}"; do
    if [ -f "${file}" ] ; then
      case "${file}" in
        *.tar.bz2)   cmd="tar xjf"    ;;
        *.tar.gz)  cmd="tar xzf"    ;;
        *.bz2)     cmd="bunzip2 -f"   ;;
        *.rar|*.001) cmd="unrar x"    ;;
        *.gz)    cmd="gunzip -f"  ;;
        *.jar)     cmd="jar xf"     ;;
        *.tar)     cmd="tar xf"     ;;
        *.tbz2)    cmd="tar xjf"    ;;
        *.tgz)     cmd="tar xzf"    ;;
        *.zip|*.xpi) cmd="unzip"    ;;
        *.Z)     cmd="uncompress"   ;;
        *.7z)    cmd="7z x"     ;;
        *.ppe|pdb)
          # A little hacking here since the file
          # might be tar.gz or tar.bz, so we take care
          # of it
          if file "${1}" | grep -q "bzip2"; then
            # a bziped file, great
            cmd="tar xjf"
          elif file "${1}" | grep -q "gzip"; then
            # a Gziped file
            cmd="tar xzf"
          else
            print_error 0 "'${1}' is not a valid ppe/pdb archive."
            return 1
          fi
          ;;
        *)
          print_error 0 "'${1}' is not an archive type I am aware of."
          return 1
          ;;
      esac
      # Ok extract it now but first let's see if the progam can be used
      program="$(echo "${cmd}" | awk '{print $1}')"
      if ! type "${program}" &>/dev/null; then
        print_error 0 "I couldn't find the program '${program}', Please make sure it is installed."
        return 1
      fi
      ${cmd} "${file}"
      if [ "${?}" -ne "0" ]; then
        print_error 0 "Oops an error occured..."
        return 1
      else
        print_info 0 "Archive has been successfully extracted!"
      fi
    else
      print_error 0 "'${file}' is not a valid file."
      return 1
    fi
  done
}
#}}}
# c ()#{{{
function c () {
  local cmd program archive files answer
  if [ "${#}" -gt "1" ]; then
    archive="${1}"
    shift
    files="${@}"
    if [ -f "${archive}" ]; then
      print_info 1 "The destination file '${archive}' already exists, overwride [y/n] " false
      read answer; echo
      if isTrue "${answer}"; then
        rm -f -- "${archive}"
      else
        print_warning 0 "Aborting..."
        return 1
      fi
    fi

    case "${archive}" in
      *.tar.bz2)   cmd="tar cjf"    ;;
      *.tar.gz)  cmd="tar czf"    ;;
      *.bz2)     cmd="bzip2"
             archive="" # Bzip2 takes one Argument
             ;;
      *.rar)     cmd="rar c"    ;;
      *.gz)    cmd="gzip"
             archive="" # gzip takes one Argument
             ;;
      *.tar)     cmd="tar cf"     ;;
      *.jar)     cmd="jar cf"     ;;
      *.tbz2)    cmd="tar cjf"    ;;
      *.tgz)     cmd="tar czf"    ;;
      *.zip|*.xpi) cmd="zip -r"     ;;
      # TODO .Z and .7z formats
      *)
        print_error 0 "'${archive}' is not a valid archive type i am aware of."
        return 1
        ;;
    esac
    # Ok extract it now but first let's see if the progam can be used
    program="$(echo "${cmd}" | awk '{print $1}')"
    if ! type "${program}" &>/dev/null; then
      print_error 0 "I couldn't find the program '${program}', Please make sure it is installed."
      return 1
    fi
     ${cmd} ${archive} ${files}
    if [ "${?}" -ne "0" ]; then
      print_error 0 "Oops an error occured..."
      return 1
    else
      print_info 0 'Archive has been successfully Created!'
    fi
  else
    print_error 0 "USAGE: c <Archive name> <Files and/or folders>"
    return 1
  fi
}
#}}}
# spwgen()#{{{
function spwgen()
{
  if [[ "${1}" == "-h" ]]; then
    print_error 0 "Usage: ${0} <pwlen> <passwords>"
  else
    local pl="${1}"
    local np="${2}"
    test -z "${pl}" && pl="12"
    test -z "${np}" && np="10"
    perl <<EOPERL
my @a = ("a".."z","A".."Z","0".."9",(split //, q{#@,.<>$%&()*^}));
for (1.."$np") {
  print join "", map { \$a[rand @a] } (1.."$pl");
  print qq{\n}
}
EOPERL
  fi
}
#}}}
# sapg()#{{{
# generate passwords with apg
function sapg()
{
  if [[ -f $(which apg) ]]; then
    if [[ "${1}" == "-h" ]]; then
      print_error 0 "usage: ${0} <pwlen> <number of passwords>"
    else
      if [[ "${1}" -le "2" ]]; then
        print_error 0 "password too small!"
        return 1
      fi
      apg -x "${1}" -m "${1}" -n "${2}" -t -M NCL
    fi
  else
    print_error 0 "apg not installed... aborting."
    return 1
  fi
}
#}}}
# pcheck()#{{{
function pcheck()
{
  local PcheckLib
  if [[ "${#}" -lt "1" ]]; then
    print_error 0 "Usage: pcheck <password>"
    return 1
  fi
  PcheckLib="${HOME}/lib/PasswordCheck.jar"
  java -jar "${PcheckLib}" "${@}"
}
#}}}
# plocale()#{{{
# print current settings of LC_*
function plocale()
{
  print_info 0 "Current settings of LC_*"
  print_info 2 "LANG=${LANG}"
  print_info 2 "LC_ALL=${LC_ALL}"
  print_info 2 "LC_CTYPE=${LC_CTYPE}"
  print_info 2 "LC_NUMERIC=${LC_NUMERIC}"
  print_info 2 "LC_TIME=${LC_TIME}"
  print_info 2 "LC_COLLATE=${LC_COLLATE}"
  print_info 2 "LC_MONETARY=${LC_MONETARY}"
  print_info 2 "LC_MESSAGES=${LC_MESSAGES}"
  print_info 2 "LC_PAPER=${LC_PAPER}"
  print_info 2 "LC_NAME=${LC_NAME}"
  print_info 2 "LC_ADDRESS=${LC_ADDRESS}"
  print_info 2 "LC_TELEPHONE=${LC_TELEPHONE}"
  print_info 2 "LC_MEASUREMENT=${LC_MEASUREMENT}"
  print_info 2 "LC_IDENTIFICATION=${LC_IDENTIFICATION}"
}
#}}}
# cal()#{{{
# show date highlighted
function cal()
{
  if [[ -n "${1}" ]]; then
    /usr/bin/cal $*
  else
    var="$(/usr/bin/cal)"
    echo "${var/$(date +%-d)/${FG_RED_B}$(date +%-d)${FG_CLEAR}}"
  fi
}
#}}}

# clean_comments()#{{{
function clean_comments()
{
  grep -v "^[ \t]*#\|^$" "${1}"
}
#}}}

# store_env()#{{{
# store_env a function to update the envirement file.
function store_env()
{
  rm -f "${HOME}/.screen/env"
  touch "${HOME}/.screen/env"

  if [ -n "${SSH_AUTH_SOCK}" ]; then
    echo "export SSH_AUTH_SOCK=${SSH_AUTH_SOCK}" >> "${HOME}/.screen/env"
  else
    echo "unset SSH_AUTH_SOCK" >> "${HOME}/.screen/env"
  fi
  if [ -n "${SSH_AGENT_PID}" ]; then
    echo "export SSH_AGENT_PID=${SSH_AGENT_PID}" >> "${HOME}/.screen/env"
  else
    echo "unset SSH_AGENT_PID" >> "${HOME}/.screen/env"
  fi
  if [ -n "${DISPLAY}" ]; then
    echo "export DISPLAY=${DISPLAY}" >> "${HOME}/.screen/env"
  else
    echo "unset DISPLAY" >> "${HOME}/.screen/env"
  fi
  if [ -n "${WINDOWID}" ]; then
    echo "export WINDOWID=${WINDOWID}" >> "${HOME}/.screen/env"
  else
    echo "unset WINDOWID" >> "${HOME}/.screen/env"
  fi
  if [ -n "${SSH_TTY}" ]; then
    cat >> "${HOME}/.screen/env" <<EOT
export SSH_CLIENT="${SSH_CLIENT}"
export SSH_CONNECTION="${SSH_CONNECTION}"
export SSH_TTY=${SSH_TTY}
EOT
  else
    echo "unset SSH_CLIENT SSH_CONNECTION SSH_TTY" >> "${HOME}/.screen/env"
  fi
  if [ -n "${SESSION_MANAGER}" ]; then
    echo "export SESSION_MANAGER=${SESSION_MANAGER}" >> "${HOME}/.screen/env"
  else
    echo "unset SESSION_MANAGER" >> "${HOME}/.screen/env"
  fi
  if [ -n "${GPG_AGENT_INFO}" ]; then
    echo "export GPG_AGENT_INFO=${GPG_AGENT_INFO}" >> "${HOME}/.screen/env"
  else
    echo "unset GPG_AGENT_INFO" >> "${HOME}/.screen/env"
  fi
  if [ -n "${XDG_SESSION_COOKIE}" ]; then
    echo "export XDG_SESSION_COOKIE=${XDG_SESSION_COOKIE}" >> "${HOME}/.screen/env"
  else
    echo "unset XDG_SESSION_COOKIE" >> "${HOME}/.screen/env"
  fi
  if [ -n "${GNOME_KEYRING_SOCKET}" ]; then
    echo "export GNOME_KEYRING_SOCKET=${GNOME_KEYRING_SOCKET}" >> "${HOME}/.screen/env"
  else
    echo "unset GNOME_KEYRING_SOCKET" >> "${HOME}/.screen/env"
  fi
  if [ -n "${DBUS_SESSION_BUS_ADDRESS}" ]; then
    echo "export DBUS_SESSION_BUS_ADDRESS=${DBUS_SESSION_BUS_ADDRESS}" >> "${HOME}/.screen/env"
  else
    echo "unset DBUS_SESSION_BUS_ADDRESS" >> "${HOME}/.screen/env"
  fi
}
#}}}
# update_env()#{{{
function update_env()
{
  [ -f "${HOME}/.screen/env" ] && source "${HOME}/.screen/env"
}
#}}}
# least()#{{{
# Wrapper around PAGER.
# if lines paged fit on a screen they will be dumped to STDOUT, otherwise they
# are paged through your pager.
#
# From Bart Trojanowski
# http://www.jukie.net/~bart/scripts/least/bashrc.least
function least()
{
  declare -a lines

  if ! [ -z "$@" ] ; then
    cat $@ | least
    return 0
  fi

  if [ -z "$LINES" ] || ! ( echo $LINES | grep -q '^[0-9]\+$' ) ; then
    LINES=20
  fi

  # dump_array()#{{{
  function dump_array () {
    for n in `fseq 1 "${#lines[@]}"` ; do
      echo "${lines[$n]}"
    done
  }
  #}}}
  while read x ; do
    lines[((${#lines[@]}+1))]="$x"

    if [ "${#lines[@]}" -ge $LINES ] ; then
      ( dump_array ; cat ) | $LEAST_PAGER
      return 0
    fi
  done

  dump_array
}
#}}}
