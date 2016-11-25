#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# zssh()#{{{
function zssh() {
    local user_host_port=${1}
    local SSH=/usr/bin/ssh
    local REMOTE_SSH_USER=""
    local DESTZDOTDIR=""
    local controlmaster_running=""

    # fail-fast if no user/host/port were given
    if [[ -z "${user_host_port}" ]]; then
        print_error 0 "Usage zssh <[user@]host[:port]>"
        return 1
    fi

    # let's get the user ssh'ing into the host
    REMOTE_SSH_USER="$(echo $user_host_port | grep @ | cut -d@ -f1)"
    REMOTE_SSH_USER="${REMOTE_SSH_USER:-$USER}"

    # The private home folder
    DESTZDOTDIR="/tmp/${USER}.zdotdir.${REMOTE_SSH_USER}"

    if [[ "$($SSH -O check ${user_host_port} &>/dev/null; echo $?)" -eq "0" ]]; then
        controlmaster_running="true"
    else
        controlmaster_running="false"
    fi

    if isFalse ${controlmaster_running}; then
        # start the controlmaster
        $SSH ${user_host_port} /bin/true
        # Transfer what we need to the server
        rm -rf $DESTZDOTDIR && mkdir -p $DESTZDOTDIR
        cp -RL ${HOME}/.{zsh,zshrc,zshrc.google,tmux.conf} $DESTZDOTDIR/
        infocmp > "$DESTZDOTDIR/${TERM}.info"
        rsync -auz --delete $DESTZDOTDIR/ ${user_host_port}:$DESTZDOTDIR/
        $SSH ${user_host_port} "chmod 700 $DESTZDOTDIR; tic $DESTZDOTDIR/${TERM}.info"
    fi

    $SSH -tt ${user_host_port} ZDOTDIR=${DESTZDOTDIR} TMUXDOTDIR=${DESTZDOTDIR} zsh -i
}
#}}}
