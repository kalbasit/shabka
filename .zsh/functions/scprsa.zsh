#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

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
