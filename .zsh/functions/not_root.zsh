#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# not_root()#{{{
function not_root()
{
    if check_root; then
        print_error 0 "For security reasons, you should not run this script as root!"
        exit 1
    fi
}
#}}}
