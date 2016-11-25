#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# isNumber()#{{{
function isNumber() {
    [ "${#}" -lt "1" ] && return 1

    case "${1}" in
        [0-9]*)
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}
#}}}
