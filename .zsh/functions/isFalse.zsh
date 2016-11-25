#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# isFalse()#{{{
function isFalse() {
    case "${1}" in
        [Ff][Aa][Ll][Ss][Ee])
            return 0
        ;;
        [Ff])
            return 0
        ;;
        [Nn][Oo])
            return 0
        ;;
        [Nn])
            return 0
        ;;
        0)
            return 0
        ;;
    esac
    return 1
}
#}}}
