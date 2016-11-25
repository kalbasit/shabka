#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

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
