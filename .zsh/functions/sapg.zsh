#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# generate passwords with apg
function sapg()
{
    if have apg; then
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
