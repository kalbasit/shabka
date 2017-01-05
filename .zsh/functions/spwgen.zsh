#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

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
