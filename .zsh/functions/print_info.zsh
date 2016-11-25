#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# print_info()#{{{
# print_info(printlevel, print [, newline [, prefixline ] ])
function print_info() {
    local NEWLINE='1'
    local PREFIXLINE='1'
    local STR=''
    local PREFIXTEXT=''

    # NOT ENOUGH ARGS
    if [ "${#}" -lt '2' ] ; then return 1; fi

    # WRONG printlevel
    if [ "${1}" -lt "0" ]; then
        print_error 1 "printlevel must be above or equal 0"
        return 1
    fi

    # If printlevel is 0, the text must be bolded
    if [ "${1}" -eq "0" ]; then
        PREFIXTEXT="${FG_WHITE_B}"
    fi

    # IF 3 OR MORE ARGS, CHECK IF WE WANT A NEWLINE AFTER PRINT
    if [ "${#}" -gt '2' ]
    then
        if isTrue "${3}"
        then
            NEWLINE='1';
        else
            NEWLINE='0';
        fi
    fi

    # IF 4 OR MORE ARGS, CHECK IF WE WANT TO PREFIX WITH A *
    if [ "${#}" -gt '3' ]
    then
        if isTrue "${4}"
        then
            PREFIXLINE='1'
        else
            PREFIXLINE='0'
        fi
    fi

    # STRUCTURE printlevel
    if [ "${1}" -gt "1" ]; then
        PRINTLEVEL="$(for i in $(seq 1 ${1}); do echo -ne "  "; done)"
    else
        PRINTLEVEL=" "
    fi

    # STRUCTURE DATA TO BE OUTPUT TO SCREEN, AND OUTPUT IT
    if [ "${PREFIXLINE}" = '1' ]
    then
        STR="${GOOD}*${FG_CLEAR}${PRINTLEVEL}${PREFIXTEXT}${2}${FG_CLEAR}"
    else
        STR="${PREFIXTEXT}${2}${FG_CLEAR}"
    fi

    if [ "${NEWLINE}" = '0' ]
    then
        echo -ne "${STR}"
    else
        echo -e "${STR}"
    fi

    return 0
}
#}}}
