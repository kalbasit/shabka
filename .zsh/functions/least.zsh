#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

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
        for n in `seq 1 "${#lines[@]}"` ; do
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
