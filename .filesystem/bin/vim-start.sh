#!/bin/bash
#
# auth: bioe007
#
# start vim server master. pass any other files to this instance of vim if
# already running
#
#
cmd="$(which vim)"

options=""
files=""

TABNEW=""

# store cli options
if [ "$#" -gt "0" ] ; then
    if [ ! -z "$(echo $@ | grep "\-\-")" ] ; then
        while [ ! -z "$( echo "$1" | grep "^\-\-" )" ] ; do
            case "$1" in
                --tn )
                TABNEW="1"
                ;;
            * )
                options="$options $1"
                ;;
            esac
            shift
        done
        cmd="$cmd $options"
    fi
fi

# assume anything left is a file, only add readable files to the list
while [ "$#" -ge "1" ] ; do
    if [ -r "$1" ] ; then
        files="$files $1"
    fi
    shift
done

srv="$($cmd --serverlist | head -1)"

# no existing vim server found use default master
if [ -z "$srv" ] ; then
    srv="master"

    # open with files from cli or not
    # also ignore any cli options so as not to confuse things
    if [ -z "$files" ] ; then
        cmd="$cmd --servername $srv"
    else
        cmd="$cmd --servername $srv \"$files\""
    fi

else

    # if the server is running and no files are being opened or options/commands
    # passed, then just exit
    if [ -z "$files" ] && [ -z "$options" ] ; then
        echo "$(basename $0):Server running, no file arguments, no cli options. vim-start exiting"
        exit 0
    fi

    # otherwise concatenate the relevant vars to do something
    if [ ! -z "$files" ] && [ -z "$options" ] ; then

        # quikcly split the current buffer before adding the files
        if [ -z "$TABNEW" ]; then
            $cmd --servername $srv --remote-send ':sp<CR>'
        else
            $cmd --servername $srv --remote-send ':tabnew<CR>'
        fi

        cmd="$cmd -c :sp --servername $srv $options --remote-silent  $files"

    elif [ -z "$files" ] && [ ! -z "$options" ] ; then
        cmd="$cmd --servername $srv $options"
    fi

    echo 'tagPop("vim")' | awesome-client > /dev/null 2>&1
    exec $cmd
    exit $?
fi

# opens vim in urxvt
# urxvt -bg '#2e3436' +tr -name vim -e sh -c "$cmd" &
urxvt +tr -name vim -e zsh -c "$cmd" &


# vim:set ft=sh tw=80 fdm=indent ts=4 sw=4 et sta ai si:
