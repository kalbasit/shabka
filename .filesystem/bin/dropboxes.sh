dropboxes=".dropbox-personal .dropbox-work"
for dropbox in $dropboxes
do
    HOME="/home/$USER"
    if ! [ -d "$HOME/$dropbox" ]
    then
        mkdir "$HOME/$dropbox" 2> /dev/null
        ln -s "$HOME/.Xauthority" "$HOME/$dropbox/" 2> /dev/null
    fi
    HOME="$HOME/$dropbox"
    /home/$USER/.dropbox-dist/dropboxd 2> /dev/null &
done
