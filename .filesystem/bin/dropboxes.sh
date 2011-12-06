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

    if [ -x "/home/$USER/.dropbox-dist/dropboxd" ]; then
      /home/$USER/.dropbox-dist/dropboxd 2> /dev/null &
    elif [ -x `which dropboxd 2>/dev/null` ]; then
      dropboxd 2> /dev/null &
    else
      echo "Could not find dropboxd"
      exit 1
    fi
done
