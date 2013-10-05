function set_my_paths
  # Add my bin folder
  if test -d "$HOME/.filesystem/bin"
    add_path "$HOME/.filesystem/bin"
  end

  # Add my usr/bin folder
  if test -d "$HOME/.filesystem/usr/bin"
    add_path "$HOME/.filesystem/usr/bin"
  end

  # Add any folder in my opt folders
  if test -d "$HOME/.filesystem/opt/"
    set -l opt_paths (find "$HOME/.filesystem/opt/" -maxdepth 2 -name bin -type d)

    if [ (count $opt_paths) -gt 0 ]
      for dir in $opt_paths
        add_path "$dir"
      end
    end
  end

  # Add npm's OSX folder
  if test -d /usr/local/share/npm/bin
    add_path /usr/local/share/npm/bin
  end

  # Add go path
  if test -d "$HOME/code/go/bin"
    add_path "$HOME/code/go/bin"
  end

  # Add homebrew path (for corporate)
  if test -d "/brew/bin"
    add_path "/brew/bin"
  end
end
