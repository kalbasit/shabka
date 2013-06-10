function tnp
  if test -z $TMUX
    echo 'Must be running inside a tmux session'
    return 1
  end

  if [ (count $argv) -ne 1 ]
    echo "Usage: $_ <project_name>"
    return 1
  end

  set -l tmux_last_window (tmux list-windows | cut -d: -f1 | tail -n 1)
  set -l tmux_server_number
  set -l project_name $argv[1]

  for name in Main Vim CI
    tmux new-window -n "[$project_name] $name"
  end

  if [ $tmux_last_window -ge 20 ]
    set tmux_server_number (math "$tmux_last_window + 1")
  else
    set tmux_server_number 20
  end

  tmux new-window -n "[$project_name] server" -t $tmux_server_number
end
