function ack
  set -l ack_grep_path (which ack-grep 2> /dev/null)

  if test -x (echo $ack_grep_path)
    command ack-grep $argv[1..-1]
  else
    command ack $argv[1..-1]
  end
end
