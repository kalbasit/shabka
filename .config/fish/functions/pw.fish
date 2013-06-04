function pw
  ps aux | grep -v grep | grep -e $argv
end
