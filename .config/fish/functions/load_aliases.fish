function load_aliases

  # Attach or Create
  function ac; tmux-attach-or-create $argv; end

  # ack
  function ack
    set -l ack_grep_path (which ack-grep ^/dev/null)

    if test -x (echo $ack_grep_path)
      command ack-grep $argv
    else
      command ack $argv
    end
  end

  # g for Git
  function g; git $argv; end

  # p for project
  function p; cd ~/code/*/$argv; end

  # PW
  function pw; ps aux | grep -v grep | grep -e $argv; end

  # Serve this
  function serve_this; python -m SimpleHTTPServer; end

  # run_xvfb
  function run_xvfb; command Xvfb :4 -screen 0 1280x1024x24; end

  # Chef Servers
  function officelist; knife node list -c ~/.chef/knife.office.rb; end
  function staginglist; knife node list -c ~/.chef/knife.staging.rb; end
  function productionlist; knife node list -c ~/.chef/knife.production.rb; end
end
