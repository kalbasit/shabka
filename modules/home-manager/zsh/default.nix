{ ... }:

{
  programs.zsh = {
    enable = true;

    shellAliases = {
      e = "\${EDITOR:-nvim}";
      gl = "github_commit_link";
      hm = "home-manager";
      http = "http --print=HhBb";
      ll = "ls -la";
      pw = "ps aux | grep -v grep | grep -e";
      rot13 = "tr '[A-Za-z]' '[N-ZA-Mn-za-m]'";
      rserve_this = "ruby -rrack -e \"Rack::Handler::WEBrick.run Rack::Directory.new('.')\"";
      serve_this = "python2 -m SimpleHTTPServer";
      t = "task";
      utf8test = "curl -L https://github.com/tmux/tmux/raw/master/tools/UTF-8-demo.txt";
      vim = "nvim";
      vi = "nvim";

      # TODO: move this to the swm package
      s = "swm tmux switch-client";
      sb = "swm --story base tmux switch-client";
      vim_ready = ""; # TODO: run direnv here

      # TODO: move to docker-config, how to tell ZSH to import them?
      remove_created_containers = "docker rm -v \$(docker ps -a -q -f status=created)";
      remove_dangling_images = "docker rmi \$(docker images -f dangling=true -q)";
      remove_dead_containers = "docker rm -v \$(docker ps -a -q -f status=exited)";

      # Always enable colored `grep` output
      # Note: `GREP_OPTIONS = "--color = auto"` is deprecated, hence the alias usage.
      egrep = "egrep --color=auto";
      fgrep = "fgrep --color=auto";
      grep = "grep --color=auto";

      # send_code sends the code to apollo
      send_code = "rsync -avuz --rsync-path=/usr/bin/rsync --delete --exclude=.snapshots/ --exclude=pkg/ --exclude=bin/ \"$CODE_PATH/\" apollo:/volume1/Code/active/";
      # get_code gets code from apollo
      get_code = "rsync -avuz --rsync-path=/usr/bin/rsync --delete --exclude=.snapshots/ --exclude=pkg/ --exclude=bin/ apollo:/volume1/Code/active/ \"$CODE_PATH/\"";

      # OS-Specific aliases
      # TODO: install this only on Mac
      #if [[ "$OSTYPE" = darwin* ]]; then  # Mac only
      #	alias mac_install_cert='sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain'
      #fi
    };

    initExtra = builtins.readFile (pkgs.substituteAll {
      src = ./init-extra.zsh;

      gocode_bin = "${pkgs.gocode}/bin/gocode";
      neovim_node_host_bin = "${pkgs.nodePackages.neovim}/bin/neovim-node-host";
      typescript_server_bin = "${pkgs.nodePackages.typescript}/bin/tsserver";
      xsel_bin = "${pkgs.xsel}/bin/xsel";
    });

    plugins = [
      {

      }
    ];
  };
}
