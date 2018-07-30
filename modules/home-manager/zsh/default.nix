{ pkgs, ... }:

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

      # use 'fc -El 1' for "dd.mm.yyyy"
      # use 'fc -il 1' for "yyyy-mm-dd"
      # use 'fc -fl 1' for mm/dd/yyyy
      history = "fc -il 1";
    };

    history = {
      expireDuplicatesFirst = true;
      save = 100000;
      size = 100000;
    };

    initExtra = builtins.readFile (pkgs.substituteAll {
      src = ./init-extra.zsh;

      ag_bin       = "${pkgs.silver-searcher}/bin/ag";
      bat_bin      = "${pkgs.bat}/bin/bat";
      direnv_dir   = "${pkgs.direnv}";
      exa_bin      = "${pkgs.exa}/bin/exa";
      fzf_out      = "${pkgs.fzf}";
      git_bin      = "${pkgs.git}/bin/git";
      rbrowser_bin = "${pkgs.rbrowser}/bin/rbrowser";
      thefuck_out  = "${pkgs.thefuck}";
    });

    plugins = [
      {
        name = "zsh-kubernetes";
        src = pkgs.fetchFromGitHub {
          owner = "Dbz";
          repo = "zsh-kubernetes";
          rev = "8d7045d553244f84e1c65434057cc7c2602390cd";
          sha256 = "0z6i9wjjklb4lvr7zjhbphibsyx51psv50gm07mbb0kj9058j6kc";
        };
      }
    ];
  };
}
