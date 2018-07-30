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

    oh-my-zsh = {
      plugins = [
        "command-not-found"
        "extract"
        "git"
        "history"
        "kuberenetes"
        "sudo"
      ];
    };

    plugins = [
      {
        name = "enhancd";
        file = "init.sh";
        src = pkgs.fetchFromGitHub {
          owner = "b4b4r07";
          repo = "enhancd";
          rev = "fd805158ea19d640f8e7713230532bc95d379ddc";
          sha256 = "0pc19dkp5qah2iv92pzrgfygq83vjq1i26ny97p8dw6hfgpyg04l";
        };
      }

      {
        name = "gitit";
        src = pkgs.fetchFromGitHub {
          owner = "peterhurford";
          repo = "git-it-on.zsh";
          rev = "4827030e1ead6124e3e7c575c0dd375a9c6081a2";
          sha256 = "01xsqhygbxmv38vwfzvs7b16iq130d2r917a5dnx8l4aijx282j2";
        };
      }

      {
        name = "solarized-man";
        src = pkgs.fetchFromGitHub {
          owner = "zlsun";
          repo = "solarized-man";
          rev = "a902b64696271efee95f37d45589078fdfbbddc5";
          sha256 = "04gm4qm17s49s6h9klbifgilxv8i45sz3rg521dwm599gl3fgmnv";
        };
      }

      # TODO: try this out and enable/remove it
      #{
      #  name = "zsh-autosuggestions";
      #  src = pkgs.fetchFromGitHub {
      #    owner = "zsh-users";
      #    repo = "zsh-autosuggestions";
      #    rev = "v0.4.0";
      #    sha256 = "0z6i9wjjklb4lvr7zjhbphibsyx51psv50gm07mbb0kj9058j6kc";
      #  };
      #}

      {
        name = "zsh-completions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-completions";
          rev = "0.27.0";
          sha256 = "1c2xx9bkkvyy0c6aq9vv3fjw7snlm0m5bjygfk5391qgjpvchd29";
        };
      }

      {
        name = "zsh-history-substring-search";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-history-substring-search";
          rev = "v1.0.1";
          sha256 = "0lgmq1xcccnz5cf7vl0r0qj351hwclx9p80cl0qczxry4r2g5qaz";
        };
      }

      {
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "db6cac391bee957c20ff3175b2f03c4817253e60";
          sha256 = "0d9nf3aljqmpz2kjarsrb5nv4rjy8jnrkqdlalwm2299jklbsnmw";
        };
      }
    ];
  };
}
