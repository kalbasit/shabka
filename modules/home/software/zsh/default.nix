{ config, pkgs, lib, ... }:

let
  myFunctions = pkgs.stdenvNoCC.mkDerivation rec {
    name = "zsh-functions-${version}";
    version = "0.0.1";
    src = ./plugins/functions;
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir $out

      cp $src/* $out/

      rm -f $out/default.nix

      substituteInPlace $out/get_pr \
        --subst-var-by curl_bin ${lib.getBin pkgs.curl}/bin/curl \
        --subst-var-by git_bin ${lib.getBin pkgs.git}/bin/git \
        --subst-var-by jq_bin ${lib.getBin pkgs.jq}/bin/jq \
        --subst-var-by xsel_bin ${lib.getBin pkgs.xsel}/bin/xsel

      substituteInPlace $out/git_gopath_formatted_repo_path \
        --subst-var-by git_bin ${lib.getBin pkgs.git}/bin/git \
        --subst-var-by perl_bin ${lib.getBin pkgs.perl}/bin/perl

      substituteInPlace $out/jsonpp \
        --subst-var-by python_bin ${lib.getBin pkgs.python37Full}/bin/python \
        --subst-var-by pygmentize_bin ${lib.getBin pkgs.python36Packages.pygments}/bin/pygmentize

      substituteInPlace $out/jspp \
        --subst-var-by js-beautify_bin ${lib.getBin pkgs.python36Packages.jsbeautifier}/bin/js-beautify

      substituteInPlace $out/new_pr \
        --subst-var-by curl_bin ${lib.getBin pkgs.curl}/bin/curl \
        --subst-var-by git_bin ${lib.getBin pkgs.git}/bin/git \
        --subst-var-by jq_bin ${lib.getBin pkgs.jq}/bin/jq \
        --subst-var-by xsel_bin ${lib.getBin pkgs.xsel}/bin/xsel

      substituteInPlace $out/tmycli \
        --subst-var-by mycli_bin ${lib.getBin pkgs.mycli}/bin/mycli \
        --subst-var-by netstat_bin ${lib.getBin pkgs.nettools}/bin/netstat \
        --subst-var-by ssh_bin ${lib.getBin pkgs.openssh}/bin/ssh

      substituteInPlace $out/pr \
        --subst-var-by git_bin ${lib.getBin pkgs.git}/bin/git

      substituteInPlace $out/vim_clean_swap \
        --subst-var-by vim_bin ${lib.getBin pkgs.vim}/bin/vim

      substituteInPlace $out/xmlpp \
        --subst-var-by xmllint_bin ${lib.getBin pkgs.libxml2Python}/bin/xmllint
    '';
  };

in {

  home.packages = with pkgs; [
    # packages needed by the extract plugin
    # TODO: move this to the extract plugin instead!
    bzip2
    gnutar
    gzip
    p7zip
  ];


  programs.zsh = {
    enable = true;

    enableCompletion = true;
    enableAutosuggestions = true;

    shellAliases = {
      cat = "${pkgs.bat}/bin/bat";
      e = "\${EDITOR:-nvim}";
      gl = "github_commit_link";
      http = "http --print=HhBb";
      kube = "kubectl";
      less = "${pkgs.bat}/bin/bat";
      ll = "ls -la";
      pw = "ps aux | grep -v grep | grep -e";
      rot13 = "tr \"[A-Za-z]\" \"[N-ZA-Mn-za-m]\"";
      serve_this = "${pkgs.python3}/bin/python -m http.server";
      utf8test = "${pkgs.curl}/bin/curl -L https://github.com/tmux/tmux/raw/master/tools/UTF-8-demo.txt";
      vi = "nvim";
      vim = "nvim";

      # for enabling and disabling the current theme. This means go back to a very basic theme
      zsh_theme_enable = "prompt_powerlevel9k_teardown";
      zsh_theme_disable = "prompt_powerlevel9k_setup";

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
      send_code = "${pkgs.rsync}/bin/rsync -avuz --rsync-path=/usr/bin/rsync --delete --exclude=.snapshots/ --exclude=pkg/ --exclude=bin/ \"$CODE_PATH/\" apollo:/volume1/Code/active/";
      # get_code gets code from apollo
      get_code = "${pkgs.rsync}/bin/rsync -avuz --rsync-path=/usr/bin/rsync --delete --exclude=.snapshots/ --exclude=pkg/ --exclude=bin/ apollo:/volume1/Code/active/ \"$CODE_PATH/\"";

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
      save = 1000000;
      size = 1000000;
    };

    initExtra = (builtins.readFile (pkgs.substituteAll {
      src = ./init-extra.zsh;

      exa_bin      = "${pkgs.exa}/bin/exa";
      fortune_bin  = "${pkgs.fortune}/bin/fortune";
      fzf_out      = "${pkgs.fzf}";
      home_path    = "${config.home.homeDirectory}";
      jq_bin       = "${pkgs.jq}/bin/jq";
    })) + (if pkgs.stdenv.isDarwin then ''
      # source the nix profiles
      if [[ -r "${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh" ]]; then
        source "${config.home.homeDirectory}/.nix-profile/etc/profile.d/nix.sh"
      fi
    '' else "");

    oh-my-zsh = {
      enable = true;

      plugins = [
        "command-not-found"
        "extract"
        "git"
        "history"
        "kubectl"
        "sudo"
      ];
    };

    plugins = with pkgs; [
      {
        name = "enhancd";
        file = "init.sh";
        src = fetchFromGitHub {
          owner = "b4b4r07";
          repo = "enhancd";
          rev = "fd805158ea19d640f8e7713230532bc95d379ddc";
          sha256 = "0pc19dkp5qah2iv92pzrgfygq83vjq1i26ny97p8dw6hfgpyg04l";
        };
      }

      {
        name = "gitit";
        src = fetchFromGitHub {
          owner = "peterhurford";
          repo = "git-it-on.zsh";
          rev = "4827030e1ead6124e3e7c575c0dd375a9c6081a2";
          sha256 = "01xsqhygbxmv38vwfzvs7b16iq130d2r917a5dnx8l4aijx282j2";
        };
      }

      {
        name = "solarized-man";
        src = fetchFromGitHub {
          owner = "zlsun";
          repo = "solarized-man";
          rev = "a902b64696271efee95f37d45589078fdfbbddc5";
          sha256 = "04gm4qm17s49s6h9klbifgilxv8i45sz3rg521dwm599gl3fgmnv";
        };
      }

      {
        name = "powerlevel9k";
        file = "powerlevel9k.zsh-theme";
        src = fetchFromGitHub {
          owner = "bhilburn";
          repo = "powerlevel9k";
          rev = "571a859413866897cf962396f02f65a288f677ac";
          sha256 = "0xwa1v3c4p3cbr9bm7cnsjqvddvmicy9p16jp0jnjdivr6y9s8ax";
        };
      }

      {
        name = "zsh-completions";
        src = fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-completions";
          rev = "0.27.0";
          sha256 = "1c2xx9bkkvyy0c6aq9vv3fjw7snlm0m5bjygfk5391qgjpvchd29";
        };
      }

      {
        name = "zsh-history-substring-search";
        src = fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-history-substring-search";
          rev = "47a7d416c652a109f6e8856081abc042b50125f4";
          sha256 = "1mvilqivq0qlsvx2rqn6xkxyf9yf4wj8r85qrxizkf0biyzyy4hl";
        };
      }

      {
        name = "zsh-syntax-highlighting";
        src = fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "db6cac391bee957c20ff3175b2f03c4817253e60";
          sha256 = "0d9nf3aljqmpz2kjarsrb5nv4rjy8jnrkqdlalwm2299jklbsnmw";
        };
      }

      {
        name = "nix-shell";
        src = fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "03a1487655c96a17c00e8c81efdd8555829715f8";
          sha256 = "1avnmkjh0zh6wmm87njprna1zy4fb7cpzcp8q7y03nw3aq22q4ms";
        };
      }

      {
        name = "functions";
        src = myFunctions;
      }
    ];
  };
}
