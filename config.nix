{
  allowUnfree = true;

  firefox = {
    enableGoogleTalkPlugin = true;
    enableAdobeFlash = true;
  };

  chromium = {
    enablePepperFlash = true;
  };

  packageOverrides = pkgs_: with pkgs_; {
    nvim-config = import ./nvim-config { inherit pkgs ; };

    git-config = import ./git-config {
      inherit (pkgs) stdenv nvim-config;
    };

    less-config = import ./less-config {
      inherit (pkgs) stdenv less;
    };

    most-config = import ./most-config {
      inherit (pkgs) stdenv;
    };

    rofi-config = import ./rofi-config {
      inherit (pkgs) stdenv rofi;
    };

    surfingkeys-config = import ./surfingkeys-config {
      inherit (pkgs) stdenv;
    };

    sway-config = import ./sway-config {
      inherit (pkgs) stdenv brightnessctl pulseaudio i3lock rofi termite libnotify slack zsh-config nvim-config i3status;
    };

    swm = import ./swm {
      inherit (pkgs) stdenv buildGoPackage fetchgit;
    };

    termite-config = import ./termite-config {
      inherit (pkgs) stdenv;
    };

    tmux-config = import ./tmux-config {
      inherit (pkgs) stdenv tmux xsel gist libnotify;
    };

    zsh-config = import ./zsh-config {
      inherit (pkgs) stdenv direnv;
    };

    all = with pkgs; buildEnv {
      name = "all";

      paths = [
        chromium

        direnv

        firefox

        fzf

        git
        git-crypt
        git-config

        go
        dep
        swm

        jq

        less-config

        mosh

        most
        most-config

        nvim-config

        nix-index

        nodejs-8_x

        powerline-fonts

        python27
        python36

        rake

        rofi-config

        surfingkeys-config

        sway-config

        termite
        termite-config

        tmux
        tmux-config

        zsh
        zsh-config
        nix-zsh-completions
      ];
    };
  };
}
