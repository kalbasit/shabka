{ config, pkgs, lib, ... }:

with lib;

let
  shabka = import <shabka> { };
  cfg = config.mine.git;
in {
  options = {

    mine.git = {
      enable = mkEnableOption "git";

      userName = mkOption {
        type = types.str;
        default = "Marc 'risson' Schmitt";
        description = "git user name";
      };

      userEmail = mkOption {
        type = types.str;
        default = "marc.schmitt@risson.space";
        description = "git user email";
      };

      gpgSigningKey = mkOption {
        type = types.str;
        default = "marc.schmitt@risson.space";
        description = "git PGP signing key";
      };
    };
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;

      userName = cfg.userName;

      userEmail = cfg.userEmail;

      aliases = {
        amend = "commit --amend";
        b         = "branch";
        ci        = "commit";
        co        = "checkout";
        cob       = "checkout -b";
        com       = "checkout master";
        coke      = "commit -a -m";
        cokewogpg = "commit --no-gpg-sign -a -m";
        logp      = "log --graph --pretty=oneline --all --decorate=full --show-signature";
        st = "status";
      };

      extraConfig = {
        apply = {
          whitespace = "strip";
        };

        branch = {
          autosetuprebase = "always";
        };

        color = {
          pager = true;
          ui    = true;
        };

        core = {
          editor = "vim";
        };

        push = {
          default = "simple";
        };

        pull = {
          rebase = true;
        };
      };

      ignores = [
        # Compiled source #
        ###################
        "*.[568vq]"
        "*.a"
        "*.cgo1.go"
        "*.cgo2.c"
        "*.class"
        "*.dll"
        "*.exe"
        "*.exe"
        "*.o"
        "*.so"
        "[568vq].out"
        "_cgo_defun.c"
        "_cgo_export.*"
        "_cgo_gotypes.go"
        "_obj"
        "_test"
        "_testmain.go"

        # Ruby/Rails #
        ##############
        "**.orig"
        "*.gem"
        "*.rbc"
        "*.sassc"
        ".bundle/"
        ".sass-cache/"
        ".yardoc"
        "/public/assets/"
        "/public/index.html"
        "/public/system/*"
        "/vendor/bundle/"
        "_yardoc"
        "app/assets/stylesheets/scaffolds.css.scss"
        "capybara-*.html"
        "config/*.yml"
        "coverage/"
        "lib/bundler/man/"
        "pickle-email-*.html"
        "pkg/"
        "rerun.txt"
        "spec/reports/"
        "spec/tmp/*"
        "test/tmp/"
        "test/version_tmp/"
        "tmp/*"
        "tmp/**/*"

        # Packages #
        ############
        "*.7z"
        "*.bzip"
        "*.deb"
        "*.dmg"
        "*.egg"
        "*.gem"
        "*.gz"
        "*.iso"
        "*.jar"
        "*.lzma"
        "*.rar"
        "*.rpm"
        "*.tar"
        "*.xpi"
        "*.xz"
        "*.zip"

        # Logs and databases #
        ######################
        "*.log"
        "*.sqlite[0-9]"

        # OS generated files #
        ######################
        ".DS_Store"
        ".Spotlight-V100"
        ".Trashes/"
        "._*"
        ".directory"
        "Desktop.ini"
        "Icon?"
        "Thumbs.db"
        "ehthumbs.db"

        # Text-Editors files #
        ######################
        "*.bak"
        "*.pydevproject"
        "*.tmp"
        "*.tmproj"
        "*.tmproject"
        "*.un~"
        "*~"
        "*~.nib"
        ".*.sw[a-z]"
        ".\#*"
        ".classpath/"
        ".cproject/"
        ".elc/"
        ".loadpath/"
        ".metadata/"
        ".project/"
        ".redcar/"
        ".settings/"
        ".ycm_extra_conf.py"
        "/.emacs.desktop"
        "/.emacs.desktop.lock"
        "Session.vim"
        "\#*"
        "\#*\#"
        "auto-save-list/"
        "local.properties"
        "nbactions.xml"
        "nbproject/"
        "tmtags/"
        "tramp/"

        # Other Version Control Systems #
        #################################
        ".svn/"


        # Invert gitingore (Should be last) #
        #####################################
        "!.keep"
        "!.gitkeep"
        "!.gitignore"
      ];

      signing = {
        key = cfg.gpgSigningKey;
        signByDefault = true;
      };

      lfs.enable = true;
    };
  };
}
