# TODO(high): the gpg key must be configurable
{ config, pkgs, lib, ... }:

with lib;

let
  shabka = import <shabka> { };
in {
  options.mine.git.enable = mkEnableOption "git";

  config = mkIf config.mine.git.enable {
    home.packages = with pkgs; [
      (gitAndTools.git-appraise or shabka.external.nixpkgs.release-unstable.gitAndTools.git-appraise)
      gitAndTools.hub
      gitAndTools.tig
    ];

    programs.git = {
      enable = true;

      userName = "Wael M. Nasreddine";

      userEmail = "wael.nasreddine@gmail.com";

      aliases = {
        aa             = "add --all .";
        aap            = "!git aa -p";
        amend          = "commit --amend";
        cb             = "checkout -b";
        ci             = "commit";
        co             = "checkout";
        com            = "checkout master";
        credit         = "\"!f() { git commit --amend --author \\\"$1 <$2>\\\" -C HEAD; }; f\"";
        dc             = "diff --cached";
        di             = "diff";
        fa             = "fetch --all";
        famff          = "!git fetch --all && git merge --ff-only origin/master";
        famm           = "!git fetch --all && git merge origin/master";
        faro           = "!git fetch --all && git rebase origin/master";
        generate-patch = "!git-format-patch --patch-with-stat --raw --signoff";
        l              = "log --graph --pretty=format':%C(yellow)%h %Cgreen%G?%Cblue%d%Creset %s %C(white) %an, %ar%Creset'";
        lol            = "log --pretty=oneline --abbrev-commit --graph --decorate";
        ls-ignored     = "ls-files --others -i --exclude-standard";
        pob            = "\"!f() { git push -u \\\"\${1:-origin}\\\" \\\"$(git symbolic-ref HEAD)\\\"; }; f\"";
        pobf           = "\"!f() { git push -fu \\\"\${1:-origin}\\\" \\\"$(git symbolic-ref HEAD)\\\"; }; f\"";
        sp             = "pull --rebase --autostash";
        st             = "status";
        unstage        = "reset HEAD --";
        who            = "shortlog -s -s";

        # list files which have changed since REVIEW_BASE
        # (REVIEW_BASE defaults to 'master' in my zshrc)
        files          = "\"!git diff --name-only \$(git merge-base HEAD \\\"\${REVIEW_BASE:-master}\\\")\"";

        # Same as above, but with a diff stat instead of just names
        # (better for interactive use)
        stat           = "\"!git diff --stat \$(git merge-base HEAD \\\"\${REVIEW_BASE:-master}\\\")\"";

        # Open all files changed since REVIEW_BASE in Vim tabs
        # Then, run fugitive's :Gdiff in each tab, and finally
        review = "\"!nvim -p $(git files) +\\\"tabdo Gdiff \${REVIEW_BASE:-master}\\\"\"";

        # Same as the above, except specify names of files as arguments,
        # instead of opening all files:
        # git reviewone foo.js bar.js
        reviewone = "\"!nvim -p +\\\"tabdo Gdiff \${REVIEW_BASE:-master}\\\"\"";
      };

      extraConfig = {
        apply = {
          whitespace = "strip";
        };

        color = {
          pager = true;
          ui    = "auto";
        };

        core = {
          whitespace = "trailing-space,space-before-tab,-indent-with-non-tab,cr-at-eol";
        };

        diff = {
          tool = "vimdiff";
        };

        difftool = {
          prompt = false;
        };

        help = {
          autocorrect = 30;
        };

        http = {
          cookiefile = "~/.gitcookies";
        };

        "http \"https://gopkg.in\"" = {
          followRedirects = true;
        };

        merge = {
          log  = true;
          tool = "vimdiff";
        };

        mergetool = {
          prompt = true;
        };

        "mergetool \"vimdiff\"" = optionalAttrs config.mine.neovim.enable {
          cmd = "nvim -d $LOCAL $REMOTE $MERGED -c '$wincmd w' -c 'wincmd J'";
        };

        "protocol \"keybase\"" = {
          allow = "always";
        };

        push = {
          default = "current";
        };

        sendemail = {
          smtpserver       = "${pkgs.msmtp}/bin/msmtp";
          smtpserveroption = "--account=personal";
        };

        status = {
          submodule = 1;
        };

        "url \"https://github\"" = {
          insteadOf = "git://github";
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

      includes = [
        { path = "~/.gitconfig.secrets"; }
      ];

      signing = {
        key = "me@yl.codes";
        signByDefault = true;
      };
    };
  };
}
