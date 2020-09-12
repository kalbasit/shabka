{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.shabka.workstation.email;

  private = import cfg.privateEmailPath {
    inherit pkgs;
    inherit (pkgs) lib;
  };

  commonEmailAccount = {
    astroid.enable = true;
    msmtp.enable = true;
    notmuch.enable = true;
    offlineimap = {
      enable = true;

      postSyncHookCommand = ''
        ${getBin pkgs.notmuch}/bin/notmuch new
      '';

      extraConfig = {
        account = {
          autorefresh = 20;
          quick = 0;
          synclabels = true;
          labelsheader = "X-Keywords";
        };

        remote = {
          folderfilter = ''lambda foldername: foldername in ['[Gmail]/All Mail', '[Google mail]/All Mail']'';
          nametrans = ''lambda foldername: re.sub('^\[G.*ail\].All Mail$', "", foldername)'';
          realdelete = true;
          maxconnections = 1;
          keepalive = 130;
          holdconnectionopen = true;
        };
      };
    };
  };

  extendAccounts = name: value: nameValuePair name (commonEmailAccount // value);

in {
  options.shabka.workstation.email = {
    enable = mkEnableOption "Enable email accounts";

    privateEmailPath = mkOption {
      type = types.path;
      defaultText = ''
        The path to the private Email module
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = builtins.pathExists cfg.privateEmailPath;
        message = "privateEmailPath must exist";
      }
    ];

    accounts.email.accounts = mapAttrs' extendAccounts private.accounts;

    programs.astroid = {
      enable = true;
      externalEditor = ''
        ${getBin pkgs.alacritty}/bin/alacritty --title email --command nvim -c 'set ft=mail' '+set fileencoding=utf-8' '+set ff=unix' '+set enc=utf-8' '+set fo+=w' %1
      '';

      pollScript = ''
        offlineimap -o >> ${config.xdg.dataHome}/offlineimap/poll.log 2>&1
      '';
    };

    programs.msmtp.enable = true;

    programs.notmuch = {
      enable = true;
      new.tags = [ "new" "inbox" "unread" ];
      extraConfig = {
        search = {
          exclude_tags = "deleted;spam;killed;";
        };
      };
      hooks = {
        postNew = ''
          # Tag new mails using my filters
          notmuch tag --batch --input=${private.filters}
          notmuch tag -new -- tag:new

          # Update the addresses database
          # TODO: install nottoomuch-addresses and enable this
          #nottoomuch-addresses --update
        '';
      };
    };

    programs.offlineimap = {
      enable = true;
      extraConfig.general = {
        ui = "basic";
      };
    };
  };
}
