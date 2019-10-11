{ pkgs, config, lib, ... }:

with lib;

let
  shabka = import <shabka> { };
in {
  options.shabka.darwinConfig = mkOption {
    type = types.attrs;
    default = {};
    defaultText = ''
      NixOS configuration. On NixOS machines, it should be the config itself.
      On non-NixOS machines, all the required keys must be set manually.
    '';
  };

  config = mkIf (config.shabka.darwinConfig != {}) {
    fonts.fontconfig.enableProfileFonts = true;

    # XXX: Having dconf enabled (which is default) breaks switching on Darwin
    #
    #   dbus-run-session: failed to execute message bus daemon 'dbus-daemon': No such file or directory
    #   dbus-run-session: EOF reading address from bus daemon
    dconf.enable = false;
  };
}
