{ config, lib, ... }:

with lib;

{
  options.mine.darwinConfig = mkOption {
    type = types.attrs;
    default = {};
    defaultText = ''
      NixOS configuration. On NixOS machines, it should be the config itself.
      On non-NixOS machines, all the required keys must be set manually.
    '';
  };

  config = mkIf (config.mine.darwinConfig != {}) {
    home.file = {
      "ssh/authorized_keys".source = import ../../../external/kalbasit-keys.nix;
    };

    fonts.fontconfig.enableProfileFonts = true;
  };
}
