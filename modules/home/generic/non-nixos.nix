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
      "ssh/authorized_keys".source = builtins.fetchurl {
        url = "https://github.com/kalbasit.keys";
        sha256 = "033rs0pnm8aiycrfmx04qx8fmnkfdhp4hy3kdpgil3cgbgff9736";
      };
    };

    fonts.fontconfig.enableProfileFonts = true;
  };
}
