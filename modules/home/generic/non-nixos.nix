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
        sha256 = "1ijzn5nmh7fcpky9zz6dsbps3pad67nlp0cs0zrs46f0bcy9cqjr";
      };
    };

    fonts.fontconfig.enableProfileFonts = true;
  };
}
