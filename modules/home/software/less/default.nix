{ config, pkgs, lib, ... }:

with lib;

let
  less-config = pkgs.stdenvNoCC.mkDerivation rec {
    name = "less-config";

    phases = [ "buildPhase" "installPhase" ];

    src = ./.;

    buildInputs = [ pkgs.less ];

    buildPhase = ''
      lesskey -o less $src/lesskey
    '';

    installPhase = ''
      install -Dm644 less $out/share/less/less
    '';
  };

in {
  options.mine.less.enable = mkEnableOption "less";

  config = mkIf config.mine.less.enable {
    home.file.".less".source = "${less-config}/share/less/less";
  };
}
