{ pkgs, ... }:

let
  # TODO: use runCommand here instead of full derivation
  less-config = pkgs.stdenv.mkDerivation rec {
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
  home.file.".less".source = "${less-config}/share/less/less";
}
