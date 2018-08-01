self: super:

{
  nix-verify = (super.stdenv.mkDerivation rec {
    name = "nix-verify";

    src = ./.;

    phases = [ "installPhase" ];

    installPhase = ''
      install -Dm755 $src/nix-verify $out/bin/nix-verify
    '';
  });
}
