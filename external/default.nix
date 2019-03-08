{ stdenvNoCC, callPackage }:

let
  mkExternal =
    { name, revision, src, patches }:

    stdenvNoCC.mkDerivation {
      inherit src patches;
      name = "${name}-${revision}";
      preferLocalBuild = true;

      buildPhase = ''
        echo -n "${revision}" > .git-revision
      '';

      installPhase = ''
        cp -r . $out
      '';

      fixupPhase = ":";
    };
in {
  home-manager = callPackage ./home-manager { inherit mkExternal; };
  kalbasit = callPackage ./kalbasit { inherit mkExternal; };
  nix-darwin = callPackage ./nix-darwin { inherit mkExternal; };
  nixos-hardware = callPackage ./nixos-hardware { inherit mkExternal; };
  nixpkgs = callPackage ./nixpkgs { inherit mkExternal; };
  nur = callPackage ./nur { inherit mkExternal; };
}
