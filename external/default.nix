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

    # XXX: callPackage must make mkExternal visible
    callPackage2 = path: args: callPackage path (args // { inherit mkExternal; });
in {
  home-manager = callPackage2 ./home-manager { };
  kalbasit = callPackage2 ./kalbasit { };
  nix-darwin = callPackage2 ./nix-darwin { };
  nixos-hardware = callPackage2 ./nixos-hardware { };
  nixpkgs = callPackage2 ./nixpkgs { };
  nur = callPackage2 ./nur { };
}
