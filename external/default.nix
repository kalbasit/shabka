{ pkgs ? import <nixpkgs> { config = { }; overlays = [ ]; } }:

with pkgs;

let
  mkExternal =
    { name, revision, src, patches }:
    runCommand "${name}-${revision}"
      {
        inherit src patches;

        preferLocalBuild = true;
      }
      ''
        echo -n "${revision}" > .git-revision
        cp -r . $out
        chmod -R +w $out
        for p in $patches; do
          echo "Applying patch $p";
          patch -d $out -p1 < "$p";
        done
      '';

in {
  home-manager = callPackage ./home-manager { inherit mkExternal; };
  kalbasit = callPackage ./kalbasit { inherit mkExternal; };
  nix-darwin = callPackage ./nix-darwin { inherit mkExternal; };
  nixos-hardware = callPackage ./nixos-hardware { inherit mkExternal; };
  nixpkgs = callPackage ./nixpkgs { inherit mkExternal; };
  nur = callPackage ./nur { inherit mkExternal; };
}
