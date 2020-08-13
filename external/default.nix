{ stdenvNoCC }:

let
  mkExternal = { name, revision, src, patches }:
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
  inherit mkExternal;

  edolstra = import ./edolstra { inherit mkExternal; };
  hammerspoon = import ./hammerspoon { inherit mkExternal; };
  home-manager = import ./home-manager { inherit mkExternal; };
  iohk-nix = import ./iohk-nix { inherit mkExternal; };
  kalbasit = import ./kalbasit { inherit mkExternal; };
  nix-darwin = import ./nix-darwin { inherit mkExternal; };
  nixos-hardware = import ./nixos-hardware { inherit mkExternal; };
  nixpkgs = import ./nixpkgs { inherit mkExternal; };
  nur = import ./nur { inherit mkExternal; };
  risson = import ./risson { inherit mkExternal; };
}
