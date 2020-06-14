{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    ./darwin-fix-nix.patch
    ./86626-pure-vim-go.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-unstable";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = { allowUnfree = true; }; overlays = []; } // { path = patched; })
