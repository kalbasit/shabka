{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # https://github.com/NixOS/nixpkgs/pull/77423
    ./0001-tmux-extraTmuxConf-to-extraConfig
    # https://github.com/NixOS/nixpkgs/pull/79916
    ./flashplayer-update.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-19-09";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = { allowUnfree = true; }; overlays = []; } // { path = patched; })
