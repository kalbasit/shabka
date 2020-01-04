{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    ./fix-nvim-terraform-plugin.patch

    # https://github.com/NixOS/nixpkgs/pull/76926
    ./76926-vim-direnv-hardcode-direnv-path.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-unstable";
    revision = pinnedVersion.rev;
  };
in
  (import patched { config = { allowUnfree = true; }; overlays = []; } // { path = patched; })
