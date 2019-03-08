{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # Improve pam.security.u2f
    # https://github.com/NixOS/nixpkgs/commit/f072cfe1ebff79efaa409258a38646a62c94dbff
    ./54756-nixos-pam-refactor-U2F-docs-about-u2f_keys-path.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "nixpkgs-release-18-09";
    revision = pinnedVersion.rev;
  };
in {
  path = patched;
  imported = import patched {
    config = {
      nixpkgs.config.allowUnfree = true; # TODO: this should be inherited, not hardcoded!
    };
    overlays = [];
  };
}
