let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nixpkgs-stable-version.json);
  pinned = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  importPinned = import pinned {
    config = {};
    overlays = [];
  };

  patches = [
    # Improve pam.security.u2f
    # https://github.com/NixOS/nixpkgs/commit/f072cfe1ebff79efaa409258a38646a62c94dbff
    ./nixpkgs-stable.54756-nixos-pam-refactor-U2F-docs-about-u2f_keys-path.patch

    # Add Gitlab extra database config
    ./nixpkgs-stable.56072-gitlab-add-database-extra-config.patch
  ];

  patched = importPinned.runCommand "nixpkgs-stable-${pinnedVersion.rev}"
    {
      inherit pinned patches;

      preferLocalBuild = true;
    }
    ''
      cp -r $pinned $out
      chmod -R +w $out
      for p in $patches; do
        echo "Applying patch $p";
        patch -d $out -p1 < "$p";
      done
    '';
in
  patched
