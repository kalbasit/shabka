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
    ./nixos-pam-refactor-U2F-docs-about-u2f_keys-path-5475.patch
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
