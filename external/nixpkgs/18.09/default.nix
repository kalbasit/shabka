{ fetchpatch, runCommand }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);
  pinned = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # Improve pam.security.u2f
    # https://github.com/NixOS/nixpkgs/commit/f072cfe1ebff79efaa409258a38646a62c94dbff
    ./54756-nixos-pam-refactor-U2F-docs-about-u2f_keys-path.patch
  ];

  patched = runCommand "nixpkgs-18.09-${pinnedVersion.rev}"
    {
      inherit pinned patches;

      preferLocalBuild = true;
    }
    ''
      cp -r $pinned $out
      echo -n "${pinnedVersion.rev}" > $out/.git-revision
      chmod -R +w $out
      for p in $patches; do
        echo "Applying patch $p";
        patch -d $out -p1 < "$p";
      done
    '';
in {
  path = patched;
  imported = import patched {
    config = {
      nixpkgs.config.allowUnfree = true; # TODO: this should be inherited, not hardcoded!
    };
    overlays = [];
  };
}
