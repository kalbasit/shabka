{ fetchpatch, runCommand }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nixpkgs-unstable-version.json);
  pinned = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  importPinned = import pinned {
    config = {};
    overlays = [];
  };

  patches = [
    # TODO: jsbeautifier is not working upstream and tests need to be disabled
    ./python36Packages-jsbeautifier-disable-check.patch

    # TODO: VirtualBox 5 does not work with latest kernel. Update to VirtualBox 6
    # https://github.com/NixOS/nixpkgs/pull/53120
    ./53120-upgrade-virtualbox.patch
  ];

  patched = runCommand "nixpkgs-unstable-${pinnedVersion.rev}"
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
