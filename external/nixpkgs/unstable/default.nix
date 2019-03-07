{ fetchpatch, runCommand }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);
  pinned = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # TODO: jsbeautifier is not working upstream and tests need to be disabled
    ./python36Packages-jsbeautifier-disable-check.patch

    # XXX: VirtualBox 5.2.22 does not work with latest kernel, update to
    # version 5.2.26.
    # https://github.com/NixOS/nixpkgs/pull/56210
    ./56210-update-vbox-5.2.26.patch
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
in {
  path = patched;
  config = [];
  overlays = [];
}
