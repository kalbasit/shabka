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

    # luarocks: patch sw_vers and gcc
    # This fixes luarocks on Darwin
    # https://github.com/NixOS/nixpkgs/issues/55553
    # https://github.com/NixOS/nixpkgs/pull/55580
    (fetchpatch {
      url = "https://github.com/NixOS/nixpkgs/commit/5b0e7de0bd2f329b506a57c54c4e68af3e2a6935.patch";
      sha256 = "0g6fhn9d5i6rbyq29dhdiqm7nlqm04dr4l2w79mr01048brmxdmw";
    })
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
