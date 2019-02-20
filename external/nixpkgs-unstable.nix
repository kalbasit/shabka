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
    ./nixpkgs-unstable.python36Packages-jsbeautifier-disable-check.patch

    # TODO: VirtualBox 5 does not work with latest kernel. Update to VirtualBox 6
    # https://github.com/NixOS/nixpkgs/pull/53120
    ./nixpkgs-unstable.53120-upgrade-virtualbox.patch

    # luarocks: patch sw_vers and gcc
    # This fixes luarocks on Darwin
    # https://github.com/NixOS/nixpkgs/issues/55553
    # https://github.com/NixOS/nixpkgs/pull/55580
    ./nixpkgs-unstable.55580-fix-luarocks-on-darwin.patch
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
