{ assertMsg
, pkgs
}:

with pkgs.lib;

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nixpkgs-version.json);
  pinned = builtins.fetchGit {
    inherit (pinnedVersion) url rev;
  };

  importPinned = import pinned {
    config = {};
    overlays = [];
  };

  mkAssertMsg = name: "${name} is available upsteam, kill this patch";

  patches = [
    # ssh-agents
    # https://github.com/NixOS/nixpkgs/pull/49892
    (
      assert assertMsg (! importPinned ? ssh-agents) (mkAssertMsg "ssh-agents");
      pkgs.fetchpatch {
        url = "https://github.com/NixOS/nixpkgs/pull/49892.patch";
        sha256 = "0nzamsmm50sm0lyyrpanv7csn75hiyx3byzrw8nqs0sdskmrvr8r";
      }
    )

    # update corgi
    # https://github.com/NixOS/nixpkgs/pull/50488
    (
      assert assertMsg (! versionAtLeast (getVersion importPinned.corgi) "0.2.4") (mkAssertMsg "corgi");
      pkgs.fetchpatch {
        url = "https://github.com/NixOS/nixpkgs/pull/50488.patch";
        sha256 = "01bldiwl79xqjc5lpdc7bv2c8zpz7bkl9ilxaklgrw539sagg4kv";
      }
    )
  ];

  patched = pkgs.runCommand "nixpkgs-${pinnedVersion.rev}"
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
