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
    # update network-manager to 1.14.4
    (
      pkgs.fetchpatch {
        url = "https://github.com/NixOS/nixpkgs/pull/51122.patch";
        sha256 = "0qvxwx0vz11najs3x0p005kmwl0c31ga1zwi7h68pd96jwcmg8r8";
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
