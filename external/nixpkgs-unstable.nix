{ assertMsg
, pkgs
}:

with pkgs;
with pkgs.lib;

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nixpkgs-unstable-version.json);
  pinned = builtins.fetchGit {
    inherit (pinnedVersion) url rev;
  };

  importPinned = import pinned {
    config = {};
    overlays = [];
  };

  mkAssertMsg = name: "${name} is available upsteam, kill this patch";

  patches = [];

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
