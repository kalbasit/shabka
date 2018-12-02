{ assertMsg
, pkgs
}:

with pkgs.lib;

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./home-manager-version.json);
  pinned = builtins.fetchGit {
    inherit (pinnedVersion) url rev;
  };

  mkAssertMsg = name: "${name} is available upsteam, kill this patch";

  patches = [
    (pkgs.fetchpatch {
      url = "https://github.com/rycee/home-manager/commit/ba1551ba7c970b783b627b1176112b9630c58ca0.patch";
      sha256 = "1laz1r78ailkizxzcdrgki4yapbvwnzmjn92vzjlkq91x6l3k8hw";
    })
  ];

  patched = pkgs.runCommand "home-manager-${pinnedVersion.rev}"
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
