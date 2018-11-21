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
    (
      pkgs.fetchpatch {
        url = "https://github.com/rycee/home-manager/pull/450.patch";
        sha256 = "1m8vdgkdcf3jis67g9vl8pkggm43s8c242bnwaj6ccxg9sad8jfc";
      }
    )
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
