{ assertMsg
, pkgs
}:

with pkgs.lib;

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./home-manager-version.json);
  pinned = builtins.fetchGit {
    inherit (pinnedVersion) url rev;
  };

  importPinned = import pinned {};

  mkAssertMsg = name: "${name} is available upsteam, kill this patch";

  patches = [
    # https://github.com/rycee/home-manager/pull/472
    (
      pkgs.fetchpatch {
        url = "https://github.com/rycee/home-manager/commit/f84e886d68d5c6d9afc6e2128403b3d139bae7e3.patch";
        sha256 = "1laz1r78ailkizxzcdrgki4yapbvwnzmjn92vzjlkq91x6l3k8hw";
      }
    )

    # https://github.com/rycee/home-manager/pull/473
    (
      pkgs.fetchpatch {
        url = "https://github.com/rycee/home-manager/commit/c3a3e49b7b0c05c47d98e829fb14dc482c4ff217.patch";
        sha256 = "0v5yrxdjsgafzdnzlqzy1h6b69xnz1l1sy9ay1armz8c11y1sbcj";
      }
    )

    # https://github.com/rycee/home-manager/pull/474
    (
      pkgs.fetchpatch {
        url = "https://github.com/rycee/home-manager/pull/474.patch";
        sha256 = "01rnl2c9k3kx0s33ap81p02ijjxciak2y1cvl553i45xx4g8siw1";
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
