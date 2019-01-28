{}:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nixpkgs-stable-version.json);
  pinned = builtins.fetchGit {
    inherit (pinnedVersion) url rev ref;
  };

  importPinned = import pinned {
    config = {};
    overlays = [];
  };

  assertMsg = (import ../util/assertMsg.nix { lib = importPinned.lib; }).assertMsg;

  mkAssertMsg = name: "${name} is available upsteam, kill this patch";

  patches = [
    # Improve pam.security.u2f
    # https://github.com/NixOS/nixpkgs/pull/54756/files
    (
      # assert assertMsg (! importPinned.nixos.security.pam ? u2f) (mkAssertMsg "security.pam.u2f");
      ./0001-pam-u2f-refactor-docs-about-u2f_keys-path.patch
    )
  ];

  patched = importPinned.runCommand "nixpkgs-stable-${pinnedVersion.rev}"
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
