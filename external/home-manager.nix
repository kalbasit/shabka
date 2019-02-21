{ fetchpatch, runCommand }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./home-manager-version.json);
  pinned = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # https://github.com/rycee/home-manager/pull/474
    ./home-manager.474-fix-switch-user-without-profile.patch

    # https://github.com/rycee/home-manager/pull/583
    ./home-manager.583-activate-hm-through-postactivation.patch

    # XXX: PR PENDING
    # profile: opensource
    # story: home-manager_login-as-user-before-activating
    ./home-manager.PR-PENDING-enable-user-packages.patch

    # XXX: PR PENDING
    # profile: opensource
    # story: home-manager_nix-darwin-install-as-user-packages
    ./home-manager.PR-PENDING-login-as-user.patch
  ];

  patched = runCommand "home-manager-${pinnedVersion.rev}"
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
