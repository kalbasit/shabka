{ assertMsg
, pkgs
}:

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
    # archiver
    # https://github.com/NixOS/nixpkgs/pull/49956
    (
      assert assertMsg (! importPinned ? archiver) (mkAssertMsg "archiver");
      pkgs.fetchpatch {
        url = "https://github.com/NixOS/nixpkgs/pull/49956.patch";
        sha256 = "0rqx0w4krm8r3pj4ismf465bc7z687k3fm8akiixacd0qm2nnwgz";
      }
    )

    # ssh-agents
    # https://github.com/NixOS/nixpkgs/pull/49892
    (
      assert assertMsg (! importPinned ? ssh-agents) (mkAssertMsg "ssh-agents");
      pkgs.fetchpatch {
        url = "https://github.com/NixOS/nixpkgs/pull/49892.patch";
        sha256 = "0nzamsmm50sm0lyyrpanv7csn75hiyx3byzrw8nqs0sdskmrvr8r";
      }
    )

    # neovim with nodejs support
    # https://github.com/NixOS/nixpkgs/pull/49884
    (
      let
        neovimFn = import "${pinned}/pkgs/applications/editors/neovim/wrapper.nix";
      in
      assert assertMsg (! (builtins.functionArgs neovimFn) ? "withNodeJs") (mkAssertMsg "neovim withNodeJs support");
      pkgs.fetchpatch {
        url = "https://github.com/NixOS/nixpkgs/pull/49884.patch";
        sha256 = "0hd00ciivmsj3d3idwvwc1wlwi3r599f2l47dyq4i3z0wzfiim08";
      }
    )

    # neovim gist-vim depends on WebAPI
    # https://github.com/NixOS/nixpkgs/pull/49881
    (
      assert assertMsg (! importPinned.vimPlugins.gist-vim ? dependencies) (mkAssertMsg "gist-vim");
      pkgs.fetchpatch {
        url = "https://github.com/NixOS/nixpkgs/pull/49881.patch";
        sha256 = "1f1vhxfyqkm9x2xmc6aqw62sdq7a71fb6ymns8myg6dlhxniw4gb";
      }
    )

    # neovim all my plugins
    # https://github.com/NixOS/nixpkgs/pull/49879
    (
      assert assertMsg (! importPinned.vimPlugins ? yats-vim) (mkAssertMsg "yats-vim");
      pkgs.fetchpatch {
        url = "https://github.com/NixOS/nixpkgs/pull/49879.patch";
        sha256 = "1gqgalilmp0k99vrw7pxriqbr3vbkq2g3xjddcza0yadcp24wn5i";
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
