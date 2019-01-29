{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.plex;
in {
  options.mine.plex = {
    enable = mkEnableOption "Enable Plex service";
    dataDir = mkOption {
      type = types.str;
      description = "The path to Plex's data directory";
    };
  };

  config = mkIf cfg.enable {
    # Plex service
    services.plex = {
      inherit (cfg) dataDir;

      enable = true;
      openFirewall = true;

      # TODO: setup the plugins after the migration, if any.
      managePlugins = false;
      extraPlugins = [];

      package = pkgs.plex.overrideAttrs (_: let
        # see https://www.plex.tv/media-server-downloads/ for 64bit rpm
        version = "1.14.1.5488";
        vsnHash = "cc260c476";
        sha1 = "r7sp2qx6vhvi7q02ygxzdl7w8ydmnzja";
      in {
        name = "plex-${version}";
        src = pkgs.fetchurl {
          url = "https://downloads.plex.tv/plex-media-server/${version}-${vsnHash}/plexmediaserver-${version}-${vsnHash}.x86_64.rpm";
          inherit sha1;
        };

        preferLocalBuild = true;

        # TODO: remove this once https://github.com/NixOS/nixpkgs/pull/47562 is merged in.
        installPhase = let
          dataDir = cfg.dataDir;
        in ''
          install -d $out/usr/lib
          cp -dr --no-preserve='ownership' usr/lib/plexmediaserver $out/usr/lib/

          # Now we need to patch up the executables and libraries to work on Nix.
          # Side note: PLEASE don't put spaces in your binary names. This is stupid.
          for bin in "Plex Media Server"              \
                     "Plex DLNA Server"               \
                     "Plex Media Scanner"             \
                     "Plex Relay"                     \
                     "Plex Script Host"               \
                     "Plex Transcoder"                \
                     "Plex Tuner Service"             ; do
            patchelf --set-interpreter "${pkgs.glibc.out}/lib/ld-linux-x86-64.so.2" "$out/usr/lib/plexmediaserver/$bin"
            patchelf --set-rpath "$out/usr/lib/plexmediaserver" "$out/usr/lib/plexmediaserver/$bin"
          done

          find $out/usr/lib/plexmediaserver/Resources -type f -a -perm -0100 \
              -print -exec patchelf --set-interpreter "${pkgs.glibc.out}/lib/ld-linux-x86-64.so.2" '{}' \;

          # executables need libstdc++.so.6
          ln -s "${pkgs.stdenv.lib.makeLibraryPath [ pkgs.stdenv.cc.cc ]}/libstdc++.so.6" "$out/usr/lib/plexmediaserver/libstdc++.so.6"

          # Our next problem is the "Resources" directory in /usr/lib/plexmediaserver.
          # This is ostensibly a skeleton directory, which contains files that Plex
          # copies into its folder in /var. Unfortunately, there are some SQLite
          # databases in the directory that are opened at startup. Since these
          # database files are read-only, SQLite chokes and Plex fails to start. To
          # solve this, we keep the resources directory in the Nix store, but we
          # rename the database files and replace the originals with symlinks to
          # /var/lib/plex. Then, in the systemd unit, the base database files are
          # copied to /var/lib/plex before starting Plex.
          RSC=$out/usr/lib/plexmediaserver/Resources
          for db in "com.plexapp.plugins.library.db"; do
              mv $RSC/$db $RSC/base_$db
              ln -s "${dataDir}/.skeleton/$db" $RSC/$db
          done
        '';
      });
    };

    # Currently the Plex systemd service tries to install the skeleton directory
    # using the install command, however that command does not work when the
    # directory where the installation is happening is living on an NFS mount.
    # TODO: This will not allow me to transition the Plex plugins to Nix so I
    # must solve this and send a patch upstream to get it working nicely. Perhaps
    # a simple test before the installation should suffice.
    systemd.services.plex.preStart = lib.mkForce "true";
  };
}
