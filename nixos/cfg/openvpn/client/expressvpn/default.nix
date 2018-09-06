{ pkgs, ... }:

let
  remotes = {
    france-paris-2 = "france-paris-2-ca-version-2.expressnetw.com 1195";
    france-strasbourg = "france-strasbourg-ca-version-2.expressnetw.com 1195";
  };

in {
  services.openvpn.servers = {
    # TODO: refactor the following two blocks in the above let expression so we
    # can iterate over the remotes and create the configuration sets.
    client-expressvpn-france-paris-2 = {
      autoStart = false;
      config = builtins.readFile (pkgs.substituteAll {
        src = /private/network-secrets/vpn/client/expressvpn/config.ovpn;

        remote = "${remotes.france-paris-2}";
      });
      updateResolvConf = true;
    };

    client-expressvpn-france-strasbourg = {
      autoStart = false;
      config = builtins.readFile (pkgs.substituteAll {
        src = /private/network-secrets/vpn/client/expressvpn/config.ovpn;

        remote = "${remotes.france-strasbourg}";
      });
      updateResolvConf = true;
    };
  };
}
