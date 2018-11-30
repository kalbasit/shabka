{ config, pkgs, lib, ... }:

with lib;
with import ../../../../../util;

let
  cfg = config.mine.openvpn.client.expressvpn;

  remotes = {
    albania = "albania-ca-version-2.expressnetw.com 1195";
    algeria = "algeria-ca-version-2.expressnetw.com 1195";
    andorra = "andorra-ca-version-2.expressnetw.com 1195";
    argentina = "argentina-ca-version-2.expressnetw.com 1195";
    armenia = "armenia-ca-version-2.expressnetw.com 1195";
    australia-brisbane = "australia-brisbane-ca-version-2.expressnetw.com 1195";
    australia-melbourne = "australia-melbourne-ca-version-2.expressnetw.com 1195";
    australia-perth = "australia-perth-ca-version-2.expressnetw.com 1195";
    australia-sydney-2 = "australia-sydney-2-ca-version-2.expressnetw.com 1195";
    australia-sydney = "australia-sydney-ca-version-2.expressnetw.com 1195";
    austria = "austria-ca-version-2.expressnetw.com 1195";
    azerbaijan = "azerbaijan-ca-version-2.expressnetw.com 1195";
    bangladesh = "bangladesh-ca-version-2.expressnetw.com 1195";
    belarus = "belarus-ca-version-2.expressnetw.com 1195";
    belgium = "belgium-ca-version-2.expressnetw.com 1195";
    bhutan = "bhutan-ca-version-2.expressnetw.com 1195";
    bosniaandherzegovina = "bosniaandherzegovina-ca-version-2.expressnetw.com 1195";
    brazil-2 = "brazil-2-ca-version-2.expressnetw.com 1195";
    brazil = "brazil-ca-version-2.expressnetw.com 1195";
    brunei = "brunei-ca-version-2.expressnetw.com 1195";
    bulgaria = "bulgaria-ca-version-2.expressnetw.com 1195";
    cambodia = "cambodia-ca-version-2.expressnetw.com 1195";
    canada-montreal = "canada-montreal-ca-version-2.expressnetw.com 1195";
    canada-toronto-2 = "canada-toronto-2-ca-version-2.expressnetw.com 1195";
    canada-toronto = "canada-toronto-ca-version-2.expressnetw.com 1195";
    canada-vancouver = "canada-vancouver-ca-version-2.expressnetw.com 1195";
    chile = "chile-ca-version-2.expressnetw.com 1195";
    colombia = "colombia-ca-version-2.expressnetw.com 1195";
    costarica = "costarica-ca-version-2.expressnetw.com 1195";
    croatia = "croatia-ca-version-2.expressnetw.com 1195";
    cyprus = "cyprus-ca-version-2.expressnetw.com 1195";
    czechrepublic = "czechrepublic-ca-version-2.expressnetw.com 1195";
    denmark = "denmark-ca-version-2.expressnetw.com 1195";
    ecuador = "ecuador-ca-version-2.expressnetw.com 1195";
    estonia = "estonia-ca-version-2.expressnetw.com 1195";
    finland = "finland-ca-version-2.expressnetw.com 1195";
    france-paris-1 = "france-paris-1-ca-version-2.expressnetw.com 1195";
    france-paris-2 = "france-paris-2-ca-version-2.expressnetw.com 1195";
    france-strasbourg = "france-strasbourg-ca-version-2.expressnetw.com 1195";
    georgia = "georgia-ca-version-2.expressnetw.com 1195";
    germany-darmstadt = "germany-darmstadt-ca-version-2.expressnetw.com 1195";
    germany-frankfurt-1 = "germany-frankfurt-1-ca-version-2.expressnetw.com 1195";
    germany-frankfurt-2 = "germany-frankfurt-2-ca-version-2.expressnetw.com 1195";
    germany-nuremberg = "germany-nuremberg-ca-version-2.expressnetw.com 1195";
    greece = "greece-ca-version-2.expressnetw.com 1195";
    guatemala = "guatemala-ca-version-2.expressnetw.com 1195";
    hongkong-1 = "hongkong-1-ca-version-2.expressnetw.com 1195";
    hongkong-2 = "hongkong-2-ca-version-2.expressnetw.com 1195";
    hongkong-4 = "hongkong4-ca-version-2.expressnetw.com 1195";
    hungary = "hungary-ca-version-2.expressnetw.com 1195";
    iceland = "iceland-ca-version-2.expressnetw.com 1195";
    india-chennai = "india-chennai-ca-version-2.expressnetw.com 1195";
    india-mumbai-1 = "india-mumbai-1-ca-version-2.expressnetw.com 1195";
    indonesia = "indonesia-ca-version-2.expressnetw.com 1195";
    ireland = "ireland-ca-version-2.expressnetw.com 1195";
    isleofman = "isleofman-ca-version-2.expressnetw.com 1195";
    israel = "israel-ca-version-2.expressnetw.com 1195";
    italy-cosenza = "italy-cosenza-ca-version-2.expressnetw.com 1195";
    italy-milan = "italy-milan-ca-version-2.expressnetw.com 1195";
    japan-tokyo-1 = "japan-tokyo-1-ca-version-2.expressnetw.com 1195";
    japan-tokyo-2 = "japan-tokyo-2-ca-version-2.expressnetw.com 1195";
    jersey = "jersey-ca-version-2.expressnetw.com 1195";
    kazakhstan = "kazakhstan-ca-version-2.expressnetw.com 1195";
    kenya = "kenya-ca-version-2.expressnetw.com 1195";
    kyrgyzstan = "kyrgyzstan-ca-version-2.expressnetw.com 1195";
    laos = "laos-ca-version-2.expressnetw.com 1195";
    latvia = "latvia-ca-version-2.expressnetw.com 1195";
    liechtenstein = "liechtenstein-ca-version-2.expressnetw.com 1195";
    lithuania = "lithuania-ca-version-2.expressnetw.com 1195";
    luxembourg = "luxembourg-ca-version-2.expressnetw.com 1195";
    macau = "macau-ca-version-2.expressnetw.com 1195";
    macedonia = "macedonia-ca-version-2.expressnetw.com 1195";
    malaysia = "malaysia-ca-version-2.expressnetw.com 1195";
    malta = "malta-ca-version-2.expressnetw.com 1195";
    mexico = "mexico-ca-version-2.expressnetw.com 1195";
    moldova = "moldova-ca-version-2.expressnetw.com 1195";
    monaco = "monaco-ca-version-2.expressnetw.com 1195";
    mongolia = "mongolia-ca-version-2.expressnetw.com 1195";
    montenegro = "montenegro-ca-version-2.expressnetw.com 1195";
    myanmar = "myanmar-ca-version-2.expressnetw.com 1195";
    nepal = "nepal-ca-version-2.expressnetw.com 1195";
    netherlands-amsterdam = "netherlands-amsterdam-ca-version-2.expressnetw.com 1195";
    netherlands-rotterdam = "netherlands-rotterdam-ca-version-2.expressnetw.com 1195";
    netherlands-thehague = "netherlands-thehague-ca-version-2.expressnetw.com 1195";
    newzealand = "newzealand-ca-version-2.expressnetw.com 1195";
    norway = "norway-ca-version-2.expressnetw.com 1195";
    pakistan = "pakistan-ca-version-2.expressnetw.com 1195";
    peru = "peru-ca-version-2.expressnetw.com 1195";
    philippines-viasingapore = "ph-via-sing-ca-version-2.expressnetw.com 1195";
    poland = "poland-ca-version-2.expressnetw.com 1195";
    portugal = "portugal-ca-version-2.expressnetw.com 1195";
    romania = "romania-ca-version-2.expressnetw.com 1195";
    russia = "russia-ca-version-2.expressnetw.com 1195";
    serbia = "serbia-ca-version-2.expressnetw.com 1195";
    singapore-cbd = "singapore-cbd-ca-version-2.expressnetw.com 1195";
    singapore-jurong = "singapore-jurong-ca-version-2.expressnetw.com 1195";
    slovakia = "slovakia-ca-version-2.expressnetw.com 1195";
    slovenia = "slovenia-ca-version-2.expressnetw.com 1195";
    southafrica = "southafrica-ca-version-2.expressnetw.com 1195";
    southkorea-2 = "southkorea2-ca-version-2.expressnetw.com 1195";
    southkorea = "southkorea-ca-version-2.expressnetw.com 1195";
    spain-barcelona = "spain-barcelona-ca-version-2.expressnetw.com 1195";
    spain-madrid = "spain-ca-version-2.expressnetw.com 1195";
    srilanka = "srilanka-ca-version-2.expressnetw.com 1195";
    sweden-2 = "sweden-2-ca-version-2.expressnetw.com 1195";
    sweden = "sweden-ca-version-2.expressnetw.com 1195";
    switzerland-2 = "switzerland-2-ca-version-2.expressnetw.com 1195";
    switzerland = "switzerland-ca-version-2.expressnetw.com 1195";
    taiwan-3 = "taiwan-3-ca-version-2.expressnetw.com 1195";
    thailand = "thailand-ca-version-2.expressnetw.com 1195";
    turkey = "turkey-ca-version-2.expressnetw.com 1195";
    uk-berkshire-2 = "uk-berkshire-2-ca-version-2.expressnetw.com 1195";
    uk-berkshire = "uk-berkshire-ca-version-2.expressnetw.com 1195";
    uk-docklands = "uk-docklands-ca-version-2.expressnetw.com 1195";
    uk-eastlondon = "uk-east-london-ca-version-2.expressnetw.com 1195";
    uk-kent = "uk-kent-ca-version-2.expressnetw.com 1195";
    uk-london = "uk-london-ca-version-2.expressnetw.com 1195";
    ukraine = "ukraine-ca-version-2.expressnetw.com 1195";
    uruguay = "uruguay-ca-version-2.expressnetw.com 1195";
    usa-atlanta = "usa-atlanta-ca-version-2.expressnetw.com 1195";
    usa-boston = "usa-boston-ca-version-2.expressnetw.com 1195";
    usa-chicago = "usa-chicago-ca-version-2.expressnetw.com 1195";
    usa-dallas-2 = "usa-dallas-2-ca-version-2.expressnetw.com 1195";
    usa-dallas = "usa-dallas-ca-version-2.expressnetw.com 1195";
    usa-denver = "usa-denver-ca-version-2.expressnetw.com 1195";
    usa-kansascity = "usa-kansascity-ca-version-2.expressnetw.com 1195";
    usa-losangeles-1 = "usa-losangeles-1-ca-version-2.expressnetw.com 1195";
    usa-losangeles-2 = "usa-losangeles-2-ca-version-2.expressnetw.com 1195";
    usa-losangeles-3 = "usa-losangeles-3-ca-version-2.expressnetw.com 1195";
    usa-losangeles = "usa-losangeles-ca-version-2.expressnetw.com 1195";
    usa-miami-2 = "usa-miami-2-ca-version-2.expressnetw.com 1195";
    usa-miami = "usa-miami-ca-version-2.expressnetw.com 1195";
    usa-minneapolis = "usa-minneapolis-ca-version-2.expressnetw.com 1195";
    usa-newjersey-1 = "usa-newjersey-1-ca-version-2.expressnetw.com 1195";
    usa-newjersey-3 = "usa-newjersey-3-ca-version-2.expressnetw.com 1195";
    usa-newyork-2 = "us-new-york-2-ca-version-2.expressnetw.com 1195";
    usa-newyork = "usa-newyork-ca-version-2.expressnetw.com 1195";
    usa-phoenix = "usa-phoenix-ca-version-2.expressnetw.com 1195";
    usa-saltlakecity = "usa-saltlakecity-ca-version-2.expressnetw.com 1195";
    usa-sanfrancisco = "usa-sanfrancisco-ca-version-2.expressnetw.com 1195";
    usa-sanjose = "usa-sanjose-ca-version-2.expressnetw.com 1195";
    usa-seattle = "usa-seattle-ca-version-2.expressnetw.com 1195";
    usa-tampa-1 = "usa-tampa-1-ca-version-2.expressnetw.com 1195";
    usa-virginia = "usa-virginia-ca-version-2.expressnetw.com 1195";
    usa-washingtondc-2 = "usa-washingtondc-2-ca-version-2.expressnetw.com 1195";
    usa-washingtondc = "usa-washingtondc-ca-version-2.expressnetw.com 1195";
    venezuela = "venezuela-ca-version-2.expressnetw.com 1195";
    vietnam = "vietnam-ca-version-2.expressnetw.com 1195";
  };

  remoteConfig = name: remote: pkgs.lib.nameValuePair ("client-expressvpn-" + name) (generateOpenVPNConfig remote);

  generateOpenVPNConfig = remote: {
    autoStart        = false;
    updateResolvConf = true;

    config = builtins.readFile (pkgs.substituteAll {
      src = ./config.ovpn;

      inherit remote;

      # Go to https://www.expressvpn.com/setup#manual
      # 1) copy the username and password given to you in step 1 and write
      #    them, each on a new line, in the auth.txt file below.
      # 2) Download the ZIP file from step 4 and make them available to cert,
      #    key, tls-auth and ca below.
      inherit (cfg) auth_user_pass ca client_cert client_key tls_auth;
    });
  };

in {
  options.mine.openvpn.client.expressvpn = {
    enable = mkEnableOption "Enable ExpressionVPN client configuration";

    # TODO(low): options must be camelcase

    auth_user_pass = mkOption {
      type = types.path;
      defaultText = ''
        Path to the file containing the username and password.
      '';
    };

    ca = mkOption {
      type = types.path;
      defaultText = ''
        Path to the certificate authority.
      '';
    };

    client_cert = mkOption {
      type = types.path;
      defaultText = ''
        Path to the certificate
      '';
    };

    client_key = mkOption {
      type = types.path;
      defaultText = ''
        Path to the private key
      '';
    };

    tls_auth = mkOption {
      type = types.path;
      defaultText = ''
        Path to the private TLS auth key
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.auth_user_pass != null;
        message = "auth_user_pass is required";
      }
      {
        assertion = builtins.pathExists cfg.auth_user_pass;
        message = "auth_user_pass must exist";
      }

      {
        assertion = cfg.ca != null;
        message = "ca is required";
      }
      {
        assertion = builtins.pathExists cfg.ca;
        message = "ca must exist";
      }

      {
        assertion = cfg.client_cert != null;
        message = "client_cert is required";
      }
      {
        assertion = builtins.pathExists cfg.client_cert;
        message = "client_cert must exist";
      }

      {
        assertion = cfg.client_key != null;
        message = "client_key is required";
      }
      {
        assertion = builtins.pathExists cfg.client_key;
        message = "client_key must exist";
      }

      {
        assertion = cfg.tls_auth != null;
        message = "tls_auth is required";
      }
      {
        assertion = builtins.pathExists cfg.tls_auth;
        message = "tls_auth must exist";
      }
    ];

    services.openvpn.servers = pkgs.lib.mapAttrs' remoteConfig remotes;
  };
}
