{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.mine.workstation.keeptruckin;

  dynamodb = with pkgs; stdenvNoCC.mkDerivation rec {
    name = "${pname}-${version}";
    pname = "dynamodb";
    version = "2018-04-11";

    preferLocalBuild = true;

    src = fetchurl {
      url = "https://s3-us-west-2.amazonaws.com/dynamodb-local/dynamodb_local_${version}.tar.gz";
      sha256 = "0qipwpichajv6dwlwa4jbzh3qs5nqvi5l6zrbm9f6mkj2maf9yja";
    };

    sourceRoot = ".";

    buildInputs = [ makeWrapper ];

    installPhase = ''
      mkdir -p $out/share/java/${pname}/DynamoDBLocal_lib

      install -Dm644 DynamoDBLocal.jar $out/share/java/${pname}/DynamoDBLocal.jar
      install -Dm644 DynamoDBLocal_lib/*.jar $out/share/java/${pname}/DynamoDBLocal_lib

      makeWrapper ${jre}/bin/java $out/bin/${pname} \
        --add-flags "-Djava.library.path=$out/share/java/${pname}/DynamoDBLocal_lib -jar $out/share/java/${pname}/DynamoDBLocal.jar -sharedDb -inMemory true"
    '';

  };

in {
  options.mine.workstation.keeptruckin = {
    enable = mkEnableOption "Enable KeepTruckin";
  };

  config = mkIf cfg.enable {
    services.redis.enable = true;

    services.postgresql = {
      enable = true;
      initialScript = pkgs.writeScript "setup-kt-user.sql" ''
        CREATE ROLE k2_developer PASSWORD 'wowthispasswordisbad' SUPERUSER CREATEDB CREATEROLE INHERIT LOGIN;
      '';
    };

    systemd.services.dynamodb = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig.ExecStart = "${getBin dynamodb}/bin/dynamodb";
    };
  };
}
