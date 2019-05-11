let
  secrets = import /yl/private/network-secrets/shabka/network/home.nix;
in {
  network.description = "Network at work";
  network.enableRollback = true;

  resources = {
    ec2SecurityGroups = {
      "ssh-in" = {
        inherit (secrets) accessKeyId region;
        description = "Allow incoming SSH connection from anywhere";
        rules = [
          {fromPort = 22; toPort = 22; protocol = "tcp"; sourceIp = "0.0.0.0/0"; }
          # TODO(low): https://github.com/NixOS/nixops/issues/683
          # {fromPort = 22; toPort = 22; protocol = "tcp"; sourceIp = "::/0"; }
        ];
      };
    };
  };

  demeter = { resources, ... }: {
    imports = [ ../hosts/demeter /configuration.nix ];
    deployment = {
      targetEnv = "ec2";
      ec2 = {
        inherit (secrets) accessKeyId region keyPair ami;

        instanceType = "t3.2xlarge";

        securityGroups = [
          resources.ec2SecurityGroups.ssh-in
        ];
      };
    };
  };
}
