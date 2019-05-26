let
  lib = (import <nixpkgs> {}).lib;
  secrets = import ../secrets/network/work.nix;
in {
  network.description = "wael-network";
  network.enableRollback = true;

  resources.vpc.nixops = {
    inherit (secrets) accessKeyId cidrBlock region;

    enableDnsSupport = true;

    tags = {
      Source = "NixOps";
      Owner = "wael";
    };
  };

  resources.vpcSubnets =
    let
      subnet = {cidr, zone}:
        { resources, ... }:
        {
          inherit (secrets) accessKeyId region zone;
          vpcId = resources.vpc.nixops;
          cidrBlock = cidr;
          mapPublicIpOnLaunch = true;
          tags = {
            Source = "NixOps";
            Owner = "wael";
          };
        };
    in
    {
      subnet-a = subnet { inherit (secrets) zone; cidr = secrets.cidrBlock; };
    };

  resources.vpcRouteTables =
    {
      route-table =
        { resources, ... }:
        {
          inherit (secrets) region accessKeyId;
          vpcId = resources.vpc.nixops;
        };
    };

  resources.vpcRouteTableAssociations =
    let
      subnets = ["subnet-a"];
      association = subnet:
        { resources, ... }:
        {
          inherit (secrets) region accessKeyId;
          subnetId = resources.vpcSubnets."${subnet}";
          routeTableId = resources.vpcRouteTables.route-table;
        };
    in
      (builtins.listToAttrs (map (s: lib.nameValuePair "association-${s}" (association s) ) subnets));

  resources.vpcRoutes = {
    igw-route =
      { resources, ... }:
      {
        inherit (secrets) region accessKeyId;
        routeTableId = resources.vpcRouteTables.route-table;
        destinationCidrBlock = "0.0.0.0/0";
        gatewayId = resources.vpcInternetGateways.igw;
      };
  };

  resources.vpcInternetGateways.igw =
    { resources, ... }:
    {
      inherit (secrets) region accessKeyId;
      vpcId = resources.vpc.nixops;
    };

  resources.ec2SecurityGroups = {
    ssh-in = { resources, ... }: {
      inherit (secrets) accessKeyId region;
      vpcId = resources.vpc.nixops;
      description = "Allow incoming SSH connection from anywhere";
      rules = [
        {fromPort = 22; toPort = 22; protocol = "tcp"; sourceIp = "0.0.0.0/0"; }
        # TODO(low): https://github.com/NixOS/nixops/issues/683
        # {fromPort = 22; toPort = 22; protocol = "tcp"; sourceIp = "::/0"; }
      ];
    };

    mosh-in = { resources, ... }: {
      inherit (secrets) accessKeyId region;
      vpcId = resources.vpc.nixops;
      description = "Allow incoming MOSH connection from anywhere";
      rules = [
        {fromPort = 60001; toPort = 60051; protocol = "udp"; sourceIp = "0.0.0.0/0"; }
      ];
    };
  };

  demeter = { resources, ... }: {
    imports = [ ../hosts/demeter/configuration.nix ];
    deployment = {
      targetEnv = "ec2";

      ec2 = {
        inherit (secrets) accessKeyId ami keyPair region;

        subnetId = resources.vpcSubnets.subnet-a;

        instanceType = "t3.2xlarge";
        ebsInitialRootDiskSize = 1024;
        associatePublicIpAddress = true;

        securityGroupIds = [
          resources.ec2SecurityGroups.ssh-in.name
          resources.ec2SecurityGroups.mosh-in.name
        ];

        tags = {
          Source = "NixOps";
          Owner = "wael";
        };
      };

      route53 = {
        inherit (secrets) accessKeyId hostName;
        ttl = 300;
      };
    };
  };
}
