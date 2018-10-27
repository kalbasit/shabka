{
  network.description = "Network at home, including my VPN on EC2";

  zeus = {
    imports = [ ../hosts/zeus/configuration.nix ];
    deployment.targetHost = "zeus.home.nasreddine.com";
  };
}
