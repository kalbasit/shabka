{ nixpkgs ? import <nixpkgs> {} }:

nixpkgs.mkShell {
  buildInputs = with nixpkgs; [
    gnumake
    nixops
    awscli
    (terraform.withPlugins (ps: [
      ps.aws
    ]))
  ];

  NIXOPS_STATE = "/yl/keybase/private/ylcodes/system/deployments.nixops";
}
