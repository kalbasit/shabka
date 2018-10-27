{ nixpkgs ? import <nixpkgs> {} }:

nixpkgs.mkShell {
  buildInputs = with nixpkgs; [
    gnumake
    nixops
  ];

  NIXOPS_STATE = "/yl/keybase/private/kalbasit/network/deployments.nixops";
}
