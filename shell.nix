{
  nixpkgs ? import <nixpkgs> {
    overlays = import ./overlays;
  }
}:

nixpkgs.mkShell {
  buildInputs = with nixpkgs; [
    gnumake
    nixops
    awscli
    (unstable.terraform_0_12.withPlugins (ps: [
      ps.aws
    ]))
  ];
}
