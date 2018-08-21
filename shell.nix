{ nixpkgs ? import ./external/nixpkgs {} }:

nixpkgs.mkShell {
  buildInputs = with nixpkgs; [
    gnumake
    sudo
  ];
}
