{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (curl.override {
      brotliSupport = true;
    })
  ];
}
