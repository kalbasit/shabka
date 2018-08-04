self: super:

{
  haskellPackages = super.haskellPackages.override {
    overrides = hsSelf: hsSuper: {
      greenclip  = self.haskell.lib.overrideCabal hsSuper.greenclip  (oa: {
        version = "3.1.1";
        sha256 = "1axh1q7kcvcnhn4rl704i4gcix5yn5v0sb3bdgjk4vgkd7fv8chw";
        executablePkgconfigDepends = oa.executablePkgconfigDepends ++ [super.xorg.libXdmcp];
      });

      wordexp  = self.haskell.lib.overrideCabal hsSuper.wordexp  (oa: {
        version = "0.2.2";
        sha256 = "1mbcrq89jz0dcibw66w0jdy4f4bfpx4zwjfs98rm3jjgdikwdzb4";
      });
    };
  };
}
