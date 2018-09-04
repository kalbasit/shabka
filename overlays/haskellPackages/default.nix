self: super:

{
  haskellPackages = super.haskellPackages.override {
    overrides = hsSelf: hsSuper: {
      greenclip  = self.haskell.lib.overrideCabal hsSuper.greenclip  (oa: {
        version = "3.2.0";
        sha256 = "09ygvyrczxqsp2plwmwx021wmbq2vln9i4b5iaj0j26j7prykikq";
        executablePkgconfigDepends = oa.executablePkgconfigDepends ++ [super.xorg.libXdmcp];
      });

      wordexp  = self.haskell.lib.overrideCabal hsSuper.wordexp  (oa: {
        version = "0.2.2";
        sha256 = "1mbcrq89jz0dcibw66w0jdy4f4bfpx4zwjfs98rm3jjgdikwdzb4";
      });
    };
  };
}
