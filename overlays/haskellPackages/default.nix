self: super:

{
  # TODO: fix the compilation of haskellPackages.greenclip
  haskellPackages = super.haskellPackages.override {
    overrides = hsSelf: hsSuper: {
      wordexp  = self.haskell.lib.overrideCabal hsSuper.wordexp  (oa: {
        version = "0.2.2";
        sha256 = "1mbcrq89jz0dcibw66w0jdy4f4bfpx4zwjfs98rm3jjgdikwdzb4";
      });
    };
  };
}
