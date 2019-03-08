self: super:

{
  # TODO: shabka should be a global object, just like pkgs
  shabka.external = super.callPackage ../external { };
}
