{ ... }:

{
  buildNixDarwinConfiguration = conf: (import <darwin> {
    configuration = conf;
  }).system;
}
