{ ... }:

{
  services.snapper = {
    configs = {
      "code" = {
        subvolume = "/code";
      };

      "home" = {
        subvolume = "/home";
      };

      "private" = {
        subvolume = "/private";
      };
    };
  };
}
