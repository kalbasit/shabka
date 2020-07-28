{ config, pkgs, lib, ... }:

with lib;

{
  config = mkIf (config.shabka.hardware.machine == "thinkpad-p1") {
    boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ "kvm-intel" ];
    boot.extraModulePackages = [ ];

    boot.loader.grub = {
      configurationLimit = 30;
      device = "nodev";
      efiSupport = true;
      enable = true;
      enableCryptodisk = true;
    };

    # boot.loader.efi.canTouchEfiVariables = true;

    boot.loader.systemd-boot.enable = false;

    nix.maxJobs = lib.mkDefault 8;

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

    # services.xserver.videoDrivers = lib.mkForce ["modesetting"];

    boot.kernelPackages = pkgs.linuxPackages_latest;

    # HiDPI settings
    console.font = "${pkgs.terminus_font}/share/consolefonts/ter-v32n.psf.gz";
    console.earlySetup = true; # Needed when typing in passwords for full disk encryption
    fonts.fontconfig.dpi = 196;
    services.xserver.dpi = 196;
    services.xserver.synaptics.minSpeed = "1.0";
    services.xserver.synaptics.maxSpeed = "1.5";
  };
}
