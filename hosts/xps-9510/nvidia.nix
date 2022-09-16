{ config, lib, pkgs, ... }:

let
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in
{
  #################################
  # NVIDIA GPU ON
  #################################
  environment.systemPackages = [ nvidia-offload ];

  hardware.nvidia.prime = {
    offload.enable = true;
    # sync.enable = true;
    intelBusId = lib.mkDefault "PCI:0:2:0";
    nvidiaBusId = lib.mkDefault "PCI:1:0:0";
  };
  hardware.nvidia.modesetting.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  #################################
  # NVIDIA GPU OFF
  #################################
  # hardware.nvidiaOptimus.disable = true;
  # boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];
  # services.xserver.videoDrivers = [ "intel" ];
  #services.udev.extraRules = with pkgs; ''
  #  #Remove NVIDIA USB xHCI Host Controller Devices, if present
  #  ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x0c0330", ATTR{remove}=="1"
  #  #Remove NVIDIA USB Type-C UCSI devices, if present
  #  ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de" , ATTR{class}=="0x0c8000", ATTR{remove}=="1"
  #  #Remove NVIDIA Audio Devices
  #  ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x040300", ATTR{remove}=="1"
  #  #enable pci port kernel power management
  #  SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x030000", ATTR{power/control}=="auto"
  #  SUBSYSTEM=="pci", ATTR{vendor}=="0x10de", ATTR{class}=="0x030200", ATTR{power/control}=="auto"
  #'';
}
