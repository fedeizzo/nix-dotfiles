{
  flake.modules.nixos.lemonade = { config, inputs, ... }: {
    imports = [inputs.nix-amd-ai.nixosModules.default];

    hardware.amd-npu = {
      enable = true;
      enableFastFlowLM = true;  # LLM inference on NPU
      enableLemonade = true;    # OpenAI-compatible API server
      enableROCm = true;        # ROCm GPU backends (llamacpp + sd-cpp)
      enableVulkan = true;      # Vulkan GPU backends (llamacpp + whispercpp)
      enableImageGen = true;    # default true; set false to drop sd-cpp from closure

      lemonade = {

        user = "root";
        port = 13306;
        # Force lemonade to run purely on the NPU to preserve CPU/GPU for other workloads



      };
    };

    users.users.root.extraGroups = ["video" "render"];

    fi.services = [
      {
        name = "lemonade";
        port = config.hardware.amd-npu.lemonade.port;
        dashboardSection = "Tools";
        toPersist = [ ];
        toBackup = [
          "/root/.cache/lemonade"
        ];
      }
    ];
  };
}
