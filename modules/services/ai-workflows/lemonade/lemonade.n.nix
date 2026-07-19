{
  flake.modules.nixos.lemonade = { config, inputs, ... }: {
    imports = [ inputs.nix-amd-ai.nixosModules.default ];

    hardware.amd-npu = {
      enable = true;
      enableNPU = true;
      enableFastFlowLM = true; # LLM inference on NPU
      enableLemonade = false; # OpenAI-compatible API server
      enableROCm = false; # ROCm GPU backends (llamacpp + sd-cpp)
      enableVulkan = false; # Vulkan GPU backends (llamacpp + whispercpp)
      enableImageGen = false; # default true; set false to drop sd-cpp from closure
      

      lemonade = {
        user = "root";
        port = 13306;
        # Force lemonade to run purely on the NPU to preserve CPU/GPU for other workloads
      };
    };

    users.users.root.extraGroups = [ "video" "render" ];
  };
}
