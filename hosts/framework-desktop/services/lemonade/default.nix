{inputs, ...}:

{
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
    };
  };

  users.users.root.extraGroups = ["video" "render"];
}
