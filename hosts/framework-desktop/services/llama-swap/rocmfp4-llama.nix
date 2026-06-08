{ pkgs ? import <nixpkgs> { config = { allowUnfree = true; rocmSupport = true; }; } }:

let
  src = pkgs.fetchFromGitHub {
    owner = "charlie12345";
    repo = "rocmfp4-llama";
    rev = "c5bfb0d8db45bf81e7bd67e95d83f18a6d5ecc76";
    hash = "sha256-8vuxlYf3yjaZE/kiTsLGBMH9SHKywdaeIi5gHjWX8Ds="; # Note: Run nix-build to get actual hash
  };

  rocmBuildInputs = with pkgs.rocmPackages; [
    clr
    hipblas
    rocblas
  ];

  vulkanBuildInputs = with pkgs; [
    vulkan-headers
    vulkan-loader
    shaderc
    spirv-headers
  ];

in
pkgs.stdenv.mkDerivation {
  pname = "rocmfp4-llama-strix";
  version = "0.0.0";

  inherit src;

  nativeBuildInputs = [
    pkgs.cmake
    pkgs.ninja
    pkgs.pkg-config
    pkgs.git
  ];

  buildInputs = rocmBuildInputs ++ vulkanBuildInputs ++ [ pkgs.openssl ];

  # Set up the essential ROCm environment from .devops/nix/package.nix
  ROCM_PATH = "${pkgs.rocmPackages.clr}";
  HIP_DEVICE_LIB_PATH = "${pkgs.rocmPackages.rocm-device-libs}/amdgcn/bitcode";

  # The original derivation defines the CMAKE_HIP_COMPILER flag;
  # Since we are using the script directly, we inject it into the environment
  # so that if cmake needs it, it can potentially find the clang binaries.
  CC = "${pkgs.rocmPackages.llvm.clang}/bin/clang";
  CXX = "${pkgs.rocmPackages.llvm.clang}/bin/clang++";

  # Disable default configurePhase so we stay in the source root
  dontConfigure = true;

  # We overwrite the build/install phases to run the script literally
  buildPhase = ''

    # Make sure we use the script's intended parallelism and patch it for the nix sandbox
    patchShebangs scripts/build-strix-rocmfp4-mtp.sh
    export JOBS=$NIX_BUILD_CORES

    # We must append CMAKE_HIP_COMPILER manually here just in case because
    # scripts/build-strix-rocmfp4-mtp.sh doesn't explicitly pass it like package.nix does.
    sed -i 's|cmake -S|cmake -DCMAKE_HIP_COMPILER=${pkgs.rocmPackages.llvm.clang}/bin/clang -DBUILD_SHARED_LIBS=OFF -S|' scripts/build-strix-rocmfp4-mtp.sh

    bash ./scripts/build-strix-rocmfp4-mtp.sh
  '';

  installPhase = ''
    mkdir -p $out/bin

    # The script builds to build-strix-rocmfp4/bin/
    cp build-strix-rocmfp4/bin/llama-cli $out/bin/
    cp build-strix-rocmfp4/bin/llama-server $out/bin/
    cp build-strix-rocmfp4/bin/llama-completion $out/bin/
    cp build-strix-rocmfp4/bin/llama-quantize $out/bin/
    cp build-strix-rocmfp4/bin/llama-bench $out/bin/
    cp build-strix-rocmfp4/bin/test-backend-ops $out/bin/
    cp build-strix-rocmfp4/bin/test-quantize-fns $out/bin/
    cp build-strix-rocmfp4/bin/test-quantize-perf $out/bin/
  '';
}
