{ config, pkgs, lib, inputs, ... }:

{
  # Add vast-cli for GPU rental management (provides vastai binary)
  environment.systemPackages = with pkgs; [
    inputs.vast-cli.packages.${pkgs.system}.default
  ];

  # Optional: Create an alias for convenience
  # The binary is actually called 'vastai'
  environment.shellAliases = {
    vast = "vastai";
  };
}