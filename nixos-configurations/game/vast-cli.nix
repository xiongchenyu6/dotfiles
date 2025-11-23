{ config, pkgs, lib, inputs, ... }:

{
  # Add vast-cli for GPU rental management
  environment.systemPackages = with pkgs; [
    inputs.vast-cli.packages.${pkgs.system}.default
  ];

  # Optional: Create an alias for convenience
  environment.shellAliases = {
    vast = "vast-cli";
  };
}