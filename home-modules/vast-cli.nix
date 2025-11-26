{ config, pkgs, lib, inputs, ... }:

with lib;

let
  cfg = config.programs.vast-cli;
in
{
  options.programs.vast-cli = {
    enable = mkEnableOption "vast-cli - CLI tool for managing Vast.AI instances";

    package = mkOption {
      type = types.package;
      default = inputs.vast-cli.packages.${pkgs.system}.default;
      description = "The vast-cli package to use.";
    };

    sshConfig = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable SSH config generation for Vast.AI instances.";
      };

      apiKeyFile = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = ''
          Path to file containing the Vast.AI API key.
          If null, the script will look for the key in standard locations.
        '';
      };

      includeFile = mkOption {
        type = types.str;
        default = "~/.ssh/vast-instances";
        description = "Path to the generated SSH config file for Vast.AI instances.";
      };

      package = mkOption {
        type = types.package;
        default = inputs.vast-cli.packages.${pkgs.system}.generate-ssh-config;
        description = "The generate-ssh-config package to use.";
      };
    };
  };

  config = mkIf cfg.enable {
    # Install vast-cli package
    home.packages = [ cfg.package ]
      ++ (lib.optional cfg.sshConfig.enable cfg.sshConfig.package);

    # Add SSH config include if SSH is configured
    programs.ssh = mkIf (cfg.sshConfig.enable && config.programs.ssh.enable) {
      includes = [ cfg.sshConfig.includeFile ];
    };

    # Optional: Create an alias for convenience
    # The binary is actually called 'vastai'
    programs.zsh.shellAliases = mkIf config.programs.zsh.enable {
      vast = "vastai";
    };

    programs.bash.shellAliases = mkIf config.programs.bash.enable {
      vast = "vastai";
    };
  };
}