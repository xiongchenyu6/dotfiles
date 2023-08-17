# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }: {
  users = {
    users = {
      xiongchenyu = {
        # name = "freeman.xiong";
        description = "freeman";
        shell = pkgs.zsh;
      };
    };
  };
  home-manager = {
    users = {
      xiongchenyu = {
        imports = [
          ../profiles/gui/darwin.nix
          ../profiles/cli/common.nix
          ../profiles/cli/shell/zsh.nix
        ];
      };
      root = {
        programs = {
          ssh = {
            matchBlocks = { "*.trontech.link" = { user = "freeman.xiong"; }; };
          };
        };
      };
    };
  };
}
