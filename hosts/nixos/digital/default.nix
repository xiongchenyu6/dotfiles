{
  modulesPath,
  profiles,
  config,
  lib,
  pkgs,
  ...
}:
{

  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./hardware-configuration.nix
    ../../../users/root/nixos.nix
    ../../../users/freeman.xiong
    ../../../profiles/sops.nix
    ../../../profiles/core/nixos.nix
    ../../../profiles/server/components
    ../../../profiles/common/components
    # ../../../profiles/common/components/datadog-agent.nix
    # ../../../profiles/server/apps/proxy/nginx.nix
  ];

  sops.secrets."wireguard/digital" = { };
  sops.secrets."authentik/env" = { };
  boot.loader.grub = {
    enable = true;
    # no need to set devices, disko will add all devices that have a EF02 partition to the list already
    devices = [ "/dev/vda" ];
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  networking =
    let
      file-path = builtins.split "/" (toString ./.);
      hostName = lib.last file-path;
    in
    {
      useDHCP = lib.mkForce false;
      inherit hostName;
      firewall = {
        allowedTCPPorts = [
          22
          89
          179
          2222
        ];
        allowedUDPPorts = [
          89
          179
          2222
          6696
          33434
        ];
      };
    };
  services = {
    openssh = {
      openFirewall = true;
    };
    do-agent = {
      enable = true;
    };
    # redis = {
    #   servers.authentik = {
    #     enable = lib.mkForce false;
    #   };
    # };

    # authentik = {
    #   enable = true;
    #   createDatabase = false;
    #   # The environmentFile needs to be on the target host!
    #   # Best use something like sops-nix or agenix to manage it
    #   environmentFile = config.sops.secrets."authentik/env".path;
    #   settings = {
    #     disable_startup_analytics = true;
    #     avatars = "initials";
    #   };
    # };
  };
  home-manager = {
    users = {
      "freeman.xiong" = {
        imports = [
          ../../../users/profiles/cli/shell/zsh/common.nix
          ../../../users/profiles/cli/common.nix
          ../../../users/profiles/cli/tmux.nix
        ];
      };
    };
  };
}
