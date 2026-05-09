{
  inputs,
  ezModules,
  pkgs,
  lib,
  ...
}:
let
  sharedConfig = import ../shared-modules/default.nix { inherit inputs lib; };

  nixos-modules = with inputs; [
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    nur.modules.nixos.default
    impermanence.nixosModules.impermanence
    nix-topology.nixosModules.default
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    (sharedConfig.mkNixosNixpkgsConfig sharedConfig.nixosOverlays)
  ];
in
{
  imports = [
    (
      {
        config,
        inputs,
        lib,
        ...
      }:
      {
        disabledModules = [
          "${inputs.srvos}/nixos/common"
          "${inputs.srvos}/nixos/common/default.nix"
        ];

        imports = [
          (inputs.srvos + "/shared/common/flake.nix")
          (inputs.srvos + "/nixos/common/detect-hostname-change.nix")
          (inputs.srvos + "/nixos/common/networking.nix")
          (inputs.srvos + "/nixos/common/nix.nix")
          (inputs.srvos + "/nixos/common/openssh.nix")
          (inputs.srvos + "/nixos/common/serial.nix")
          (inputs.srvos + "/nixos/common/sudo.nix")
          (inputs.srvos + "/nixos/common/update-diff.nix")
          (inputs.srvos + "/shared/common/well-known-hosts.nix")
          (inputs.srvos + "/nixos/common/zfs.nix")
        ];

        services.userborn.enable = lib.mkIf (
          !(
            (config.environment ? persistence && config.environment.persistence != { })
            || (lib.any (u: u.subUidRanges != [ ] || u.autoSubUidGidRange) (lib.attrValues config.users.users))
          )
        ) (lib.mkDefault true);

        boot.initrd.systemd.enable = lib.mkDefault (!config.boot.swraid.enable && !config.boot.isContainer);
        environment.ldso32 = null;
        boot.tmp.cleanOnBoot = lib.mkDefault true;
      }
    )
    ezModules.kernel
    ezModules.security
    ezModules.ssh-harden
    ../shared-modules/core.nix
    ../shared-modules/sops.nix
  ]
  ++ nixos-modules;

  home-manager = sharedConfig.homeManagerConfig;
  documentation.info.enable = false;
  zramSwap.enable = true;
  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
  };
  environment = {
    systemPackages = with pkgs; [
      lrzsz
    ];
  };
  services = {
    resolved = {
      enable = true;
      # dnssec = "allow-downgrade";
      # dnsovertls = "opportunistic";
      # llmnr = "false";
    };

  };
  networking.nftables.enable = true;
}
