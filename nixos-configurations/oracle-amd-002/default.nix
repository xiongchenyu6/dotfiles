{
  inputs,
  modulesPath,
  lib,
  pkgs,
  config,
  ezModules,
  ...
}:
{
  imports = with inputs; [
    disko.nixosModules.disko
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.server
    ezModules.acme
    ezModules.datadog-agent
    ezModules.sing-box
    srvos.nixosModules.server
    srvos.nixosModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    ./hardware-configuration.nix
  ];
  # security.wrappers.kanidm_ssh_authorizedkeys = {
  #   owner = "root";
  #   group = "root";
  #   source = "${pkgs.kanidm}/bin/kanidm_ssh_authorizedkeys";
  # };

  services = {
    # kanidm = {
    #   enablePam = true;
    #   clientSettings = {
    #     uri = "https://kanidm.${config.networking.domain}";
    #   };
    #   unixSettings = {
    #     default_shell = "${pkgs.zsh}/bin/zsh";
    #     home_alias = "name";
    #     home_attr = "uuid";
    #     pam_allowed_login_groups = [ "devops" ]; # Updated group to match changes in groups
    #     home_mount_prefix = "/run/kanidm:/run/kanidm";
    #   };
    # };

    openssh = {
      enable = true;
      authorizedKeysCommand = "/run/wrappers/bin/kanidm_ssh_authorizedkeys %u";
      authorizedKeysCommandUser = "nobody";
      settings = {
        UsePAM = true;
      };
    };
  };
}
