{ pkgs, ... }: {
  virtualisation = {
    libvirtd = {
      enable = true;
      qemu.package = pkgs.qemu_kvm;
    };
    podman = {
      enable = true;
      dockerSocket.enable = true;
      defaultNetwork.settings.dns_enabled = true;
      dockerCompat = true;
      autoPrune = {
        enable = true;
        flags = [ "--all" "--force" ];
      };
    };
  };
  environment.systemPackages = with pkgs; [ virt-manager ];

  networking.firewall.checkReversePath = false;
}
