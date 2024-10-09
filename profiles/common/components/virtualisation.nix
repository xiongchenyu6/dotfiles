{ pkgs, ... }: {
  virtualisation = {
    libvirtd = {
      enable = true;
      qemu.package = pkgs.qemu_kvm;
    };
    # docker = {
    #   enable = true;
    #   autoPrune = {
    #     enable = true;
    #     flags = [ "--all" "--force" ];
    #   };
    # };
    podman = {
      enable = true;
      autoPrune = {
        enable = true;
        flags = [ "--all" "--force" ];
      };
      dockerSocket.enable = true;
      defaultNetwork.settings.dns_enabled = true;
      dockerCompat = true;
      # networkSocket = {
      #   enable = true;
      #   server = "ghostunnel";
      # };
     };
  };
  environment.systemPackages = with pkgs; [ virt-manager ];

  networking.firewall.checkReversePath = false;
}
