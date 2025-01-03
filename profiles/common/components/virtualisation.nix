{ pkgs, ... }:
{
  virtualisation = {
    spiceUSBRedirection.enable = true;
    libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        swtpm.enable = true;
        ovmf = {
          enable = true;
          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            }).fd
          ];
        };
      };
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
        flags = [
          "--all"
          "--force"
        ];
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

  networking.firewall.checkReversePath = false;

  programs = {
    virt-manager = {
      enable = true;
    };
  };
}
