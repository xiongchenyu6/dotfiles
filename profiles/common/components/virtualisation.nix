{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    virt-viewer
    swtpm
  ];
  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
    options kvm_intel emulate_invalid_guest_state=0
    options kvm ignore_msrs=1
  '';

  virtualisation = {
    spiceUSBRedirection.enable = true;
    libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        swtpm.enable = true;
        runAsRoot = true;
        ovmf = {
          enable = true;
          packages = [
            (pkgs.OVMFFull.override {
              secureBoot = true;
              tpmSupport = true;
            }).fd
          ];
        };
      };
    };

    kvmgt = {
      enable = true;
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
