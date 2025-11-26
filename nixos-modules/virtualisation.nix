{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    virt-viewer
    swtpm
    #virt-top
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
      extraOptions = [
        "--verbose"
      ];
      qemu = {
        package = pkgs.qemu_kvm;
        swtpm.enable = true;
        runAsRoot = true;
      };
    };

    kvmgt = {
      enable = true;
    };

    docker = {
      enable = true;
      enableNvidia = true;
      autoPrune = {
        enable = true;
        flags = [
          "--all"
          "--force"
        ];
      };
    };
    #podman = {
    #  enable = true;
    #  autoprune = {
    #    enable = true;
    #    flags = [
    #      "--all"
    #      "--force"
    #    ];
    #  };
    #  dockersocket.enable = true;
    #  defaultnetwork.settings.dns_enabled = true;
    #  dockerCompat = true;
    # networkSocket = {
    #   enable = true;
    #   server = "ghostunnel";
    # };
    #};
  };

  networking.firewall.checkReversePath = false;

  programs = {
    virt-manager = {
      enable = true;
    };
  };
}
