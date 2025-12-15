{
  config,
  pkgs,
  lib,
  inputs,
  shares,
  ...
}:
let
  nixvirt = inputs.NixVirt;
  nixvirtLib = nixvirt.lib;

  poolName = "nixvirt-ubuntu";
  poolUuid = "62e37c8e-be0a-4cec-aab2-119dd64a1cfd";
  poolDir = "/var/lib/libvirt/images/nixvirt";

  networkName = "nixvirt";
  networkUuid = "7c866405-6284-4d83-a26b-c375e760fe29";
  bridgeName = "virbr10";

  domainName = "ubuntu-nixvirt";
  domainUuid = "30c69d8d-d505-4b8b-8971-04ff3d63e018";
  overlayName = "ubuntu-24.04-overlay.qcow2";

  ubuntuBaseImage = pkgs.fetchurl {
    url = "https://cloud-images.ubuntu.com/releases/24.04/release/ubuntu-24.04-server-cloudimg-amd64.img";
    hash = "sha256-b97AlveI0/s5Zo8hEUblq4L48LE7pSmkYiaryiK6W88=";
  };

  sshAuthorizedKeys =
    [
      shares.users-dict."freeman.xiong".public-key
      shares.users-dict."freeman.xiong".yubikey
    ];

  cloudInitUserData = pkgs.writeText "ubuntu-nixvirt-user-data" ''
#cloud-config
ssh_pwauth: false
users:
  - name: freeman
    sudo: ALL=(ALL) NOPASSWD:ALL
    groups: [ adm, sudo ]
    shell: /bin/bash
    ssh_authorized_keys:
${lib.concatMapStrings (key: "      - ${key}\n") sshAuthorizedKeys}
package_update: true
package_upgrade: false
packages:
  - qemu-guest-agent
runcmd:
  - [ systemctl, enable, --now, qemu-guest-agent ]
'';

  cloudInitMetaData = pkgs.writeText "ubuntu-nixvirt-meta-data" ''
instance-id: ubuntu-nixvirt
local-hostname: ubuntu-nixvirt
'';

  # Seed image for cloud-init, generated reproducibly from the keys above.
  cloudInitIso = pkgs.runCommand "ubuntu-nixvirt-seed.iso" { buildInputs = [ pkgs.cloud-utils ]; } ''
    cloud-localds $out ${cloudInitUserData} ${cloudInitMetaData}
  '';
in
{
  imports = [ nixvirt.nixosModules.default ];

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

  systemd.tmpfiles.rules = [
    "d ${poolDir} 0755 root root -"
  ];

  virtualisation = {
    spiceUSBRedirection.enable = true;
    libvirt = {
      enable = true;
      verbose = true;
      package = pkgs.libvirt;
      connections."qemu:///system" = {
        networks = [
          {
            definition = nixvirtLib.network.writeXML (nixvirtLib.network.templates.bridge {
              name = networkName;
              uuid = networkUuid;
              bridge_name = bridgeName;
              subnet_byte = 85;
            });
            active = true;
            restart = true;
          }
        ];
        pools = [
          {
            definition = nixvirtLib.pool.writeXML {
              name = poolName;
              uuid = poolUuid;
              type = "dir";
              target = { path = poolDir; };
            };
            active = true;
            restart = true;
            volumes = [
              {
                definition = nixvirtLib.volume.writeXML {
                  name = overlayName;
                  capacity = { count = 40; unit = "GiB"; };
                  target = { format = { type = "qcow2"; }; };
                  backingStore = {
                    path = toString ubuntuBaseImage;
                    format = { type = "qcow2"; };
                  };
                };
              }
            ];
          }
        ];
        domains = [
          {
            definition = nixvirtLib.domain.writeXML (nixvirtLib.domain.templates.linux {
              name = domainName;
              uuid = domainUuid;
              vcpu = { count = 4; };
              memory = { count = 8; unit = "GiB"; };
              storage_vol = { pool = poolName; volume = overlayName; };
              backing_vol = null;
              install_vol = cloudInitIso;
              bridge_name = bridgeName;
              virtio_drive = true;
              virtio_video = true;
            });
            active = true;
            restart = true;
          }
        ];
      };
    };

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
      allowedBridges = [ bridgeName "virbr0" ];
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
