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
  wifiInterface = "wlo1";
  macvtapMac = null; # let libvirt choose a MAC for macvtap

  domainName = "ubuntu-nixvirt";
  domainUuid = "30c69d8d-d505-4b8b-8971-04ff3d63e018";
  overlayName = "ubuntu-24.04-overlay-v6.qcow2";

  ubuntuBaseImage = pkgs.fetchurl {
    # Canonical refreshes this same URL with newer builds; whenever the daily
    # image rotates the sha256 changes. Bump via:
    #   nix-prefetch-url --type sha256 \
    #     https://cloud-images.ubuntu.com/releases/24.04/release/ubuntu-24.04-server-cloudimg-amd64.img \
    #     | xargs nix hash to-sri --type sha256
    url = "https://cloud-images.ubuntu.com/releases/24.04/release/ubuntu-24.04-server-cloudimg-amd64.img";
    hash = "sha256-XD3bAPYLxFXawIYvq+nYus7EbDOsF1EUPFw2g0BLEQ0=";
  };

  sshAuthorizedKeys = [
    shares.users-dict."freeman.xiong".public-key
    shares.users-dict."freeman.xiong".yubikey
  ];

  # TODO: The passwd hash below ends up in /nix/store (world-readable).
  # Consider generating cloud-init user-data at runtime via sops template
  # if stronger isolation is needed for the VM password.
  cloudInitUserData = pkgs.writeText "ubuntu-nixvirt-user-data" ''
    #cloud-config
    ssh_pwauth: true
    users:
      - name: freeman
        sudo: ALL=(ALL) NOPASSWD:ALL
        groups: [ adm, sudo ]
        shell: /bin/bash
        lock_passwd: false
        passwd: "$6$VNrS5PBI8A6yrY2c$zWY9N42NDkwLoYnadeU028n0nuXtfv/zRYgfoT9i1MAPPM7HC5v2Fz59UmVDZYhpp3KAK3gLB3kiqjh4hMh5K0"
        ssh_authorized_keys:
    ${lib.concatMapStrings (key: "      - ${key}\n") sshAuthorizedKeys}
    chpasswd:
      expire: false
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

  cloudInitNetworkConfig = pkgs.writeText "ubuntu-nixvirt-network-config" ''
    version: 2
    ethernets:
      eth0:
        match:
          macaddress: 52:54:00:7d:ce:fb
        set-name: enp1s0
        dhcp4: true
        optional: true
  '';

  # Seed image for cloud-init, generated reproducibly from the keys above.
  cloudInitIso = pkgs.runCommand "ubuntu-nixvirt-seed.iso" { buildInputs = [ pkgs.cloud-utils ]; } ''
    cloud-localds --network-config=${cloudInitNetworkConfig} $out ${cloudInitUserData} ${cloudInitMetaData}
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
    options kvm_amd nested=1
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
            definition = nixvirtLib.network.writeXML (
              nixvirtLib.network.templates.bridge {
                name = networkName;
                uuid = networkUuid;
                bridge_name = bridgeName;
                subnet_byte = 85;
                dhcp_hosts = [ ];
              }
            );
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
              target = {
                path = poolDir;
              };
            };
            active = true;
            restart = true;
            volumes = [
              {
                definition = nixvirtLib.volume.writeXML {
                  name = overlayName;
                  capacity = {
                    count = 40;
                    unit = "GiB";
                  };
                  target = {
                    format = {
                      type = "qcow2";
                    };
                  };
                  backingStore = {
                    path = toString ubuntuBaseImage;
                    format = {
                      type = "qcow2";
                    };
                  };
                };
              }
            ];
          }
        ];
        domains = [
          {
            definition =
              let
                domainConfig = nixvirtLib.domain.templates.linux {
                  name = domainName;
                  uuid = domainUuid;
                  vcpu = {
                    count = 4;
                  };
                  memory = {
                    count = 8;
                    unit = "GiB";
                  };
                  storage_vol = {
                    pool = poolName;
                    volume = overlayName;
                  };
                  backing_vol = null;
                  install_vol = toString cloudInitIso; # ensure cdrom uses file source, not libvirt volume
                  bridge_name = bridgeName;
                  virtio_drive = true;
                  virtio_video = null; # disable GL accel to avoid EGL init failures on headless host
                  net_iface_mac = null;
                  virtio_net = true;
                };
              in
              nixvirtLib.domain.writeXML (
                domainConfig
                // {
                  devices = domainConfig.devices // {
                    interface = {
                      type = "bridge";
                      source = {
                        bridge = bridgeName;
                      };
                      model = {
                        type = "virtio";
                      };
                      mac = null; # let libvirt assign; avoids MAC conflicts
                    };
                    serial = [
                      {
                        type = "pty";
                        target = {
                          type = "isa-serial";
                          port = 0;
                        };
                      }
                    ];
                    console = [
                      {
                        type = "pty";
                        target = {
                          type = "serial";
                          port = 0;
                        };
                      }
                    ];
                  };
                }
              );
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
      allowedBridges = [
        bridgeName
        "virbr0"
      ];
    };

    kvmgt = {
      enable = true;
    };

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
    };
  };

  networking.firewall.checkReversePath = false;

  programs = {
    virt-manager = {
      enable = true;
    };
  };
}
