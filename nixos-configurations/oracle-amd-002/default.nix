{
  inputs,
  modulesPath,
  lib,
  pkgs,
  config,
  ezModules,
  shares,
  ...
}:
let
  vpn-dev = "wg0";
  port = 22616;
in
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
    ezModules.sing-box
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    xiongchenyu6.nixosModules.nautilus-equity-trend
    ./hardware-configuration.nix
    ./ib-gateway.nix
    ./nautilus-equity.nix
    {
      topology.self.interfaces.home = {
        type = "wireguard";
        addresses = [ "172.22.240.97/27" ];
      };
    }
  ];

  boot.initrd.kernelModules = [ "nvme" ];

  boot = {
    kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
    tmp.cleanOnBoot = true;
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
      };
    };
  };

  networking = {
    nat = {
      enable = true;
      enableIPv6 = true;
      externalInterface = "ens5";
      internalInterfaces = [ vpn-dev ];
    };

    firewall = {
      enable = true;
      trustedInterfaces = [ vpn-dev ];
      allowedTCPPorts = [
        22
        53
        80
        443
        179
        389
        636
        993
      ];
      allowedUDPPorts = [
        port
        53
        80
        179
        389
        636
        5353
      ];
      allowedUDPPortRanges = [
        {
          from = 49152;
          to = 65535;
        }
      ];
    };

    wg-quick = {
      interfaces =
        let
          privateKeyFile = config.sops.secrets."wireguard/tcloud".path;
          table = "off";
          mkPeer = mesh4: mesh6: publicKey: {
            inherit publicKey;
            allowedIPs = [
              "${mesh4}/32"
              "${mesh6}/128"
            ];
          };
        in
        {
          ${vpn-dev} = {
            inherit privateKeyFile table;
            listenPort = port;
            address = [
              "172.22.240.97/27"
              "fd48:4b4:f3::1/64"
            ];
            postUp = ''
              ${pkgs.iproute2}/bin/ip link set multicast on dev ${vpn-dev}
            '';
            peers = [
              (mkPeer "172.22.240.98" "fd48:4b4:f3::2"
                shares.hosts-dict.office.wg.public-key)
              (mkPeer "172.22.240.99" "fd48:4b4:f3::3"
                shares.hosts-dict.game.wg.public-key)
              (mkPeer "172.22.240.100" "fd48:4b4:f3::4"
                "nBEkTpn4kRYXS9r7beXh3uMYJBAq/534byXv8NsB8gM=")
              (mkPeer "172.22.240.101" "fd48:4b4:f3::5"
                "CAW6+atqM9xmCAZUaev3OZWbYKwjDNCHezyiBpiHmSg=")
              (mkPeer "172.22.240.102" "fd48:4b4:f3::6"
                "9WkAJx+EG3VifVLiMgD8+6CoCsBwSyWAMwtajoy/OTk=")
            ];
          };
          wg_kioubit = {
            inherit privateKeyFile table;
            address = [ "fe80::100/64" ];
            peers = [
              {
                endpoint = "hk1.g-load.eu:22616";
                publicKey = "sLbzTRr2gfLFb24NPzDOpy8j09Y6zI+a7NkeVMdVSR8=";
                allowedIPs = [
                  "10.0.0.0/8"
                  "172.20.0.0/14"
                  "172.31.0.0/16"
                  "fd00::/8"
                  "fe80::/64"
                  "fd48:4b4:f3::/48"
                  "ff02::1:6/128"
                  "224.0.0.251/32"
                  "ff02::fb/128"
                ];
              }
            ];
          };
        };
    };
  };

  sops.secrets."wireguard/tcloud" = { };

  sops.templates."s3fs-passwd" = {
    content = "${config.sops.placeholder."s3fs/access_key"}:${
      config.sops.placeholder."s3fs/secret_key"
    }";
    mode = "0600";
    owner = "root";
    group = "root";
  };

  sops.secrets."s3fs/access_key" = { };
  sops.secrets."s3fs/secret_key" = { };

  services = {
    openssh = {
      enable = true;
      authorizedKeysCommand = "/run/wrappers/bin/kanidm_ssh_authorizedkeys %u";
      authorizedKeysCommandUser = "nobody";
      settings = {
        UsePAM = true;
      };
    };
  };

  environment.systemPackages = with pkgs; [ s3fs ];

  # S3 FUSE mount for iDrive e2 docs bucket
  fileSystems."/mnt/s3/docs" = {
    device = "docs";
    fsType = "fuse./run/current-system/sw/bin/s3fs";
    noCheck = true;
    options = [
      "_netdev"
      "allow_other"
      "use_path_request_style"
      "url=https://s3.us-west-1.idrivee2.com"
      "endpoint=us-west-1"
      "passwd_file=${config.sops.templates."s3fs-passwd".path}"
    ];
  };
}
