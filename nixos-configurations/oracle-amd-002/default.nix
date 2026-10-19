{
  inputs,
  modulesPath,
  lib,
  pkgs,
  config,
  ezModules,
  ...
}:
let
in
{
  imports = with inputs; [
    disko.nixosModules.disko
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.wgmesh
    ezModules.server
    ezModules.acme
    ezModules.bird-border
    ezModules.dn42
    ezModules.sing-box
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    ./hardware-configuration.nix
    ./rustdesk.nix
    {
      topology.self.interfaces.home = {
        type = "wireguard";
        addresses = [ "172.22.240.97/27" ];
      };
    }
  ];

  # 在 10086 的裸 VLESS 之外加一个 VLESS + Reality 入站(443)。
  # 腾讯HK 的 sub2api 出海要经这里:裸 VLESS 没有 TLS,内层 SNI 明文暴露,
  # api.openai.com 会在 TLS 握手阶段被跨境链路重置(实测 100% 复现)。
  # Reality 把握手伪装成访问真实大站,链路上看不到真实目的地。
  # 用 443 是因为 Oracle Cloud 安全组只放行 22/80/443/10086,且 443 空闲 —
  # 这样既不用动云控制台,也不顶掉 10086 上给 clash-verge 用的裸 VLESS。
  # hysteria2 出海入站(UDP 8443),给腾讯HK 上的 sub2api 用。
  # Reality 试过但退掉了:同链路只有 83% 成功率,而 hysteria2 有 93% 且快 3.6 倍。
  my.sing-box-hysteria2.enable = true;
  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 2048;
    }
  ];

  boot.initrd.kernelModules = [ "nvme" ];

  boot = {
    kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
    tmp.cleanOnBoot = true;
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
        # VLESS is TCP-based here. BBR plus fq improves throughput on long-fat
        # paths, while the larger maxima let TCP autotuning grow only when the
        # measured bandwidth-delay product requires it.
        "net.core.default_qdisc" = "fq";
        "net.ipv4.tcp_congestion_control" = "bbr";
        "net.core.rmem_max" = 33554432;
        "net.core.wmem_max" = 33554432;
        "net.ipv4.tcp_rmem" = "4096 131072 33554432";
        "net.ipv4.tcp_wmem" = "4096 16384 33554432";
      };
    };
  };

  # Same VCN as oracle-amd-001: dial its private address, not the public one.
  services.wgmesh.nodes.oracle-amd-001.endpoint = lib.mkForce "10.0.0.199:51821";

  networking = {
    nat = {
      enable = true;
      enableIPv6 = true;
      # Oracle shapes name the uplink ens3; "ens5" never existed here, so NAT was a no-op.
      externalInterface = "ens3";
      internalInterfaces = [ config.services.wgmesh.interface ];
    };

    # The phone cannot run wgautomesh, so it is a plain peer of the relay. The
    # mesh /27 is already routed into wgmesh, so no per-peer route either.
    wireguard.interfaces.${config.services.wgmesh.interface} = {
      allowedIPsAsRoutes = false;
      peers = [
        {
          publicKey = "CAW6+atqM9xmCAZUaev3OZWbYKwjDNCHezyiBpiHmSg=";
          allowedIPs = [ "172.22.240.101/32" ];
        }
      ];
    };

    firewall = {
      enable = true;
      trustedInterfaces = [ config.services.wgmesh.interface ];
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

    # DN42 eBGP to Kioubit: a static link-local tunnel that wgmesh cannot carry.
    wg-quick.interfaces = {
      wg_kioubit = {
        privateKeyFile = config.sops.secrets."wireguard/oracle-amd-002".path;
        table = "off";
        address = [ "fe80::100/64" ];
        peers = [
          {
            endpoint = "hk1.g-load.eu:22616";
            # We dial out and nothing else sends on this link until bird's
            # BGP session is up, so keep the handshake alive ourselves.
            persistentKeepalive = 25;
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

  sops.secrets."wireguard/oracle-amd-002" = { };

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

  # fuse ships the mount.fuse helper; without it mount(8) hands the s3fs options straight to
  # the kernel fuse driver ("Unknown parameter 'use_path_request_style'").
  environment.systemPackages = with pkgs; [
    fuse
    s3fs
  ];

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
