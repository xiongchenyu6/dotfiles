{
  config,
  inputs,
  modulesPath,
  lib,
  pkgs,
  ezModules,
  shares,
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
    ezModules.wgmesh
    ezModules.server
    ezModules.acme
    ezModules.sing-box
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    ./hardware-configuration.nix
    ./sex-qd-je.nix
    ./ib-gateway.nix
  ];

  # hysteria2 出海入站(UDP 8443):sub2api 跨境 + 个人客户端统一走这个。
  # 历史:裸 VLESS(10086)和 VLESS+Reality(443)都试过并已下线——
  # Reality 同链路只有 83% 成功率,hysteria2 有 93% 且快 3.6 倍。
  my.sing-box-hysteria2.enable = true;

  sops.secrets."cloudflared/tunnel-credentials" = { };
  sops.secrets."wireguard/oracle-amd-001" = { };

  # 1GB RAM box running the IB Gateway JVM — zram alone can't absorb its spikes.
  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 2048;
    }
  ];

  # Make the already-loaded BBR module effective and allow TCP autotuning to
  # fill high-bandwidth, high-latency paths (nginx 等 TCP 服务仍受益)
  # without forcing every connection to allocate the maximum buffer.
  boot.kernel.sysctl = {
    "net.core.default_qdisc" = "fq";
    "net.ipv4.tcp_congestion_control" = "bbr";
    "net.core.rmem_max" = 33554432;
    "net.core.wmem_max" = 33554432;
    "net.ipv4.tcp_rmem" = "4096 131072 33554432";
    "net.ipv4.tcp_wmem" = "4096 16384 33554432";
  };

  environment = {
    systemPackages = [
      pkgs.cloudflared
      pkgs.nix
    ];
  };

  # Same VCN as the relay: dial its private address, not the public one.
  services.wgmesh.nodes.oracle-amd-002.endpoint = lib.mkForce "10.0.0.246:51821";

  networking = {
    firewall = {
      allowedTCPPorts = [
        80
        443
        636
      ];
      allowedUDPPorts = [ 53 ];
      # IB Gateway paper API, published on the mesh address only (ib-gateway.nix).
      interfaces.wgmesh.allowedTCPPorts = [ 4002 ];
    };

    # Mesh peer of the oracle-amd-002 hub. Both hosts share the Oracle VCN, so dial the
    # hub's private address instead of hairpinning through its public IP.
    wg-quick.interfaces.wg_ora = {
      privateKeyFile = config.sops.secrets."wireguard/oracle-amd-001".path;
      table = "off";
      address = [
        "fd48:4b4:f3::8/64"
      ];
      peers = [
        {
          endpoint = "10.0.0.246:22616";
          publicKey = shares.hosts.oracle-amd-002.wg.public-key;
          persistentKeepalive = 25;
          allowedIPs = [
            "fd48:4b4:f3::/64"
          ];
        }
      ];
    };
  };
}
