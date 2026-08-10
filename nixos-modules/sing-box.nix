{
  config,
  lib,
  ...
}:
let
  cfg = config.my.sing-box-hysteria2;
in
{
  options.my.sing-box-hysteria2 = {
    enable = lib.mkEnableOption ''
      hysteria2 入站（UDP/QUIC），给 sub2api 的跨境出海用。

      为什么需要它：跨境段专门掐 TCP 类隧道。同一条链路上实测，
      shadowsocks 只跑到 16 KB/s，而两端各自直连都有 8-9 MB/s。
      hysteria2 走 UDP/QUIC + Brutal 拥塞控制，不吃这套 QoS，
      同链路实测 3.10 MB/s —— 快 180 倍。

      默认关闭 —— tcloud 也用这个模块，它不需要这个入站
    '';

    port = lib.mkOption {
      type = lib.types.port;
      default = 8443;
      description = ''
        UDP 监听端口。Oracle Cloud 的安全组是放行 UDP 的（实测），
        真正挡住的是 NixOS 自己的防火墙 —— 所以下面同时放行
        allowedUDPPorts。注意这些机器是 nftables 后端，拿 iptables
        去临时开洞会静默失败（排查时在这上面白费过一轮）。
      '';
    };

    certName = lib.mkOption {
      type = lib.types.str;
      default = "panda.qzz.io";
      description = ''
        用哪张 ACME 证书。oracle-amd-00x.autolife-robotics.me 那张早在
        2025-04 就过期没再续，只有 panda.qzz.io 的 acme 定时器还在跑。
        证书目录属组是 nginx，所以下面把 sing-box 加进了 nginx 组。
      '';
    };
  };

  config = {
    sops.secrets =
      let
        singBoxOwned = {
          owner = config.users.users.sing-box.name or "root";
          mode = "0400";
        };
      in
      {
        "sing-box/V2RAY" = singBoxOwned;
      }
      // lib.optionalAttrs cfg.enable {
        "sing-box/HYSTERIA2_PASSWORD" = singBoxOwned;
      };

    # ACME 把 panda.qzz.io 的证书目录设成 acme:nginx 750，sing-box 默认只在
    # 自己的组里，读不到。加进 nginx 组是最小改动 —— 另一条路是改
    # security.acme.certs.<name>.group，但那会牵动 nginx 自己的读取。
    users.users.sing-box.extraGroups = lib.mkIf cfg.enable [ "nginx" ];

    networking.firewall = {
      # 10086 是给 clash-verge 等个人客户端的裸 VLESS，与 sub2api 无关。
      allowedTCPPorts = [ 10086 ];
      allowedUDPPorts = lib.optional cfg.enable cfg.port;
    };

    services.sing-box = {
      enable = true;
      settings = {
        inbounds = [
          {
            type = "vless";
            listen = "::";
            listen_port = 10086;
            users = [
              {
                uuid = {
                  _secret = config.sops.secrets."sing-box/V2RAY".path;
                };
                flow = "";
              }
            ];
          }
        ]
        ++ lib.optional cfg.enable {
          type = "hysteria2";
          tag = "hysteria2-in";
          listen = "::";
          listen_port = cfg.port;
          users = [
            {
              password = {
                _secret = config.sops.secrets."sing-box/HYSTERIA2_PASSWORD".path;
              };
            }
          ];
          # 不开 salamander 混淆：实测反而把成功率从 93%(14/15) 拉低到
          # 75%(15/20)，吞吐只微涨。多一层逐包处理在这条链路上得不偿失。
          tls = {
            enabled = true;
            certificate_path = "/var/lib/acme/${cfg.certName}/fullchain.pem";
            key_path = "/var/lib/acme/${cfg.certName}/key.pem";
          };
        };
        outbounds = [
          {
            type = "direct";
            tag = "direct";
          }
        ];
      };
    };
  };
}
