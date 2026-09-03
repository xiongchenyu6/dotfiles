{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.my.sing-box-client;
  hy2Nodes = [
    {
      tag = "hy2-lubancat";
      server = "203.116.95.146";
      serverName = "hy2-lubancat.panda.qzz.io";
    }
    {
      tag = "hy2-oracle-amd-001";
      server = "213.35.97.233";
      serverName = "panda.qzz.io";
    }
    {
      tag = "hy2-oracle-amd-002";
      server = "213.35.117.232";
      serverName = "panda.qzz.io";
    }
    {
      tag = "hy2-sg-office";
      server = "101.78.126.6";
      serverName = "hy2-sg.panda.qzz.io";
    }
    {
      tag = "hy2-jtti-sg";
      server = "45.194.18.75";
      serverName = "hy2-jtti-sg.panda.qzz.io";
    }
  ];
  userAllowed = lib.concatMapStringsSep " || " (
    user: "subject.user == ${builtins.toJSON user}"
  ) cfg.toggleUsers;
in
{
  options.my.sing-box-client = {
    enable = lib.mkEnableOption "the opt-in sing-box Hysteria2 TUN client";
    toggleUsers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Users allowed to start and stop sing-box through systemd.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.toggleUsers != [ ];
        message = "my.sing-box-client.toggleUsers must contain at least one user";
      }
    ];

    sops.secrets."sing-box/HYSTERIA2_PASSWORD" = { };

    # pon/poff own the lifecycle so the TUN never appears just because the
    # workstation booted.
    systemd.services.sing-box = {
      wantedBy = lib.mkForce [ ];
      unitConfig.X-OnlyManualStart = true;
    };

    security.polkit.extraConfig = ''
      polkit.addRule(function(action, subject) {
        if (action.id == "org.freedesktop.systemd1.manage-units" &&
            (${userAllowed}) &&
            action.lookup("unit") == "sing-box.service" &&
            (action.lookup("verb") == "start" ||
             action.lookup("verb") == "restart" ||
             action.lookup("verb") == "stop")) {
          return polkit.Result.YES;
        }
      });
    '';

    services.sing-box = {
      enable = true;
      settings = {
        log.level = "warn";
        dns = {
          servers = [
            {
              type = "https";
              tag = "dns-direct";
              server = "223.5.5.5";
              detour = "direct";
              tls.server_name = "dns.alidns.com";
            }
            {
              type = "https";
              tag = "dns-proxy";
              server = "1.1.1.1";
              detour = "proxy";
              tls.server_name = "cloudflare-dns.com";
            }
          ];
          rules = [
            {
              domain_suffix = [
                "anthropic.com"
                "claude.ai"
                "claude.com"
                "claudeusercontent.com"
              ];
              action = "route";
              server = "dns-proxy";
            }
            {
              rule_set = [ "geosite-cn" ];
              action = "route";
              server = "dns-direct";
            }
          ];
          final = "dns-proxy";
          strategy = "ipv4_only";
          reverse_mapping = true;
        };
        inbounds = [
          {
            type = "tun";
            tag = "tun-in";
            interface_name = "sing-tun";
            address = [ "10.255.0.1/30" ];
            stack = "system";
            auto_route = true;
            auto_redirect = true;
            strict_route = true;
          }
        ];
        experimental.clash_api = {
          external_controller = "127.0.0.1:9090";
          external_ui = "${pkgs.metacubexd}";
        };
        outbounds = [
          {
            type = "urltest";
            tag = "proxy";
            outbounds = map (node: node.tag) hy2Nodes;
            url = "https://api.anthropic.com/";
            interval = "15s";
            tolerance = 2000;
            idle_timeout = "10m";
            interrupt_exist_connections = true;
          }
        ]
        ++ map (node: {
          type = "hysteria2";
          inherit (node) tag server;
          server_port = 8443;
          password._secret = config.sops.secrets."sing-box/HYSTERIA2_PASSWORD".path;
          tls = {
            enabled = true;
            server_name = node.serverName;
          };
        }) hy2Nodes
        ++ [
          {
            type = "direct";
            tag = "direct";
            domain_resolver = "dns-direct";
          }
        ];
        route = {
          auto_detect_interface = true;
          final = "proxy";
          rules = [
            { action = "sniff"; }
            {
              protocol = "dns";
              action = "hijack-dns";
            }
            {
              domain_suffix = [
                "anthropic.com"
                "claude.ai"
                "claude.com"
                "claudeusercontent.com"
              ];
              action = "route";
              outbound = "proxy";
            }
            {
              ip_cidr = [ "100.64.0.0/10" ];
              action = "bypass";
              outbound = "direct";
            }
            {
              ip_is_private = true;
              action = "bypass";
              outbound = "direct";
            }
            {
              rule_set = [
                "geoip-cn"
                "geosite-cn"
              ];
              action = "bypass";
              outbound = "direct";
            }
          ];
          rule_set = [
            {
              tag = "geoip-cn";
              type = "local";
              format = "binary";
              path = "${pkgs.sing-geoip}/share/sing-box/rule-set/geoip-cn.srs";
            }
            {
              tag = "geosite-cn";
              type = "local";
              format = "binary";
              path = "${pkgs.sing-geosite}/share/sing-box/rule-set/geosite-cn.srs";
            }
          ];
        };
      };
    };
  };
}
