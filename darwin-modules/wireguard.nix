{
  config,
  pkgs,
  lib,
  shares,
  ...
}:
let
  wgInterface = "wg_tcloud";
in
{
  sops.secrets."wireguard/office" = { };

  environment.systemPackages = with pkgs; [
    wireguard-tools
    wireguard-go  # WireGuard userspace implementation for macOS
  ];

  launchd.daemons.wireguard-${wgInterface} = {
    script = ''
      set -e
      
      # Remove existing interface if it exists
      ${pkgs.wireguard-tools}/bin/wg-quick down ${wgInterface} 2>/dev/null || true
      
      # Create WireGuard configuration file with Table = off to handle routing manually
      cat > /tmp/${wgInterface}.conf <<EOF
      [Interface]
      PrivateKey = $(cat ${config.sops.secrets."wireguard/office".path})
      Address = fe80::101/64, 172.22.240.98/32, fd48:4b4:f3::2/128
      Table = off
      PostUp = route -n add -net 10.0.0.0/8 -interface ${wgInterface}
      PostUp = route -n add -net 172.20.0.0/14 -interface ${wgInterface}
      PostUp = route -n add -net 172.31.0.0/16 -interface ${wgInterface}
      PostUp = route -n add -inet6 fd00::/8 -interface ${wgInterface}
      PostUp = route -n add -inet6 fd48:4b4:f3::/48 -interface ${wgInterface}
      PostDown = route -n delete -net 10.0.0.0/8 -interface ${wgInterface} 2>/dev/null || true
      PostDown = route -n delete -net 172.20.0.0/14 -interface ${wgInterface} 2>/dev/null || true
      PostDown = route -n delete -net 172.31.0.0/16 -interface ${wgInterface} 2>/dev/null || true
      PostDown = route -n delete -inet6 fd00::/8 -interface ${wgInterface} 2>/dev/null || true
      PostDown = route -n delete -inet6 fd48:4b4:f3::/48 -interface ${wgInterface} 2>/dev/null || true
      
      [Peer]
      PublicKey = ${shares.hosts-dict.tcloud.wg.public-key}
      Endpoint = 43.156.66.157:22616
      PersistentKeepalive = 30
      AllowedIPs = 10.0.0.0/8, 172.20.0.0/14, 172.31.0.0/16, fd00::/8, fe80::/10, fd48:4b4:f3::/48, ff02::1:6/128, 224.0.0.251/32, ff02::fb/128
      EOF
      
      # Bring up the interface
      ${pkgs.wireguard-tools}/bin/wg-quick up /tmp/${wgInterface}.conf
    '';
    
    serviceConfig = {
      RunAtLoad = true;
      KeepAlive = true;
      StandardOutPath = "/var/log/wireguard-${wgInterface}.log";
      StandardErrorPath = "/var/log/wireguard-${wgInterface}.error.log";
    };
  };

  launchd.daemons.wireguard-${wgInterface}-stop = {
    script = ''
      ${pkgs.wireguard-tools}/bin/wg-quick down ${wgInterface} 2>/dev/null || true
    '';
    serviceConfig = {
      RunAtLoad = false;
    };
  };

  networking.knownNetworkServices = [ "Wi-Fi" ];
}