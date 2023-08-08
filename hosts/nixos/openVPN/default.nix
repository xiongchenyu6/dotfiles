{ pkgs, config, lib, modulesPath, ... }:
let
  # generate via openvpn --genkey --secret openvpn-laptop.key
  client-key = "/root/openvpn-laptop.key";
  domain = "3.82.157.47";
  vpn-dev = "tun0";
  port = 1194;
in {

  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];
  networking = {
    nat = {
      enable = true;
      enableIPv6 = true;
      externalInterface = "enX0";
      internalInterfaces = [ vpn-dev ];
    };
    firewall.enable = true;
    firewall.trustedInterfaces = [ vpn-dev ];
    firewall.allowedUDPPorts = [ port ];
  };

  environment.systemPackages = [ pkgs.openvpn ]; # for key generation

  services.openvpn.servers.smartphone.config = ''
    dev ${vpn-dev}
    proto udp
    ifconfig 10.8.0.1 10.8.0.2
    secret ${client-key}
    port ${toString port}

    cipher AES-256-CBC
    auth-nocache

    comp-lzo
    keepalive 10 60
    ping-timer-rem
    persist-tun
    persist-key
  '';

  environment.etc."openvpn/smartphone-client.ovpn" = {
    text = ''
      dev tun
      remote "${domain}"
      ifconfig 10.8.0.2 10.8.0.1
      port ${toString port}
      redirect-gateway def1

      cipher AES-256-CBC
      auth-nocache

      comp-lzo
      keepalive 10 60
      resolv-retry infinite
      nobind
      persist-key
      persist-tun
      secret [inline]

    '';
    mode = "600";
  };
  system.activationScripts.openvpn-addkey = ''
    f="/etc/openvpn/smartphone-client.ovpn"
    if ! grep -q '<secret>' $f; then
      echo "appending secret key"
      echo "<secret>" >> $f
      cat ${client-key} >> $f
      echo "</secret>" >> $f
    fi
  '';

  programs.zsh.enable = true;

  users = {
    users = {
      root = {
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKZJj5k1FTiW8+2KBqw1WWgSwVNxV+Md/32G3AfjqXNR"
        ];
        shell = pkgs.zsh;
      };
    };
  };
}
