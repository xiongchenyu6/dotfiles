{ pkgs, config, modulesPath, ... }:
let
  # generate via openvpn --genkey --secret openvpn-laptop.key
  client-key = "/root/openvpn-laptop.key";
  domain = "43.156.66.157";
  vpn-dev = "tun0";
  port = 1194;

  openvpn = {
    ca = ./vpn/ca.crt;
    cert = ./vpn/server.crt;
    key = ./vpn/server.key;
    ta = ./vpn/ta.key;
    dh = ./vpn/dh2048.pem;

    client_subnet = "10.89.98.0";
    client_mask_bits = 24;
    client_mask = "255.255.255.0";

    forward_to_subnet = "192.168.4.0";
    forward_to_mask_bits = 24;
    forward_to_mask = "255.255.255.0";
  };

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

  services.openvpn.servers = {
    # peer2peer.config = ''
    #   dev ${vpn-dev}
    #   proto udp
    #   ifconfig 10.8.0.1 10.8.0.2
    #   secret ${client-key}
    #   port ${toString port}

    #   cipher AES-256-CBC
    #   auth-nocache

    #   comp-lzo
    #   keepalive 10 60
    #   ping-timer-rem
    #   persist-tun
    #   persist-key
    # '';
    server.config = ''
      port 1194
      proto udp
      dev tun
      ca ${openvpn.ca}
      cert ${openvpn.cert}
      key ${openvpn.key}
      dh ${openvpn.dh}
      tls-auth ${openvpn.ta} 0

      server ${openvpn.client_subnet} ${openvpn.client_mask}
      keepalive 10 120
      comp-lzo
      max-clients 5
      user nobody
      group nogroup
      persist-key
      persist-tun
      verb 6
      reneg-sec 0

      push "route ${openvpn.forward_to_subnet} ${openvpn.forward_to_mask}"
      push "redirect-gateway def1" # https://openvpn.net/index.php/open-source/documentation/howto.html#redirect
    '';
  };

  environment.etc."openvpn/peer2peer.ovpn" = {
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
    f="/etc/openvpn/peer2peer.ovpn"
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
