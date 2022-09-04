{ ... }: {
  imports = [ ./hardware-configuration.nix ];

  boot.cleanTmpDir = true;
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
    "net.ipv6.conf.all.forwarding" = 1;
  };
  zramSwap.enable = true;
  networking.hostName = "VM-8-10-ubuntu";
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCzzKV5IF7ekz9oJQ37nbaUNhXKkQ4KzJiDOYVRVroFq+LEJZHqNxe/Lt1Z1cKvFjRruu6f3clzqRargKlmqbO1d8mJZy0R9TbKQxleEZZq2cZJemX99xrkiu9keBF2qhohwn28v0JUuUyjNo188/YyS1tocoWFNZtp7qPiK8HRF7LQQ99nOa3zGmZJQL5Rvs2RFTFMGhiehsq8aXFuTZNejjivl5BFJjzxoVqZSbB8//lwsGZWpU5Ue54KV51UTv+9wDh2myuyenP/ZbdK9UZo9abCIeI52F9QbGJtjz6cOKG6oz67x06EYxvD/HKJ/uPuisy/cu+rPInmaF5AZTnd skey-p1r300u3"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC3LHhrdC+Mor6lM9U0fxyJ2WCn4CzNUZPyOP8ACQpAl5bADYY8ici2SbRD6y0dZnwNvSUJKw090HXiPOgKCYrfPQPX4IOgiPLqPBJq0JCI7w7/pewRmg1bd/5k5BC8C5x0P2H63DovDXEnyIJxqZnVWZDjfhysGEVGueoYBxeDAHHBZLwGGxW36oX8OmfiTGDmMrHWqQxKpluR6KIbe4aFML+ZIol0Vy6+244gREZZXn6xTAoCxRGghaEnOf5X3SivKOJHLTDpAXI7JYesepHHyCPd+OXH2VzSVj0qqOtzb5t6mNHkM4wC9HhTqPT/KWuxjv9HcpXjag9ZGuby/LxOo+6knb5a7VtRm0GxvbBptNS5Frlrl9HNwARiqSmiaSvOWydrZYKV0/ClIYdA7f4DMUc46KIP+wHqLXO2oBe5I4sK4TesmOxCYezi2ti/T4sC/e4Hlvgc/luvS6p0GdTtZ0wQLMmqz2u79LVRpjQMFygLa0IQXFo7c+0FqB7Et8M= skey-21jfw3n7"
  ];
  networking.useNetworkd = true;
  networking.useDHCP = false;
  networking.firewall.enable = false;
  systemd.network = {
        enable = true;
        config = {
          routeTables.custom = 23;
        };
        netdevs = {
          "90-wg0" = {
            netdevConfig = { Kind = "wireguard"; Name = "wg0"; };
            wireguardConfig = {
              # NOTE: we're storing the wireguard private key in the
              #       store for this test. Do not do this in the real
              #       world. Keep in mind the nix store is
              #       world-readable.
              PrivateKeyFile = pkgs.writeText "wg0-priv" privk;
              ListenPort = 51820;
              FirewallMark = 42;
            };
            wireguardPeers = [ {wireguardPeerConfig={
              Endpoint = "192.168.1.${peerId}:51820";
              PublicKey = pubk;
              PresharedKeyFile = pkgs.writeText "psk.key" "yTL3sCOL33Wzi6yCnf9uZQl/Z8laSE+zwpqOHC4HhFU=";
              AllowedIPs = [ "10.0.0.${peerId}/32" ];
              PersistentKeepalive = 15;
            };}];
          };
        };
        networks = {
          "99-nope" = {
            matchConfig.Name = "eth*";
            linkConfig.Unmanaged = true;
          };
          "90-wg0" = {
            matchConfig = { Name = "wg0"; };
            address = [ "10.0.0.${nodeId}/32" ];
            routes = [
              { routeConfig = { Gateway = "10.0.0.${nodeId}"; Destination = "10.0.0.0/24"; }; }
              { routeConfig = { Gateway = "10.0.0.${nodeId}"; Destination = "10.0.0.0/24"; Table = "custom"; }; }
            ];
          };
          "30-eth1" = {
            matchConfig = { Name = "eth1"; };
            address = [
              "192.168.1.${nodeId}/24"
              "fe80::${nodeId}/64"
            ];
            routingPolicyRules = [
              { routingPolicyRuleConfig = { Table = 10; IncomingInterface = "eth1"; Family = "both"; };}
              { routingPolicyRuleConfig = { Table = 20; OutgoingInterface = "eth1"; };}
              { routingPolicyRuleConfig = { Table = 30; From = "192.168.1.1"; To = "192.168.1.2"; SourcePort = 666 ; DestinationPort = 667; };}
              { routingPolicyRuleConfig = { Table = 40; IPProtocol = "tcp"; InvertRule = true; };}
              { routingPolicyRuleConfig = { Table = 50; IncomingInterface = "eth1"; Family = "ipv4"; };}
            ];
          };
        };
}
