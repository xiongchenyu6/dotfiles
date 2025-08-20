{ config, lib, pkgs, ... }:

{
  # Define SOPS secrets for OpenFortiVPN
  sops.secrets = {
    "openfortivpn/server" = {};
    "openfortivpn/username" = {};
    "openfortivpn/password" = {};
    "openfortivpn/trusted_cert" = {};
  };

  # Use SOPS template to generate config file
  sops.templates."openfortivpn-config" = {
    content = ''
      # OpenFortiVPN Configuration
      host = ${config.sops.placeholder."openfortivpn/server"}
      port = 443
      username = ${config.sops.placeholder."openfortivpn/username"}
      password = ${config.sops.placeholder."openfortivpn/password"}
      trusted-cert = ${config.sops.placeholder."openfortivpn/trusted_cert"}
      
      # Connection settings
      set-dns = 1
      set-routes = 1
      half-internet-routes = 0
      pppd-use-peerdns = 1
    '';
    mode = "0600";
    owner = "root";
    group = "root";
  };

  # Create symlink to the generated config
  environment.etc."openfortivpn/config".source = config.sops.templates."openfortivpn-config".path;

  # Install openfortivpn and convenience scripts
  environment.systemPackages = with pkgs; [
    openfortivpn
    
    (writeShellScriptBin "vpn-connect" ''
      echo "Starting OpenFortiVPN connection..."
      read -p "Enter OTP code: " otp_code
      sudo openfortivpn -c /etc/openfortivpn/config --otp="$otp_code"
    '')
    
    (writeShellScriptBin "vpn-disconnect" ''
      echo "Stopping OpenFortiVPN..."
      sudo pkill openfortivpn || true
      echo "VPN disconnected"
    '')
    
    (writeShellScriptBin "vpn-status" ''
      if pgrep openfortivpn > /dev/null; then
        echo "OpenFortiVPN is running"
        ip addr show | grep -A2 "ppp\|tun"
      else
        echo "OpenFortiVPN is not running"
      fi
    '')
  ];
}