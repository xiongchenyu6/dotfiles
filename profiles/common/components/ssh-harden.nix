{ pkgs, lib, ... }: {

  # sops.secrets."openldap/adminPass" = {
  #   owner = "nobody";
  #   group = "nogroup";
  #   mode = "400";
  # };

  environment.etc = {
    # "ssh/auth" = {
    #   mode = "0555";
    #   text = ''
    #           #!${pkgs.stdenv.shell}
    #     #       ${pkgs.openldap}/bin/ldapsearch -x -D cn=admin,dc=autolife-robotics,dc=tech -w $(${pkgs.coreutils-full}/bin/cat ${
    #       config.sops.secrets."openldap/adminPass".path
    #     }) -b dc=autolife-robotics,dc=tech '(&(objectClass=posixAccount)(uid='"$1"'))' 'sshPublicKey' | ${pkgs.gnused}/bin/sed -n '/^ /{H;d};/sshPublicKey:/x;$g;s/\n *//g;s/sshPublicKey: //gp'
    #   '';
    # };
    "sudo.conf" = {
      mode = "0400";
      text = ''
        Path askpass ${pkgs.x11_ssh_askpass}/libexec/ssh-askpass
      '';
    };
  };

  services = {
    openssh = {
      openFirewall = lib.mkDefault false;
      enable = true;
      startWhenNeeded = false;
      settings = {
        X11Forwarding = true;
        PasswordAuthentication = false;
        LogLevel = "VERBOSE";
        Ciphers = [
          "chacha20-poly1305@openssh.com"
          "aes256-gcm@openssh.com"
          "aes128-gcm@openssh.com"
        ];
        KexAlgorithms = [
          "sntrup761x25519-sha512@openssh.com"
          "curve25519-sha256"
          "curve25519-sha256@libssh.org"
        ];
        Macs = [
          "hmac-sha2-512"
          "hmac-sha2-512-etm@openssh.com"
          "hmac-sha2-256"
          "hmac-sha2-256-etm@openssh.com"
        ];
      };
      ports = [ 2222 ];
      extraConfig = ''
        StreamLocalBindUnlink yes
      '';
      #  GSSAPIAuthentication yes
      #  GSSAPICleanupCredentials yes

    };
    # sshguard = { enable = true; };
  };
}
