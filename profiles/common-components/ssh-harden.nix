_: {

  # sops.secrets."openldap/adminPass" = {
  #   owner = "nobody";
  #   group = "nogroup";
  #   mode = "400";
  # };

  # environment.etc = {
  #   "ssh/auth" = {
  #     mode = "0555";
  #     text = ''
  #       #!${pkgs.stdenv.shell}
  #       ${pkgs.openldap}/bin/ldapsearch -x -D cn=admin,dc=freeman,dc=engineer -w $(${pkgs.coreutils-full}/bin/cat ${
  #         config.sops.secrets."openldap/adminPass".path
  #       }) -b dc=freeman,dc=engineer '(&(objectClass=posixAccount)(uid='"$1"'))' 'sshPublicKey' | ${pkgs.gnused}/bin/sed -n '/^ /{H;d};/sshPublicKey:/x;$g;s/\n *//g;s/sshPublicKey: //gp'
  #     '';
  #   };
  #   "sudo.conf" = {
  #     mode = "0400";
  #     text = ''
  #       Path askpass ${pkgs.lxqt.lxqt-openssh-askpass}/bin/lxqt-openssh-askpass
  #     '';
  #   };
  # };
  services = {
    openssh = {
      enable = true;
      startWhenNeeded = false;
      forwardX11 = true;
      settings = {
        passwordAuthentication = true;
        logLevel = "VERBOSE";
      };
      ports = [ 2222 ];
      ciphers = [
        "chacha20-poly1305@openssh.com"
        "aes256-gcm@openssh.com"
        "aes128-gcm@openssh.com"
      ];
      kexAlgorithms = [
        "sntrup761x25519-sha512@openssh.com"
        "curve25519-sha256"
        "curve25519-sha256@libssh.org"
      ];
      macs = [
        "hmac-sha2-512"
        "hmac-sha2-512-etm@openssh.com"
        "hmac-sha2-256"
        "hmac-sha2-256-etm@openssh.com"
      ];
      extraConfig = ''
        GSSAPIAuthentication yes
        GSSAPICleanupCredentials yes
      '';
      # authorizedKeysCommand = "/etc/ssh/auth";
    };
    # sshguard = { enable = true; };
  };
}
