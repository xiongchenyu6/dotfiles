{ pkgs, lib, config, ... }:
let
  hasGuiTag =
    config ? system
    && config.system ? nixos
    && config.system.nixos ? tags
    && (builtins.elem "gui" config.system.nixos.tags);
in
{

  # sops.secrets."openldap/adminPass" = {
  #   owner = "nobody";
  #   group = "nogroup";
  #   mode = "400";
  # };

  # Install askpass only on GUI machines
  environment.systemPackages = lib.optionals hasGuiTag [
    pkgs.x11_ssh_askpass
  ];

  # Configure SSH askpass only on GUI machines
  programs.ssh = {
    enableAskPassword = hasGuiTag;
  } // lib.optionalAttrs hasGuiTag {
    askPassword = "${pkgs.x11_ssh_askpass}/libexec/ssh-askpass";
  };

  environment.etc = {
    # "ssh/auth" = {
    #   mode = "0555";
    #   text = ''
    #           #!${pkgs.stdenv.shell}
    #     #       ${pkgs.openldap}/bin/ldapsearch -x -D cn=admin,dc=auotlife,dc=ai -w $(${pkgs.coreutils-full}/bin/cat ${
    #       config.sops.secrets."openldap/adminPass".path
    #     }) -b dc=auotlife,dc=ai '(&(objectClass=posixAccount)(uid='"$1"'))' 'sshPublicKey' | ${pkgs.gnused}/bin/sed -n '/^ /{H;d};/sshPublicKey:/x;$g;s/\n *//g;s/sshPublicKey: //gp'
    #   '';
    # };
    "sudo.conf" = lib.mkIf hasGuiTag {
      mode = "0400";
      text = ''
        Path askpass ${pkgs.x11_ssh_askpass}/libexec/ssh-askpass
      '';
    };
  };

  services = {
    openssh = {
      enable = true;
      startWhenNeeded = false;
      settings = {
        #X11Forwarding = true;
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
      extraConfig = ''
        StreamLocalBindUnlink yes
      '';
      #  GSSAPIAuthentication yes
      #  GSSAPICleanupCredentials yes

    };
    # sshguard = { enable = true; };
  };
}
