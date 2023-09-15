# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, config, ... }: {
  imports = [ ./common.nix ];

  environment = {
    systemPackages = with pkgs; [
      apg
      aspell
      aspellDicts.en
      clang
      clang-tools
      cava
      cmake
      config.nur.repos.xddxdd.baidupcs-go
      config.nur.repos.xddxdd.qq
      config.nur.repos.xddxdd.wechat-uos
      firefox
      gcc
      gdb
      geoip
      google-chrome-dev
      gimp
      gnumake
      gitkraken
      grafana-loki
      haskell-language-server
      # (haskellPackages.ghcWithPackages (_:
      #   with haskellPackages;
      #   with pkgs.haskell.lib; [
      #     apply-refact
      #     cabal-install
      #     hlint
      #     stylish-haskell
      #     hasktags
      #     hoogle
      #     hadolint
      #   ]))
      imagemagick
      inetutils
      jp2a
      ledger-live-desktop
      linuxPackages.ply
      lldb
      llvm
      lsof
      manix
      mpv
      my_cookies
      mutagen
      openfortivpn
      openiscsi
      (python3.withPackages (_: with python3.pkgs; [ my_cookies mutagen ]))
      pass
      pciutils
      patchelf
      pinentry
      # poetry
      # poetry2nix.cli
      procs
      qemu_kvm
      termius
      tdesktop
      tpm2-tools
      unrar-wrapper
      virt-manager
      whatsapp-for-linux
      wineWowPackages.staging
      zotero
      zssh
    ];

    pathsToLink = [ "/share/zsh" ];
  };

  security = {
    tpm2 = {
      enable = true;
      abrmd.enable = true;
      # pkcs11.enable = true;
      tctiEnvironment = {
        enable = true;
        interface = "tabrmd";
      };
    };
  };

  services = {
    fprintd.enable = true;
    usbguard = {
      enable = false;
      IPCAllowedUsers = [ "root" "freeman.xiong" ];
      rules = ''
        allow id 1d6b:0002 serial "0000:00:14.0" name "xHCI Host Controller" hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" parent-hash "rV9bfLq7c2eA4tYjVjwO4bxhm+y6GgZpl9J60L0fBkY=" with-interface 09:00:00 with-connect-type ""
        allow id 1d6b:0003 serial "0000:00:14.0" name "xHCI Host Controller" hash "prM+Jby/bFHCn2lNjQdAMbgc6tse3xVx+hZwjOPHSdQ=" parent-hash "rV9bfLq7c2eA4tYjVjwO4bxhm+y6GgZpl9J60L0fBkY=" with-interface 09:00:00 with-connect-type ""
        allow id 1d6b:0002 serial "0000:00:0d.0" name "xHCI Host Controller" hash "d3YN7OD60Ggqc9hClW0/al6tlFEshidDnQKzZRRk410=" parent-hash "Y1kBdG1uWQr5CjULQs7uh2F6pHgFb6VDHcWLk83v+tE=" with-interface 09:00:00 with-connect-type ""
        allow id 1d6b:0003 serial "0000:00:0d.0" name "xHCI Host Controller" hash "4Q3Ski/Lqi8RbTFr10zFlIpagY9AKVMszyzBQJVKE+c=" parent-hash "Y1kBdG1uWQr5CjULQs7uh2F6pHgFb6VDHcWLk83v+tE=" with-interface 09:00:00 with-connect-type ""
        allow id 0bda:5411 serial "" name "USB2.1 Hub" hash "g/+iB1DQ0KhZLxPXGjcKbQj2R3XydzpZT6lp+326KOc=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" via-port "1-3" with-interface { 09:00:01 09:00:02 } with-connect-type "hotplug"
        allow id 06cb:00fc serial "df653f289845" name "" hash "HJRq/Yx3PvFras+eIaoPveJJvwee3fTIz6aI3mGtmHs=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" with-interface ff:00:00 with-connect-type "not used"
        allow id 5986:1178 serial "200901010001" name "Integrated Camera" hash "jQ1EQBHmEUXK9vqKCOJPNisN+5SFwvOaKxr69ffvZLE=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" with-interface { 0e:01:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 0e:02:00 fe:01:01 } with-connect-type "not used"
        allow id 0409:005a serial "" name "" hash "vUGB8+Dvgr/rRNWxsolEfpt0Jsl38zWjzgMaEOWvVd4=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" via-port "1-9" with-interface 09:00:00 with-connect-type "hotplug"
        allow id 8087:0033 serial "" name "" hash "ciwwGozaSw4maEXfs4NdvETeMt6bnFEK6f4vmCqfud0=" parent-hash "jEP/6WzviqdJ5VSeTUY8PatCNBKeaREvo2OqdplND/o=" via-port "1-10" with-interface { e0:01:01 e0:01:01 e0:01:01 e0:01:01 e0:01:01 e0:01:01 e0:01:01 e0:01:01 } with-connect-type "not used"
        allow id 043e:9a39 serial "204NTFA6Q531" name "LG Monitor Controls" hash "FCw6Jl2uZDVBhSDdTNRVJZBXKiZRRdRSJMlIGr5Cc0g=" parent-hash "g/+iB1DQ0KhZLxPXGjcKbQj2R3XydzpZT6lp+326KOc=" with-interface { 03:00:00 02:02:00 0a:00:00 } with-connect-type "unknown"
        allow id 043e:9c02 serial "123456789ABCDEFGH" name "BillBoard Device" hash "ulV8+X3UoK+7tQA0dYJJJOxp/djH1O6vI+nIdZmi86U=" parent-hash "g/+iB1DQ0KhZLxPXGjcKbQj2R3XydzpZT6lp+326KOc=" with-interface 11:00:00 with-connect-type "unknown"
        allow id 0853:0100 serial "" name "HHKB Professional" hash "RqDbLlyZQo+I8xpLEjAtlAA1DGq02JiaoUd8lsXt3og=" parent-hash "vUGB8+Dvgr/rRNWxsolEfpt0Jsl38zWjzgMaEOWvVd4=" via-port "1-9.1" with-interface 03:01:01 with-connect-type "unknown"
        allow id 2c97:4011 serial "0001" name "Nano X" hash "w22Ri9b7zIwfQk+eM8FQ7Kj7DSVMtuwM4ZskmHmaFc4=" parent-hash "vUGB8+Dvgr/rRNWxsolEfpt0Jsl38zWjzgMaEOWvVd4=" with-interface { 03:00:00 ff:ff:ff } with-connect-type "unknown"
        allow id 046d:c548 serial "" name "USB Receiver" hash "y0nMBYvoXYCGsR9c/YuFAQLlt1a63qgUaXo232qnT8Y=" parent-hash "vUGB8+Dvgr/rRNWxsolEfpt0Jsl38zWjzgMaEOWvVd4=" via-port "1-9.3" with-interface { 03:01:01 03:01:02 03:00:00 } with-connect-type "unknown"
      '';
    };
    dbus = { enable = true; };

    gnome = { gnome-keyring = { enable = true; }; };

    openldap = { enable = true; };

    # ympd = { enable = true; };
  };

  programs = {
    steam = { enable = true; };
    npm = { enable = true; };
    _1password = { enable = true; };
    _1password-gui = { enable = true; };
    # atop = {
    #   enable = true;
    #   netatop = { enable = true; };
    #   atopgpu = { enable = true; };
    # };
    nm-applet = { enable = true; };
    nix-ld.enable = true;
    wireshark = { enable = true; };
    proxychains = {
      enable = true;
      proxies = {
        michael = {
          type = "socks5";
          host = "localhost";
          port = 8888;
          enable = true;
        };
      };
    };
  };
}
