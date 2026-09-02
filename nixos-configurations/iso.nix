# This module defines a small NixOS installation CD. It does not
# contain any graphical stuff.
{
  pkgs,
  lib,
  modulesPath,
  shares,
  ...
}:
{
  imports = [
    # Currently fails to build due to ZFS incompatibility with bcachefs
    #<nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    (modulesPath + "/installer/cd-dvd/installation-cd-graphical-calamares-gnome.nix")
  ];
  boot.supportedFilesystems = {
    btrfs = true;
    zfs = lib.mkForce false;
    bcachefs = true;
  };

  # **装机介质必须能远程进。**
  # 不开 sshd 的话，装机全程只能趴在物理机前一个字一个字敲——分区、看日志、
  # 排错都得手打，出了问题连日志都拷不出来。开了之后插上网线/连上 WiFi 就能
  # 从工位 SSH 进去，装机器本身照常在屏幕上跑。
  #
  # 公钥来源和 nixos-modules/root.nix 同一个（shares），不另写一份——两处各自
  # 维护迟早会分叉，而分叉的那一半正好是救援时要用的。
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";
      PasswordAuthentication = false;
    };
  };
  users.users.root.openssh.authorizedKeys.keys = [
    shares.users."freeman.xiong".public-key
    shares.users."freeman.xiong".yubikey
  ];
  # live ISO 的默认用户，图形会话就是它——同样铺上钥匙，免得只能用 root。
  users.users.nixos.openssh.authorizedKeys.keys = [
    shares.users."freeman.xiong".public-key
    shares.users."freeman.xiong".yubikey
  ];

  nixpkgs.hostPlatform = "x86_64-linux";

  environment = {
    systemPackages = with pkgs; [
      gnupg
      sops
    ];
  };

  programs = {
    gnupg = {
      agent = {
        enable = true;
        enableSSHSupport = true;
        enableExtraSocket = true;
        enableBrowserSocket = true;

      };
    };
  };
  nix = {
    settings = {
      accept-flake-config = true;
      allow-import-from-derivation = true;
      experimental-features = [
        "nix-command"
        "flakes"
        #"ca-derivations"
        "parse-toml-timestamps"
      ];
      trusted-users = [
        "freeman.xiong"
        "freeman"
        "@wheel"
        "@admin"
      ];
      allowed-users = [
        "root"
        "freeman"
        "freeman.xiong"
        "@wheel"
        "@admin"
      ];
      substituters = [
        "https://xddxdd.cachix.org"
        "https://xiongchenyu6.cachix.org"
        "https://hyprland.cachix.org"
      ];
      trusted-public-keys = [
        "xddxdd.cachix.org-1:ay1HJyNDYmlSwj5NXQG065C8LfoqqKaTNCyzeixGjf8="
        "xiongchenyu6.cachix.org-1:mpOGlINmMwc2gb3xb1BjVmhzR8BYWzWYlg4xlTiBr7Q="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      ];
    };
    distributedBuilds = lib.mkDefault true;
  };

  # kernelPackages already defined in installation-cd-minimal-new-kernel-no-zfs.nix
  boot.kernelPackages = lib.mkOverride 0 pkgs.linuxPackages_latest;
  isoImage.squashfsCompression = "gzip -Xcompression-level 1";
}
