{ lib, modulesPath, ... }:
{
  imports = [
    #    ./base.nix
    "${modulesPath}/virtualisation/digital-ocean-image.nix"
  ];
  # digital-ocean-config.nix 故意把 hostName 置空,好让 droplet 的 metadata
  # 在启动时决定主机名;而 ez-configs 又按目录名设成 "digitalocean"。两者
  # 同优先级,求值直接冲突报错。这里构建的是 DO 镜像,语义以上游为准。
  networking.hostName = lib.mkForce "";

  boot.loader.grub.devices = lib.mkForce [ "/dev/vda" ];
  boot.loader.grub.device = "/dev/vda";
  swapDevices = [
    {
      device = "/swap/swapfile";
      size = 1024 * 2; # 2 GB
    }
  ];

  system.stateVersion = "24.11"; # Never change this
  # virtualisation.digitalOceanImage.compressionMethod = "bzip2";
}
