{ config, lib, ... }:
{
  # 我用的一些内核参数
  imports = [
    ../../users/root/nixos.nix
    ../../profiles/common/components/ssh-harden.nix
  ];

  boot.kernelParams = [
    # 关闭内核的操作审计功能
    "audit=0"
    # 不要根据 PCIe 地址生成网卡名（例如 enp1s0，对 VPS 没用），而是直接根据顺序生成（例如 eth0）
    "net.ifnames=0"
  ];

  # 我用的 Initrd 配置，开启 ZSTD 压缩和基于 systemd 的第一阶段启动
  boot.initrd = {
    compressor = "zstd";
    compressorArgs = [
      "-19"
      "-T0"
    ];
    systemd.enable = true;
  };

  # 安装 Grub
  boot.loader.grub = {
    enable = !config.boot.isContainer;
    default = "saved";
    devices = [ "/dev/vda" ];
  };

  # 使用 systemd-networkd 管理网络
  systemd.network.enable = true;
  services.resolved.enable = false;

  # 配置网络 IP 和 DNS
  systemd.network.networks.eth0 = {
    address = [ "123.45.678.90/24" ];
    gateway = [ "123.45.678.1" ];
    matchConfig.Name = "eth0";
  };
  networking.nameservers = [ "8.8.8.8" ];

  # 关闭 DHCP，手动配置 IP
  networking.useDHCP = false;

  # 首次安装系统时 NixOS 的最新版本，用于在大版本升级时避免发生向前不兼容的情况
  system.stateVersion = "24.05";

  # QEMU（KVM）虚拟机需要使用的内核模块
  boot.initrd.postDeviceCommands = lib.mkIf (!config.boot.initrd.systemd.enable) ''
    # Set the system time from the hardware clock to work around a
    # bug in qemu-kvm > 1.5.2 (where the VM clock is initialised
    # to the *boot time* of the host).
    hwclock -s
  '';

  boot.initrd.availableKernelModules = [
    "virtio_net"
    "virtio_pci"
    "virtio_mmio"
    "virtio_blk"
    "virtio_scsi"
  ];
  boot.initrd.kernelModules = [
    "virtio_balloon"
    "virtio_console"
    "virtio_rng"
  ];
  services = {
    openssh = {
      openFirewall = true;
    };
  };

  disko = {
    # 不要让 Disko 直接管理 NixOS 的 fileSystems.* 配置。
    # 原因是 Disko 默认通过 GPT 分区表的分区名挂载分区，但分区名很容易被 fdisk 等工具覆盖掉。
    # 导致一旦新配置部署失败，磁盘镜像自带的旧配置也无法正常启动。
    enableConfig = false;

    devices = {
      # 定义一个磁盘
      disk.main = {
        # 要生成的磁盘镜像的大小，2GB 足够我使用，可以按需调整
        imageSize = "2G";
        # 磁盘路径。Disko 生成磁盘镜像时，实际上是启动一个 QEMU 虚拟机走一遍安装流程。
        # 因此无论你的 VPS 上的硬盘识别成 sda 还是 vda，这里都以 Disko 的虚拟机为准，指定 vda。
        device = "/dev/vda";
        type = "disk";
        # 定义这块磁盘上的分区表
        content = {
          # 使用 GPT 类型分区表。Disko 对 MBR 格式分区的支持似乎有点问题。
          type = "gpt";
          # 分区列表
          partitions = {
            # GPT 分区表不存在 MBR 格式分区表预留给 MBR 主启动记录的空间，因此这里需要预留
            # 硬盘开头的 1MB 空间给 MBR 主启动记录，以便后续 Grub 启动器安装到这块空间。
            boot = {
              size = "1M";
              type = "EF02"; # for grub MBR
              # 优先级设置为最高，保证这块空间在硬盘开头
              priority = 0;
            };

            # ESP 分区，或者说是 boot 分区。这套配置理论上同时支持 EFI 模式和 BIOS 模式启动的 VPS。
            ESP = {
              name = "ESP";
              # 根据我个人的需求预留 512MB 空间。如果你的 boot 分区占用更大/更小，可以按需调整。
              size = "512M";
              type = "EF00";
              # 优先级设置成第二高，保证在剩余空间的前面
              priority = 1;
              # 格式化成 FAT32 格式
              content = {
                type = "filesystem";
                format = "vfat";
                # 用作 Boot 分区，Disko 生成磁盘镜像时根据此处配置挂载分区，需要和 fileSystems.* 一致
                mountpoint = "/boot";
                mountOptions = [
                  "fmask=0077"
                  "dmask=0077"
                ];
              };
            };

            # 存放 NixOS 系统的分区，使用剩下的所有空间。
            nix = {
              size = "100%";
              # 格式化成 Btrfs，可以按需修改
              content = {
                type = "filesystem";
                format = "btrfs";
                # 用作根分区，Disko 生成磁盘镜像时根据此处配置挂载分区，需要和 fileSystems.* 一致
                mountpoint = "/";
                mountOptions = [
                  "compress-force=zstd"
                  "nosuid"
                  "nodev"
                ];
              };
            };
          };
        };
      };
    };
  };
  fileSystems."/" = {
    device = "/dev/sda3";
    fsType = "btrfs";
    options = [
      "compress-force=zstd"
      "nosuid"
      "nodev"
    ];
  };

  # /boot 分区，是磁盘镜像上的第二个分区。由于我的 VPS 将硬盘识别为 sda，因此这里用 sda3。如果你的 VPS 识别结果不同请按需修改
  fileSystems."/boot" = {
    device = "/dev/sda2";
    fsType = "vfat";
    options = [
      "fmask=0077"
      "dmask=0077"
    ];
  };
}
