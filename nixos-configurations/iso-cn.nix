# 大陆版安装 ISO：在 iso.nix 之上换国内二进制缓存源。
#
# 为什么要单独一份而不是给 iso.nix 加开关：这两个产出面向不同的人，各自烧盘、
# 各自分发。做成一个带 flag 的镜像，烧的时候没人记得传 flag；做成两个产物，
# 拿哪个就是哪个。
#
# 换的只是下载源，信任链不变——narinfo 仍由 cache.nixos.org-1 签名并校验。
# 三个源的实测延迟与取舍见 nixos-modules/cn.nix 的注释。
{ ... }:
{
  imports = [
    ./iso.nix
    ../nixos-modules/cn.nix
  ];
}
