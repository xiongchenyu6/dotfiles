# 大陆版安装 ISO：只留国内可达的 substituter。
#
# 为什么要 mkForce 整个列表，而不是只"加"国内源：
# iso.nix 里那几个 cachix（xddxdd / xiongchenyu6 / hyprland …）在国内**解析不了**，
# 而不通的 substituter 不是没代价——nix 会为**每个包**先超时重试 5 次再放弃：
#
#   unable to download 'https://xddxdd.cachix.org/nix-cache-info':
#   Resolving timed out after 5000 milliseconds; retrying in 97 ms (attempt 1/5)
#
# 真机装机实测卡在这里。加国内源却不去掉不通的，等于白加。
#
# 这里显式列出全部要用的源（含 cache.nixos.org 兜底），因为 mkForce 会连
# nixos 模块用 mkAfter 追加的官方源一起覆盖掉。
#
# 换的只是下载源，信任链不变——narinfo 仍由 cache.nixos.org-1 签名并校验。
# 三个国内源的实测延迟与取舍见 nixos-modules/cn.nix。
{ lib, ... }:
{
  imports = [
    ./iso.nix
    ../nixos-modules/calamares-no-manual.nix
  ];

  nix.settings = {
    substituters = lib.mkForce [
      "https://mirrors.ustc.edu.cn/nix-channels/store"
      "https://mirrors.cernet.edu.cn/nix-channels/store"
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
      "https://cache.nixos.org"
    ];
    trusted-public-keys = lib.mkForce [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
  };
}
