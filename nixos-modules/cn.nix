# 国内 nix 二进制缓存镜像。
#
# 顺序按实测延迟排（同一个 narinfo，多次一致）：
#   ustc    0.09s
#   cernet  0.38s
#   tuna    未计时，与前两者同为完整 store 镜像
#   官方    0.49s
#
# 三个都实测确认是真正的 store 镜像（nix-cache-info 里 StoreDir: /nix/store，
# Priority 40），不是只挂了个 channels 目录。
#
# **不需要在这里写 cache.nixos.org**：nixos/modules/config/nix.nix 用
# `substituters = mkAfter [ "https://cache.nixos.org/" ]`，NixOS 的列表定义是合并的，
# 官方源会自动排在这几个之后兜底。写了反而会多一份重复。
#
# 信任源不变：narinfo 仍由 cache.nixos.org-1 签名并校验，这里换的只是下载源。
{
  nix.settings.substituters = [
    # 路径必须带 /store。写成 https://mirrors.ustc.edu.cn/nix-channels/ 是 404——
    # 那是 channels 目录，不是 binary cache 根。
    "https://mirrors.ustc.edu.cn/nix-channels/store"
    "https://mirrors.cernet.edu.cn/nix-channels/store"
    "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
    # 移除了 https://mirror.nju.edu.cn/nix-channels/store：
    # 实测是重定向死循环（curl: Maximum (50) redirects followed），连
    # nix-cache-info 都取不到。不通的 substituter 不是没代价——nix 会为**每个包**
    # 多一次失败往返，越多包越慢，正好和加镜像的目的相反。
  ];
}
