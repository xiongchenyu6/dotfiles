# 让 Calamares 生成的 configuration.nix 关掉 NixOS 手册。
#
# 装机实测：93% 时 nixos-manual-html 构建崩掉（exit 139 = SIGSEGV），
# 连带 system-path 依赖失败，整个安装失败。
#
# 这个包**必须本地构建**——手册内容取决于这台机器的配置，任何 binary cache 里都
# 没有它，所以国内源、离线包都救不了。而它是整个装机里最重的一个构建，在内存不大
# 的机器上很容易崩。
#
# 装完之后想要手册，`nixos-help` 在线版、`man configuration.nix` 都还在；真要本地
# 手册，改 configuration.nix 打开这个选项再 rebuild 一次即可——那时是在装好的系统
# 上跑，不会把装机卡死。
{ ... }:
{
  nixpkgs.overlays = [
    (final: prev: {
      calamares-nixos-extensions = prev.calamares-nixos-extensions.overrideAttrs (old: {
        postPatch = (old.postPatch or "") + ''
          substituteInPlace modules/nixos/main.py \
            --replace-fail \
              '      ./hardware-configuration.nix
    ];
' \
              '      ./hardware-configuration.nix
    ];

  # 本地构建 nixos-manual-html 很重，且任何 binary cache 里都没有（内容取决于本机
  # 配置）。装机时它崩过一次（exit 139），所以默认关掉。要的话改成 true 再 rebuild。
  documentation.nixos.enable = false;
'
        '';
      });
    })
  ];
}
