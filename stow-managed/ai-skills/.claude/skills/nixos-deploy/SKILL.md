---
name: nixos-deploy
description: dotfiles 仓库(个人主机)的 NixOS/darwin 部署、主机迁移、secrets 管理惯例。用于 nixos-rebuild、部署个人主机、sops 密钥操作时。autolife 服务器(gz/sg-office 等)的部署不用本 skill,用 autolife-ops。
---

# NixOS 部署与迁移(dotfiles 仓库)

主机清单、目录约定见仓库 CLAUDE.md。

## 部署流程

1. **先求值再切换**——切换前必须确认能构建,别让用户拿密码去试错:
   ```bash
   nix build .#nixosConfigurations.<host>.config.system.build.toplevel --no-link
   ```
2. 本机切换需要 sudo,让用户自己跑(提示用 `! ` 前缀在会话内执行):
   ```bash
   sudo nixos-rebuild switch --flake .#<host>
   ```
3. 远程主机直接推:
   ```bash
   nixos-rebuild switch --flake .#<host> --target-host root@<host名>
   ```
   SSH 主机名优先用 ssh config 里的名字,不用裸 IP
4. macOS: `darwin-rebuild switch --flake .#office-mac`(整机求值在 Linux 上
   会因平台不支持而失败,属正常;验证单个 option 即可)

## 服务迁移模式(A 机 → B 机)

1. 在 B 上把服务模块 + secrets 配好,部署并验证服务健康
2. 数据有状态的先同步数据,再切 DNS/入口
3. 确认 B 正常服务后,才从 A 的配置里摘除
4. 两台机器的 `.sops.yaml` age key 都要能解到所需 secrets,迁移时同步跑
   `sops updatekeys`

## Secrets

- 编辑: `sops secrets/<file>.yaml`;每主机有自己的 age key(`.sops.yaml`)
- autolife 相关的 secrets 放 ansible 侧的 sops,不放本仓库
- 新主机要先把 host age key 加进 `.sops.yaml` 对应 creation_rules,
  再 `sops updatekeys secrets/*.yaml`

## 常见坑

- flake 输入更新后,依赖它的第三方 flake(如 niri-flake)可能引用已删除的
  nixpkgs 属性——报错先看是哪个 input 落后了,优先 `nix flake update <input>`
- `flake check` 里 `digital` 等模板主机的报错是既有问题,与当前改动无关
- home-manager 更新后重启机器才暴露的问题(如 ZDOTDIR 链断),先对比新旧
  generation 的生成文件再下结论
