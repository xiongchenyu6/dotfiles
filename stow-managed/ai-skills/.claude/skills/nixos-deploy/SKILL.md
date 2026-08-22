---
name: nixos-deploy
description: dotfiles 仓库(个人主机)的 NixOS/darwin 部署、远程构建、主机迁移、secrets 管理、ISO 构建惯例。用于 nixos-rebuild、部署个人主机、本机太慢要借远程机编译、sops 密钥操作、做安装 ISO 时。autolife 服务器(gz/sg-office 等)的部署也优先用 nixos-rebuild,业务侧惯例见 autolife-ops。
---

# NixOS 部署与迁移(dotfiles 仓库)

主机清单、目录约定见仓库 CLAUDE.md 与 docs/HOSTS.md。

## 铁律

- **NixOS 机器一律用 `nixos-rebuild`,不要用 ansible 绕过去。**只有非 NixOS
  的机器(机器人 Ubuntu、jtti 等)才走 ansible/脚本
- **能进 NixOS 模块的就别手改线上**:caddy/nginx 配置、服务单元、模型服务,
  统统写进模块再 rebuild
- **上游有包就用上游**,别自己重新打包(先查 nixpkgs、`inputs.llm-agents`、
  nur-packages,再考虑自己写)
- 能 systemd 就别 docker;能二进制部署就别容器

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

## 借远程机编译(本机慢/要关机时的标准姿势)

```bash
nixos-rebuild switch --flake .#<host> \
  --use-substitutes --build-host root@<builder> --target-host root@<builder> \
  --impure --keep-failed --show-trace
```

- 大编译(CUDA torch、vLLM、大型 wasm)放算力机上跑,本机随时可关
- 编译是长任务:挂后台 + 定时自检 + 主动汇报进度(见 long-task-babysit skill)
- 国内网络慢就先切国内源:仓库里的 `nixos-modules/cn.nix`

## Secrets 与 yubikey

- 编辑: `sops secrets/<file>.yaml`;每主机有自己的 age key(`.sops.yaml`)
- 新主机要先把 host age key 加进 `.sops.yaml` 对应 creation_rules,
  再 `sops updatekeys secrets/*.yaml`
- **明文本地化模式**:需要频繁读又不算密码的运维数据,存 sops 密文进仓库,
  由 home-manager 在 rebuild 时解到本地明文路径供直接 Read(省 token),
  仓库里始终只有密文
- 有些操作要 yubikey 触碰,做之前先提醒用户插上,别静默卡住
- autolife 相关的 secrets 放 ansible/公司仓库侧的 sops,不放本仓库

## 服务迁移模式(A 机 → B 机)

1. 在 B 上把服务模块 + secrets 配好,部署并验证服务健康
2. 数据有状态的先同步数据,再切 DNS/入口(DNS 走 terraform 仓库,提交即生效)
3. 确认 B 正常服务后,才从 A 的配置里摘除
4. 两台机器的 `.sops.yaml` age key 都要能解到所需 secrets,迁移时同步跑
   `sops updatekeys`

## ISO / 装机镜像

- 出厂 generation 签名 + 启动项要一起做进 ISO,别分两步
- **验证顺序:KVM 先跑通完整装机流程,再烧 U 盘上真机**。KVM 里要确认
  UEFI 模式、网桥/NAT 有外网
- 真机常见坑:U 盘启动被跳过直接进旧系统(检查启动顺序/UEFI 项)、
  自定义分区卡住、无网线只有 wifi 时装机器要能进 GUI 连网——**有线和无线
  两条路都得能装**
- 尽量把要用的东西打进镜像,别让开机后现下载等很久

## 常见坑

- flake 输入更新后,依赖它的第三方 flake(如 niri-flake)可能引用已删除的
  nixpkgs 属性——报错先看是哪个 input 落后了,优先 `nix flake update <input>`
- `flake check` 里 `digital` 等模板主机的报错是既有问题,与当前改动无关
- home-manager 更新后重启机器才暴露的问题(如 ZDOTDIR 链断),先对比新旧
  generation 的生成文件再下结论
- 卡在某个 `.drv` 构建时先确认是不是没有二进制缓存的大包(如 noctalia-shell、
  CUDA torch),该等就等/该转远程构建就转,别反复重试
- 脚本产物(装机脚本、install 脚本)放对象存储给别人 curl 用时,**注意编码**:
  必须 UTF-8 且服务端 Content-Type 带 charset,浏览器下载看到乱码要一路查到
  上传环节,不能只在本地看没问题就下结论
