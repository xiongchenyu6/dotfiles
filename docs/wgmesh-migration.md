# wg-quick 网状网 → wgmesh 迁移计划

## 目标形态(一句话)

**Linux NixOS 主机之间的私有流量走 wgmesh(P2P 直连 + oracle-amd-002 中继);
DN42 的 eBGP 隧道和内部路由面继续用 wg-quick,两套平面并存、互不依赖。**

wgmesh 不是 DN42 隧道的替代品:DN42 对等是双方手写 pubkey/endpoint 的静态隧道,
而且要用 IPv6 link-local、组播(babel `ff02::1:6`)和任意前缀的 AllowedIPs。
wgmesh 是纯 IPv4 overlay,每个 peer 只能拿 `/32`,这些都承载不了。所以"DN42 不断"
的做法是 **迁移期间完全不碰 DN42 平面**,而不是想办法把 DN42 塞进 wgmesh。

## 现状(2026-10-19 实测)

| 项 | 状态 |
|---|---|
| hub `oracle-amd-002` `wg0` (UDP 22616, 172.22.240.97/27) | 正常;game、mac-pro、.101、.102 有握手 |
| `office` (.98)、`.100` peer | 从未握手(`latest-handshake 0`) |
| `office` 和 `office-mac` | **共用同一把 `wireguard/office` 私钥** —— 同一时间只能有一台在线 |
| `oracle-amd-001` | 暂存区里新增了 `wg_ora` 但还没部署(机器上没有 `wg`) |
| DN42 外部 `wg_kioubit`(在 amd-002 上) | **从未握手**;amd-002 没有跑 bird(`bird-border` 只 import 在 tcloud 上) |
| `tcloud`(配置里的 DN42 border) | SSH 被拒(publickey),状态未知 |
| game 上 `172.20.0.53` (DN42 DNS) | 不通;另外 docker 网桥占了 `172.20.0.0/16`、`172.21.0.0/16`,与 DN42 冲突 |
| game 上已占用的网段 | LAN 10.171.x、docker 10.90/24、podman 10.88/16、netbird `wt0` 100.98/16、sing-box 规则 100.64/10 |
| Oracle VCN | 10.0.0.0/24(amd-001 ↔ amd-002 走内网) |

结论:**DN42 对外现在大概率已经断了**,内部 /27 hub 是通的。所以"不断"要以
Stage 0 先修好的基线为准,否则没法判断迁移有没有把它搞坏。

## wgmesh 的形态(2026-10-19 重写)

wgmesh 仓库已经不再有自己的 Rust 守护进程,只剩一个 NixOS 模块:

- `networking.wireguard.interfaces.wgmesh` 建接口,唯一的静态 peer 是中继 amd-002,AllowedIPs 是整个 `10.42.0.0/24`
- nixpkgs 里的 `services.wgautomesh` 负责其余节点:gossip 交换 endpoint、局域网广播发现、经中继看到的地址双向打洞
- 一个约 20 行的补丁给 wgautomesh 加了 `relay_fallback`:握手新鲜时才给对端 `/32`,否则流量走中继的 `/24`
- 节点表 = `shares.toml` 里带 `mesh-ip` 的主机;密钥复用已有的 `wireguard/<host>`(sops);gossip 密钥 `wgmesh/gossip-secret`
- dotfiles 侧入口:`nixos-modules/wgmesh.nix`

原计划里的 coordinator、nginx 虚拟主机 `mesh.panda.qzz.io`、SSH host key 派生密钥、`staticIPs`、`skipInterfaces` 全部随 Rust 代码一起删除。

已知限制:只做 IPv4、单中继、wgautomesh 30 秒一轮(新直连最多 30 秒两端才稳定),
对端消失后直连路由最多保留 5 分钟才回落到中继。

## 地址与端口规划

- mesh CIDR:`10.42.0.0/24`。DN42 的 import filter 已拒收 `10.42.0.0/16+`。
- 地址:amd-002(中继)`.1`、amd-001 `.10`、game `.20`、office `.21`。
- 接口 `wgmesh`,WireGuard UDP 51821(51820 是桌面机上 netbird `wt0` 的;OCI 安全列表放行 51821,实测),gossip UDP 1666。
- 中继 endpoint `213.35.117.232:51821`;amd-001 同 VCN,改拨 `10.0.0.246:51821`。

## Stage 0: DN42 基线 + 前置清理
**Goal**: 先把要"保持不断"的东西修到已知正常,再把 wgmesh 的两个小功能做完。
**Success Criteria / 进度**(2026-10-19):
- [x] 根因:4 月把 hub(含 `wg_kioubit`)从 tcloud 迁到 amd-002 时 bird 没跟过来,隧道上 0 字节
- [x] amd-002 跑 `bird-border` + `dn42`,ROA 已加载(v4 2801 / v6 2900),已上线
- [x] tcloud 已被重装成 Ubuntu(SSH banner `OpenSSH_9.6p1 Ubuntu`),上面没有我们的 bird;NixOS 配置里也已摘掉 bird
- [x] import filter 拒收 `10.42.0.0/16+`(已上线)
- [x] 顺带修复:amd-002 的 NAT `externalInterface` 是不存在的 `ens5` → `ens3`(已上线)
- [x] game/office 改为经 `wg_ora` 的静态 DN42 路由(原 bird-inner/babel 绑在不存在的 `wg_mail` 上,是死配置,已删);game 已上线
- [x] game docker `default-address-pools` → 10.201.0.0/16(已上线),占 DN42 段的空闲网络已删
- [x] office-mac 有了独立私钥 `wireguard/office-mac` 和地址 172.22.240.105 / fd48:4b4:f3::9;hub 侧 peer 已上线,
      office-mac 本机 `darwin-rebuild switch` 待机器上线后执行
- [ ] **Kioubit peering 重建(需要本人登录 Kioubit 后台)**:对端 `hk1.g-load.eu:22616` 回 ICMP port unreachable,peering 已被对方删除。
      填写:AS4242422616,我方 WG 公钥 `9TXI2YQ0cdhW3xBhxzuHpPuISR7k2NwTjZ2Sq/lwoE0=`,
      我方 endpoint 留空(我方主动拨号 + keepalive),link-local `fe80::100`,MP-BGP + extended next hop
**Tests**: `birdc show proto all kioubit_v6` 为 Established、game 上 `ping 172.20.0.53`、
`drill @172.20.0.53 burble.dn42`、`wg show wg_kioubit latest-handshakes` 不为 0。
**Status**: In Progress(只剩 Kioubit 后台这一步)

## Stage 1: amd-002 上线中继
**Goal**: `wgmesh` 接口和中继在 hub 上与 `wg0`/`wg_kioubit` 并存运行。
**Success Criteria**: `wg show wgmesh` 监听 51821,`wgautomesh` active,`wg0` 握手不变,bird 不受影响。
**Tests**: 同上;DN42 检查清单通过。
**Status**: Complete(2026-10-19:`wgmesh` 监听 51821,`wgautomesh` active,`wg0`/bird 照常)

## Stage 2: oracle-amd-001 接入
**Goal**: 第一个普通节点,经 VCN 内网直连中继。
**Success Criteria**: amd-001 `ping 10.42.0.1` 通;hub 上 amd-001 的 endpoint 是 10.0.0.x。
**Tests**: 同上。
**Status**: Complete(hub 上 amd-001 endpoint `10.0.0.199:51821`,互 ping 通)

## Stage 3: game、office 接入
**Goal**: 桌面机加上 wgmesh,`wg_ora` 原样保留。
**Success Criteria**:
- game、office 都能 `ping 10.42.0.1` 和其他节点
- game ↔ office 在同一局域网时走直连(对方 peer 有 `/32`,endpoint 是 LAN 地址)
**Tests**: `wg show wgmesh allowed-ips`、`wg show wgmesh endpoints`;DN42 检查清单通过。
**Status**: In Progress(game 已接入,经中继 ping 通 .1/.10;office 离线,配置已就绪,上线后 `nixos-rebuild switch --flake .#office`)

## Stage 4: 业务流量切到 wgmesh 并观察一周
**Goal**: 所有写死旧 mesh IP 的地方改用新地址,旧地址只留给 DN42。
**Success Criteria**:
- `home-configurations/freeman.xiong.nix` 里 ssh matchBlocks 的 `office`/`game` HostName → 10.42.0.x
- Sunshine `csrf_allowed_origins` → `https://10.42.0.20:47990`
- `nixos-modules/bind.nix` 的 inner 区 `office`/`game` A 记录 → 10.42.0.x
- 防火墙的业务端口只留 `interfaces.wgmesh`(`wg_ora` 只放 babel/BGP)
- 观察 7 天:`wgautomesh` 没有重启循环,`journalctl -u wgautomesh` 里同一个 peer 没有反复 `Configure … with endpoint`
**Tests**: `ssh game`、`ssh office` 走的是 10.42 地址;Moonlight 推流正常;DN42 检查清单通过。
**Status**: Not Started

## Stage 5: 收缩旧 mesh
**Goal**: wg-quick 只保留 wgmesh 做不了的部分。
**Success Criteria**:
- hub 的 `wg0` 只留:mac-pro、office-mac、非 NixOS 的 .101/.102,以及仍要接入 DN42 的内部主机
- 删掉从未握手的 peer(.100,以及 Stage 0 之后仍然离线的)
- 如果决定 game/office 不再需要 DN42 地址:去掉它们的 `wg_ora`、`bird-inner`、`dn42` 模块,
  `wireguard/game`、`wireguard/office` secret,shares 里的 wg 公钥 —— 直接删,不留兼容层
- 更新 `docs/HOSTS.md`;等所有 Stage 完成后删掉本文件
**Tests**: `nix flake check`;每台主机 `nixos-rebuild build` 都过;DN42 检查清单通过;macOS 上的 `wg_ora` 正常。
**Status**: Not Started

## 回滚

wgmesh 是新增的并行平面(新接口名、新网段、新端口)。回滚就是把对应主机的 `ezModules.wgmesh`
import 去掉再 rebuild;旧的 `wg_ora` 在 Stage 5 之前一直完整可用。中继挂了不影响已经建好的直连
(内核 WireGuard 不依赖它),只是直连不通的节点对之间暂时不通。

## 默认采用的假设(不同意的话改这里)

1. game/office 在本次迁移中**保留** DN42 内网身份(Stage 5 再决定要不要去掉)。
2. 两台 Mac 留在 wg-quick 上(wgautomesh 只支持 Linux)。
3. 中继和现有 hub 放在同一台 amd-002 上。
4. mesh 用 10.42.0.0/24,不从自己的 DN42 /27 里切。
