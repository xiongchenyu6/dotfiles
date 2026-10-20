# wg-quick 网状网 → wgmesh 迁移计划

## 目标形态(一句话)

**自己 DN42 段 172.22.240.96/27 的 IPv4 在 Linux NixOS 主机之间走 wgmesh(P2P 直连 +
oracle-amd-002 中继,中继同时是 DN42 border,DN42 IPv4 经它转发);IPv6 和 Mac 继续走
wg-quick。地址一个不改,将来正式接入 DN42 时网段和 IP 原样可用。**

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

- `networking.wireguard.interfaces.wgmesh` 建接口,唯一的静态 peer 是中继 amd-002,AllowedIPs 是整个 mesh 段加 `relayPrefixes`
- nixpkgs 里的 `services.wgautomesh` 负责其余节点:gossip 交换 endpoint、局域网广播发现、经中继看到的地址双向打洞
- 一个约 20 行的补丁给 wgautomesh 加了 `relay_fallback`:握手新鲜时才给对端 `/32`,否则流量走中继的 `/24`
- `relayPrefixes` 让普通节点把 mesh 外的网段(这里是 DN42 `172.20.0.0/14`)也经中继路由
- 节点表 = `shares.toml` 里带 `mesh-ip` 的主机;密钥复用已有的 `wireguard/<host>`(sops);gossip 密钥 `wgmesh/gossip-secret`
- dotfiles 侧入口:`nixos-modules/wgmesh.nix`

原计划里的 coordinator、nginx 虚拟主机 `mesh.panda.qzz.io`、SSH host key 派生密钥、`staticIPs`、`skipInterfaces` 全部随 Rust 代码一起删除。

已知限制:只做 IPv4、单中继、wgautomesh 30 秒一轮(新直连最多 30 秒两端才稳定),
对端消失后直连路由最多保留 5 分钟才回落到中继。

## 地址与端口规划

- mesh CIDR 就是自己的 DN42 段 `172.22.240.96/27`,**地址沿用原有分配,不改**:
  amd-002(中继)`.97`、office `.98`、game `.99`、amd-001 `.104`。
- hub 上 `/27` 归 `wgmesh`;`wg0` 地址收成 `172.22.240.97/32`,只给 Mac(.103/.105)和
  非 NixOS 客户端(.100–.102)各一条 `/32` 路由。三台 NixOS 主机在 `wg0` 上只剩 IPv6。
- 普通节点的 `wg_ora` 只保留 IPv6(`fd48:4b4:f3::/48`、DN42 `fd00::/8`);DN42 IPv4 走 wgmesh 的中继 peer。
- Mac 还是 wg-quick 连 hub,hub 在 `wg0` 和 `wgmesh` 之间直接转发(同一个 /27,不需要 NAT)。
- 接口 `wgmesh`,WireGuard UDP 51821(51820 是桌面机上 netbird `wt0` 的;OCI 安全列表放行 51821,实测),gossip UDP 1666。
- 中继 endpoint `213.35.117.232:51821`;amd-001 同 VCN,改拨 `10.0.0.246:51821`。

## Stage 0: DN42 基线 + 前置清理
**Goal**: 先把要"保持不断"的东西修到已知正常,再把 wgmesh 的两个小功能做完。
**Success Criteria / 进度**(2026-10-19):
- [x] 根因:4 月把 hub(含 `wg_kioubit`)从 tcloud 迁到 amd-002 时 bird 没跟过来,隧道上 0 字节
- [x] amd-002 跑 `bird-border` + `dn42`,ROA 已加载(v4 2801 / v6 2900),已上线
- [x] tcloud 已被重装成 Ubuntu(SSH banner `OpenSSH_9.6p1 Ubuntu`),上面没有我们的 bird;NixOS 配置里也已摘掉 bird
- [x] 顺带修复:amd-002 的 NAT `externalInterface` 是不存在的 `ens5` → `ens3`(已上线)
- [x] game/office 改为经 `wg_ora` 的静态 DN42 路由(原 bird-inner/babel 绑在不存在的 `wg_mail` 上,是死配置,已删);game 已上线
- [x] game docker `default-address-pools` → 10.201.0.0/16(已上线),占 DN42 段的空闲网络已删
- [x] office-mac 有了独立私钥 `wireguard/office-mac` 和地址 172.22.240.105 / fd48:4b4:f3::9;hub 侧 peer 已上线,
      (office-mac 已于 2026-10-20 出售,该私钥和地址已删除)
- [ ] **(暂缓,先做自己的机器)Kioubit peering 重建(需要本人登录 Kioubit 后台)**:对端 `hk1.g-load.eu:22616` 回 ICMP port unreachable,peering 已被对方删除。
      填写:AS4242422616,我方 WG 公钥 `9TXI2YQ0cdhW3xBhxzuHpPuISR7k2NwTjZ2Sq/lwoE0=`,
      我方 endpoint 留空(我方主动拨号 + keepalive),link-local `fe80::100`,MP-BGP + extended next hop
**Tests**: `birdc show proto all kioubit_v6` 为 Established、game 上 `ping 172.20.0.53`、
`drill @172.20.0.53 burble.dn42`、`wg show wg_kioubit latest-handshakes` 不为 0。
**Status**: In Progress(只剩 Kioubit 后台这一步,已暂缓)

## Stage 1: amd-002 上线中继
**Goal**: `wgmesh` 接口和中继在 hub 上与 `wg0`/`wg_kioubit` 并存运行。
**Success Criteria**: `wg show wgmesh` 监听 51821,`wgautomesh` active,`wg0` 握手不变,bird 不受影响。
**Tests**: 同上;DN42 检查清单通过。
**Status**: Complete(2026-10-19:`wgmesh` 监听 51821,`wgautomesh` active,`wg0`/bird 照常)

## Stage 2: oracle-amd-001 接入
**Goal**: 第一个普通节点,经 VCN 内网直连中继。
**Success Criteria**: amd-001 `ping 172.22.240.97` 通;hub 上 amd-001 的 endpoint 是 10.0.0.x。
**Tests**: 同上。
**Status**: Complete(hub 上 amd-001 endpoint `10.0.0.199:51821`,互 ping 通;ib-gateway 4002 绑在 wgmesh 的 `.104` 上,game 可连)

## Stage 3: game、office 接入
**Goal**: 桌面机加上 wgmesh,`wg_ora` 原样保留。
**Success Criteria**:
- game、office 都能 `ping 172.22.240.97` 和其他节点
- game ↔ office 在同一局域网时走直连(对方 peer 有 `/32`,endpoint 是 LAN 地址)
**Tests**: `wg show wgmesh allowed-ips`、`wg show wgmesh endpoints`;DN42 检查清单通过。
**Status**: Complete(game 已接入:ping 通 .97/.104,经 hub 到 Mac `.103:22` 通;office 于 2026-10-20 出售,不再接入)

## Stage 4: 业务端口收到 wgmesh 并观察一周
**Goal**: 地址不变,业务端口只在 `wgmesh` 上开。
**Success Criteria**:
- [x] game(Sunshine、ssh 等)、office(2222)、amd-001(4002)的防火墙端口从 `wg_ora` 挪到 `interfaces.wgmesh`
- [x] ssh matchBlocks、Sunshine `csrf_allowed_origins` 用的就是原地址,无需改
- [ ] 观察 7 天(到 2026-10-26):`wgautomesh` 没有重启循环,`journalctl -u wgautomesh` 里同一个 peer 没有反复 `Configure … with endpoint`。
  已知例外:mac-pro 在 SG 办公室 NAT 后面端口会变,它的直连会周期性重新握手;mac-pro↔sg-office 永久走中继。
- [x] 各节点接口上残留的 office / office-mac 两个 peer 已手动 `wg set … remove`(hub、game、amd-001、sg-office、mac-pro,2026-10-20)
**Tests**: Mac 上 `ssh game` 正常;Moonlight 推流正常;DN42 检查清单通过。
**Status**: In Progress(观察期 2026-10-19 → 2026-10-26)

## Stage 5: 收缩旧 mesh
**Goal**: wg-quick 只保留 wgmesh 做不了的部分。
**Success Criteria**:
- hub 的 `wg0` 已经只给 mac-pro、office-mac、.100–.102 留 IPv4;剩下决定 IPv6 是否也要挪
- 删掉从未握手的 peer(.100,以及 Stage 0 之后仍然离线的)
- 如果决定 game/office 不再需要 DN42 地址:去掉它们的 `wg_ora`、`bird-inner`、`dn42` 模块,
  `wireguard/game`、`wireguard/office` secret,shares 里的 wg 公钥 —— 直接删,不留兼容层
- 更新 `docs/HOSTS.md`;等所有 Stage 完成后删掉本文件
**Tests**: `nix flake check`;每台主机 `nixos-rebuild build` 都过;DN42 检查清单通过;macOS 上的 `wg_ora` 正常。
**进度**(2026-10-19):
- [x] game、mac-pro、oracle-amd-001 删掉 `wg_ora`;mac-pro 跑 nix-darwin 的 wgautomesh
- [x] 手机 `.101` 改成中继上的静态 wgmesh peer(endpoint 端口改 51821)
- [x] sg-office `.102` 在 workRobot/nixos 里接入(直接 import 本仓库 `shared-modules/wgmesh.nix`),已部署
- [x] hub 的 `wg0` 整个删掉;`.100` 从未握手,不迁移
- [x] 直连实测:game↔mac-pro、game↔oracle-amd-001(给 amd-001 加了公网 `mesh-endpoint`,之前中继只会 gossip 它的 VCN 地址)
- [x] office、office-mac 于 2026-10-20 二手卖掉:主机配置、shares 条目、sops 收件人、`wireguard/{office,game-office,office-mac}` 私钥全部删除,secrets 重新加密
- [x] sg-office 的 dotfiles lock bump 到 8b52af60;sg-office↔game、sg-office↔oracle-amd-001 直连(3–4 ms)
- [x] mac-pro 直连 sg-office、oracle-amd-001(对端 `/32` 指向 Mac 的公网 endpoint,TCP 22 通;Mac 开了防火墙隐身模式,入站 ping 不回)
- mac-pro 在广州办公室(LAN 10.1.63.x),mesh 延迟约 260 ms 是跨境链路本身。它的 WireGuard UDP 经公司
  网络从新加坡侧 NAT(203.116.95.146)出去,且该 NAT 的端口映射会变,直连会周期性抖到下一次握手。
  试过给 Clash 加 UDP 51821 DIRECT 规则:直出变成广州本地出口(112.94.x),握手全断,不要再加
- mac-pro↔sg-office 直连不通:Mac 发往 101.78.126.6:51821 或 10.31.16.114:51821 的 UDP(直出、经代理、
  经公司内网三条路都试过)一个都到不了 sg-office,反向拨 Mac 的 NAT 口也被过滤。TCP(ssh)是通的,
  只有 UDP 不行,属于公司网络问题。这一对永久走 hub 中继,靠 `relay_fallback` 兜底
- [x] 修了 `relay_fallback` 补丁的一个洞(wgmesh 88609a7):上游对"候选 endpoint 和当前一样"的掉线 peer
  直接跳过,单候选的 peer 就永远留着连通时装的 `/32`,中继回退不生效——经 hub 转来的包被 wireguard-go
  按来源丢掉。现在跳过时也清 `/32`;VM 测试新增"掐断直连后 300 s 内回退到中继"的子测试。
  game/hub/amd-001/mac-pro/sg-office 都已部署(2026-10-20;sg-office 经 workRobot/nixos f2c4f3a 手动 switch)
**Status**: Complete(2026-10-20:hub 上只剩 `wgmesh` 和 `wg_kioubit`;office、office-mac 已出售并从仓库删除)

## 回滚

wgmesh 是新增的并行平面(新接口名、新网段、新端口)。回滚就是把对应主机的 `ezModules.wgmesh`
import 去掉再 rebuild;旧的 `wg_ora` 在 Stage 5 之前一直完整可用。中继挂了不影响已经建好的直连
(内核 WireGuard 不依赖它),只是直连不通的节点对之间暂时不通。

## 默认采用的假设(不同意的话改这里)

1. 所有主机保留原有 DN42 段地址(v4 /27、v6 fd48:4b4:f3::/48),不重新编址。
2. Mac 也跑 wgautomesh(它只调 `wg`,在 wireguard-go 上可用);手机不能跑,是中继上的静态 peer。
3. 中继和现有 hub 放在同一台 amd-002 上。
