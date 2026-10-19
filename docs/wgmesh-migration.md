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

## wgmesh 当前的能力缺口(决定了计划怎么排)

| 缺口 | 影响 | 处理 |
|---|---|---|
| 只有 Linux agent(`ip`/`wg` + NixOS 模块) | mac-pro、office-mac 接不进来 | 留在旧 hub 的 `wg0` 上(见最终形态) |
| 只有 IPv4、`/32` AllowedIPs、没有子网路由 | 跑不了 babel/BGP/DN42 | DN42 平面保持 wg-quick |
| IP 按注册顺序分配(`store.rs` 里的 `allocate_locked`) | mesh IP 不在 git 里,ssh/bind/CSRF 写死的 IP 没法提前定 | **wgmesh 加功能 A**:按 SSH pubkey 静态预留 IP |
| `skipInterfaces` 只能精确匹配 | `br-<hash>`、`veth*`、`wt0`、`wg_ora` 的地址会被当成 LAN 候选上报;每个坏候选要白等 60 秒 probe 窗口 | **wgmesh 加功能 B**:按前缀匹配(`br-`、`veth`、`docker`、`virbr`、`wg`、`wt`、`podman`) |
| 默认接口名 `wg0` | 和 amd-002 现有的 hub `wg0` 冲突 | 两端都用 `interface = "wgmesh"` |

功能 A、B 都很小,按 wgmesh 仓库的惯例写:先写单元测试,`cargo test --workspace`,
需要碰内核的部分进 `tests/kernel-mesh.sh`,README 中英文两份同步更新。

## 地址与端口规划

- mesh CIDR:`10.42.0.0/24`(wgmesh 默认值,和上面所有已占用网段都不冲突)。
  在 10.0.0.0/8 里,而 DN42 过滤器允许 `10.0.0.0/8{15,24}`,所以 border 的 import filter
  要加 `if net ~ [10.42.0.0/16+] then reject;`,防止 DN42 里有人宣告这段把它劫持走。
- 预留(功能 A):coord `10.42.0.1`、amd-001 `.10`、game `.20`、office `.21`、
  后续主机按需加。
- 接口 `wgmesh`,WG 端口 UDP 51820(和 22616 不冲突)。
- 控制面:coord 监听 `127.0.0.1:8443`(明文 HTTP),外面套 nginx 虚拟主机
  `mesh.panda.qzz.io`,用 amd-002 上已有的 `*.panda.qzz.io` 通配证书
  (`useACMEHost`)→ TCP 不用新开端口,只走 443。
- 主机身份:`shares.toml` 里新增 `hosts.<name>.ssh-host-key`(即 `/etc/ssh/ssh_host_ed25519_key.pub`)。
  coord 的 `authorizedSigners` 和 IP 预留都从这一个字段生成,做到单一事实来源。

## Stage 0: DN42 基线 + 前置清理
**Goal**: 先把要"保持不断"的东西修到已知正常,再把 wgmesh 的两个小功能做完。
**Success Criteria**:
- amd-002 上 bird 在跑,`birdc show proto` 里 `kioubit_v6` 是 `Established`
  (把 `bird-border` + `dn42` 从 tcloud 挪到实际持有 `wg_kioubit` 的 amd-002,
  或者确认 tcloud 才是 border、把隧道挪回去 —— 二选一,配置和运行状态要对上)
- game 上 `ping 172.20.0.53` 能通;docker `default-address-pools` 挪出 172.20.0.0/14
- office / office-mac 各用一把独立的 WG 私钥
- wgmesh 功能 A、B 合入,CI 通过
**Tests**: `birdc show proto all kioubit_v6`、`ping 172.20.0.53`、`drill @172.20.0.53 burble.dn42`、
`wg show wg_kioubit latest-handshakes` 不为 0;wgmesh 侧 `cargo test --workspace --locked` + `tests/kernel-mesh.sh`。
记录下这份 DN42 检查清单,后面每个 Stage 结束都重跑一遍。
**Status**: Not Started

## Stage 1: amd-002 上并行起 coordinator
**Goal**: wgmesh 控制面 + 中继在 hub 上和现有 `wg0`/`wg_kioubit` 并存运行。
**Success Criteria**:
- `flake.nix` 加 input `wgmesh`(`inputs.nixpkgs.follows = "nixpkgs"`)
- amd-002 配置 `services.wgmesh-coord = { enable; interface = "wgmesh"; meshCidr = "10.42.0.0/24"; listenAddr = "127.0.0.1:8443"; authorizedSigners = …; }`
- nginx 虚拟主机 `mesh.panda.qzz.io` 反代到 127.0.0.1:8443;DNS 加 A 记录指向 hub 公网 IP
- NixOS 防火墙放行 UDP 51820,**OCI 安全列表也要放行 UDP 51820**(需要在 OCI 控制台/CLI 操作)
- border import filter 加上 10.42/16 的 reject
- `wg0` / `wg_kioubit` / bird 完全不动
**Tests**: `curl https://mesh.panda.qzz.io/healthz`;`wg show wgmesh` 有接口、端口是 51820;
`wg show wg0 latest-handshakes` 和改动前一致;DN42 检查清单通过。
**Status**: Not Started

## Stage 2: 试点 oracle-amd-001
**Goal**: 第一个 agent 选还没接入旧 mesh 的主机 —— 出问题零影响。
**Success Criteria**:
- 用 `services.wgmesh = { enable; coordinator = "https://mesh.panda.qzz.io"; interface = "wgmesh"; }`
  替换暂存区里新加的 `wg_ora` 那段(以及 `wireguard/oracle-amd-001` secret 和 shares 里的 wg 公钥)
- IB Gateway 4002 改成 `interfaces.wgmesh.allowedTCPPorts`
- 两台在同一个 VCN 里 → 应该通过 LAN 候选(10.0.0.x)直连 hub
**Tests**: `ping 10.42.0.1`;hub 上 `wg show wgmesh endpoints` 看到 amd-001 的 10.0.0.x;
从 hub 访问 `nc -z 10.42.0.10 4002`;重启 coord 后 amd-001 的 IP 不变。
**Status**: Not Started

## Stage 3: game、office 双栈运行
**Goal**: 桌面机加上 wgmesh agent,`wg_ora` + babel + bird 原样保留。
**Success Criteria**:
- 两台都跑起 agent(`skipInterfaces` 用功能 B 的前缀列表)
- game ↔ office 走 **DIRECT**(以前所有流量都绕道 Oracle hub;这是这次迁移最大的收益)
- 需要的服务在 `wg_ora` 和 `wgmesh` 两个接口上都放行(SSH 22、Sunshine 端口、office 的 2222)
**Tests**: `wg show wgmesh` 里对方 peer 的 AllowedIPs 是 `/32` 且 endpoint 是对方的公网/LAN 地址;
`ping 10.42.0.21` 的 RTT 比经 hub 的 `ping 172.22.240.98` 低;Moonlight 能经 10.42.0.20 推流;
把 hub 上的 `wgmesh` 关掉,已有的直连不受影响;DN42 检查清单通过。
**Status**: Not Started

## Stage 4: 业务流量切到 wgmesh 并观察一周
**Goal**: 所有写死旧 mesh IP 的地方改用新地址,旧地址只留给 DN42。
**Success Criteria**:
- `home-configurations/freeman.xiong.nix` 里 ssh matchBlocks 的 `office`/`game` HostName → 10.42.0.x
- Sunshine `csrf_allowed_origins` → `https://10.42.0.20:47990`
- `nixos-modules/bind.nix` 的 inner 区 `office`/`game` A 记录 → 10.42.0.x
- 防火墙的业务端口只留 `interfaces.wgmesh`(`wg_ora` 只放 babel/BGP)
- 观察 7 天:agent/coord 没有重启循环,`journalctl -u wgmesh-agent` 没有 DIRECT↔RELAY 来回抖动
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

每个 Stage 都是新增并行平面(新接口名、新网段、新端口),回滚就是对相关主机设
`services.wgmesh.enable = false` 再 rebuild;旧的 `wg_ora` 在 Stage 5 之前一直是完整可用的。
coord 挂了也不影响已经建好的直连隧道(内核 WG 不依赖 coord),只是新节点加不进来、
endpoint 变化无法同步。每次部署前备份 `/var/lib/wgmesh-coord/state.json`
(有了功能 A,丢了也能按预留重建)。

## 默认采用的假设(不同意的话改这里)

1. game/office 在本次迁移中**保留** DN42 内网身份(Stage 5 再决定要不要去掉)。
2. 两台 Mac 留在 wg-quick 上;给 wgmesh 做 macOS agent(wireguard-go + `ifconfig`/`route`)是另一个项目。
3. coord 和现有 hub 放在同一台 amd-002 上;可用内存约 157 MB,coord 常驻约 3.5 MB,够用。
4. mesh 用 10.42.0.0/24,不从自己的 DN42 /27 里切。
