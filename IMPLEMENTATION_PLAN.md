# 公私分离：autolife 资产迁出 dotfiles

**目标**：`dotfiles` 成为纯个人仓库（可公开），autolife 公司资产收敛到
`github.com/AutoLifeRobot/nixos`（私有）。

## 前置认知

目标仓库 **不是**一个独立配置仓库，而是 dotfiles 的薄封装：它把
`github:xiongchenyu6/dotfiles` 当作 flake input，模块几乎全部从
`inputs.dotfiles.nixosModules.*` 拉取，本地只留 sg-office 的硬件/公司特有部分
（`nixos-modules/{kernel,security,ssh-harden,virtualisation}.nix` 都已经退化成 4 行转发 shim）。

**这意味着迁移不是"复制文件"，而是"在目标仓库建 host 目录 + 补 input + 补 sops key"。**
同时也意味着：**dotfiles 必须先变干净，公司仓库才能安全地继续引用它。**
Stage 1/2 是真正的价值所在，Stage 4 的搬家反而是最轻的一步。

## 归属判定（调查结论）

| 资产 | 判定 | 处置 |
|---|---|---|
| `nixos-configurations/corp.nix` | 100% 公司，且从未被求值（import 路径都是坏的） | 删 |
| `corp-infrastructure.nix` / `samba.nix` / `fleet.nix` | 只被 corp.nix 用 | 删 |
| `openldap.nix` / `sssd.nix` / `hydra.nix` / `ldap.toml` | 硬编码 `AUTOLIFE.TECH`，**无任何主机 import** | 删 |
| `kerberos.nix` | 同上，只被非部署目标 office-windows 用 | 删 |
| `rust-web-server-config.nix` + `shares.toml` 的 `[oauth]` 段 | Auth0 公司租户，无人 import | 删 |
| `office-windows/` + `xiongchenyu.nix:28` + `use-remote-builder.nix:17` | **前雇主 Tron**（trontech.link），既非公司也非个人 | 删 |
| `huoshan-bj-001` | 混合：公司官网 + 个人游戏共用公司域名 | **整机迁公司**（Stage 3） |
| `oracle-arm-001` | 混合：autolife.ai 门面，但实测全是空壳 | **就地洗成个人机**（Stage 3b） |
| `ansible/{parseable,rustfs}` 全部 + `{netbird,vaultwarden,sub2api}` 的 autolife profile | 公司（gz-office / 腾讯云 43.139.62.96） | 迁 |
| oracle-amd-001/002、oracle-arm-002、tcloud、game、office | 个人 | 留，但需解除 domain 污染 |

## 已决策

- **huoshan-bj-001 整机迁往公司仓库**，个人游戏 vhost 一并带走（域名和证书本就是公司资产）。
- **oracle-arm-001 洗成纯个人机**，其上的 autolife 内容合并进 huoshan-bj-001。

## ⚠️ 线上实测：arm-001 的 autolife 门面已经是空壳

动工前做的实测（2026-07-24），结论与配置文件的"看起来还在服务"完全不同：

| 站点 | DNS | 实测 | 真相 |
|---|---|---|---|
| `api.autolife.ai` | 138.2.95.174 (arm-001) | **HTTP 502** | 反代 `localhost:3333`，后端 rust-web-server 模块已注释 |
| `rust-server.autolife.ai` | 138.2.95.174 | **HTTP 502** | 反代 `localhost:3000`，同上 |
| `odoo.autolife.ai` | 138.2.95.174 | HTTP 200 | **不是 Odoo** —— 该 vhost 没有 `locations` 块，返回的是 nginx 默认页；`services.odoo.enable = false` |
| `auth.autolife.ai` | **无 A 记录** | — | 死配置 |
| `fleet.autolife.ai` | **无 A 记录** | — | 死配置，root 指向空目录 `/var/www/fleet` |
| `autolife-robotics.com` | 115.190.173.250 (huoshan) | HTTP 200 | 真正在服务 |

证书方面 arm-001 确实持有有效的 `CN=autolife.ai`（Let's Encrypt，有效期至 2026-09-13）。

**结论：arm-001 上没有任何 autolife 服务在实际运行，只有五个空壳 vhost 和一张证书。**
所以"迁移"的实质内容是：把 **autolife.ai 这个域名的承载能力**（证书签发 + vhost 骨架）
挪到 huoshan，而不是搬运任何运行中的服务。死掉的 odoo/rust-server/fleet/auth 配置
建议直接删除而非搬迁 —— 将来真要恢复，那是一次全新部署，照着三年前的空配置改反而更费事。
需要保留的只有 `api` 和 `rust-server` 两个名字（如果业务上还打算用），
以及 CORS map 那段 `~^https://.*\.autolife\.ai$`。

## 域名归属：北京只做 autolife-robotics.com

**已决策：autolife.ai 不迁往北京，huoshan-bj-001 只承载 `autolife-robotics.com`。**

理由（也是原本的阻塞风险，现已规避）：huoshan 是火山引擎**北京**节点，境内主机对外提供
80/443 需要 ICP 备案，而 `.ai` 是安圭拉国别顶级域，不在工信部允许备案的后缀名单内。
`autolife-robotics.com`（.com，可备案）在该机正常服务，说明这台机器就在备案体系内 ——
把一个备不了案的域名指过去只会招来拦截或合规下线。

**推论：**
- huoshan 侧**不需要**新增 `acme/cloudflare` 凭证，维持现有的 volcengine DNS-01 单证书即可。
- arm-001 上的 autolife 配置**全部直接删除**（反正实测都是空壳），不做任何搬迁。
- `autolife.ai` 就此退出 NixOS 管理。域名本身和 Cloudflare 上的 apex 记录不受影响，
  但指向 arm-001 的三条 A 记录（`api` / `odoo` / `rust-server` → 138.2.95.174）
  需要在 DNS 侧摘除，否则清理后会从"502"变成"连不上"，都是对外暴露的破站。
  这一步在仓库外，需要手工到 Cloudflare 操作。

---

## Stage 0: 解除全局 autolife 污染
**Goal**: dotfiles 里任何个人主机都不再隐式携带 autolife 身份。这是所有后续步骤的前提。
**Status**: ✅ Complete

### 实际做法与偏差

- **Stage 3b 提前合并进来了**：删掉 `acme/default.nix` 里的 `ai` 证书后，arm-001 的四个
  `useACMEHost = "ai"` 立刻失效，不清理就无法验证 Stage 0。arm-001 的 autolife 内容
  （五个 vhost、CORS map、odoo、rustdesk、autolife-relay/rust-web-server 注释块、
  osquery、Fleet 端口、pypdf2 insecure 例外）已全部删除，净减 287 行。
- **tcloud 需要显式 domain**：`nixos-modules/birg-lg.nix`（DN42 bird-lg looking glass）
  经 `bird-border.nix` 被 tcloud 引用，之前的调查误判成死代码。已显式设为
  `domain = "panda.qzz.io"`。
- **coturn realm 按原值钉死**：`tcloud.autolife.ai`。TURN 长期凭证摘要是
  `MD5(user:realm:password)`，realm 一改所有已下发凭证立即失效。**这是唯一残留在个人机上的
  autolife 字符串**，改名需要和 TURN 客户端一起协调 —— 待你决定。
- `acme/default.nix` 的 certs 合并顺序做了调整，让 `panda.qzz.io` 的完整定义
  （`group` / `reloadServices`）在主机自身 domain 恰好也是 panda.qzz.io 时不被覆盖。

### 验证结果（改动后各主机实际签发的证书）

| 主机 | domain | certs |
|---|---|---|
| tcloud | panda.qzz.io | panda.qzz.io |
| oracle-arm-001 | null | panda.qzz.io |
| oracle-arm-002 | null | api / auth / hashtopolis / realtime / panda.qzz.io |
| oracle-amd-002 | null | panda.qzz.io |
| huoshan-bj-001 | autolife.com | autolife-robotics.com, parrot.bj…, rts.bj… |
| game / office | null | （无） |

`autolife.ai` 与 `ai` 两张证书已从全部主机消失。改动前 tcloud / arm-001 / amd-002
各自白签 3 张，现在只剩实际需要的。huoshan 的 `domain = "autolife.com"` 未产生多余证书
（该主机不 import `ezModules.acme`），Stage 3 的顾虑解除。

### 顺带发现（不属于本阶段，未修）

- **`birg-lg` 的 TLS 一直是坏的**：`serverName = "bird-lg.inner.<domain>"` 但证书只覆盖
  `<domain>` 和 `*.<domain>`，而通配符只匹配一级标签，`inner.` 这层匹配不上。
  这是既有问题，改 domain 前后都存在。
- **`oracle-amd-001` 求值失败（既有）**：`Node.js 20 support was removed given upstream
  End-of-Life on 2026-04-30`，由 nixpkgs 升级引入，与本次改动无关。已用 `git stash`
  确认改动前同样失败。**这台机器目前无法构建，需要单独修。**
- `office-mac` 在 Linux 上无法求值（aarch64-darwin 不支持），环境限制，非回归。

1. ~~线上核实 autolife.ai 门面状态~~ ✅ 已完成，见上方实测表：全是空壳。
2. `nixos-modules/core.nix:10`：`networking.domain = lib.mkDefault "autolife.ai"` → 改为 `null`。
   然后给每台仍需要 domain 的机器显式声明（参考 `game/default.nix:187` 已有的做法）。
3. `nixos-modules/acme/default.nix:43-46`：删掉硬编码的 `ai = { domain = "autolife.ai"; ... }` cert。
   `:31-33` 的 `${config.networking.domain}` cert 需要在 domain 为 null 时跳过。
4. `home-modules/cli-minimal.nix:299-310`：`.ldaprc`（`dc=autolife,dc=ai` / `AUTOLIFE.TECH`）
   从最底层 CLI tier 移除 —— 目标仓库的 `home-modules/autolife-extras.nix` 已经有等价实现，无需搬。
5. `shares.toml:26`：`oracle-amd-002.autolife.ai` 这个 wireguard endpoint 换成个人域名，
   否则公司域名一旦不归你管，个人机之间的 wg 直接断。

**Success Criteria**:
- `nix flake check` 通过
- 六台个人主机 `nixos-rebuild build --flake .#<host>` 全部成功
- `grep -ri autolife nixos-modules/core.nix nixos-modules/acme/ home-modules/` 无输出
- 逐台 diff 构建产物，确认没有意外丢失证书（`nix store diff-closures`）

---

## Stage 1: 清除死代码与前雇主遗留
**Goal**: 删掉从未被求值的公司模块和 Tron 残留，缩小后续需要判断的面积。
**Status**: ✅ Complete

### 相对计划的增补

- **`nixos-modules/grafana.nix` 一并删除**：原计划只列了 `ldap.toml`，但它的唯一引用者
  就是 grafana.nix。而 grafana.nix 自己也无人引用，且依赖现在为 null 的
  `networking.domain` —— 留着必然求值失败。两者一起删。
  另外确认：`ldap.toml` 是 git-crypt 加密的，而仓库里**已经没有 `.gitattributes`**，
  git-crypt 早已不再配置，这个文件永远解不开，是纯粹的死数据。
- **`nixos-configurations/game/default.nix` 的 frp 块删除**：`enable = false` 却
  硬编码 `serverAddr = "autolife.ai"`，是个人机上的公司残留。
- **`nixos-modules/ssh-harden.nix` 的注释块删除**：整段被注释的 ldapsearch，
  引用了刚被删除的 openldap 模块和 `dc=autolife,dc=ai`。
- **`shares.toml` 的 `digital` endpoint 删除**：`digital.autolife.ai`，与 amd-002 同样处理。
- **`shares.nix` 的 `inherit (shares) oauth` 删除**：随 `[oauth]` 段一起。

### 验证

- 七台主机求值全部通过（amd-001 仍是 Stage 0 记录的既有 Node.js 20 问题）。
- `git grep -i trontech` → **无输出**，前雇主遗留已彻底清除。
- `grep AutoLifeRobot flake.lock` → **0**，个人 flake 不再需要公司 SSH 凭证才能求值。
- 无任何对已删模块的悬空引用。

### 残留的 autolife 引用（均为预期）

| 位置 | 归属 |
|---|---|
| `flake.nix` 的 `autolife-www` input | Stage 3 随 huoshan 迁走 |
| `home-configurations/freeman.xiong.nix:230,235` sz-office / gz-office 的 ssh 条目 | Stage 3 —— 与 ansible 里同样的办公室 IP 一起处理。**注意：dotfiles 若要公开，这两个公司办公室 IP + 用户名必须先移除。** |
| `nixos-configurations/tcloud/default.nix:204` coturn realm | 刻意保留，见 Stage 0 |
| `nixos-configurations/game/default.nix:184` 注释 | 无害，正好记录了解耦决定 |

删除清单（全部已确认无活跃 import）：
- `nixos-configurations/corp.nix`、`nixos-modules/corp-infrastructure.nix`、`samba.nix`、`fleet.nix`
- `nixos-modules/openldap.nix`、`sssd.nix`、`hydra.nix`、`kerberos.nix`、`ldap.toml`（git-crypt 加密）
- `nixos-modules/rust-web-server-config.nix` + `shares.toml:41-51` 的 `[oauth]` 段
- `nixos-configurations/office-windows/`、`nixos-modules/xiongchenyu.nix:28`、
  `home-modules/use-remote-builder.nix:17` 的 `*.trontech.link` 条目
- `flake.nix:117-123` 的 `autolife-relay` input（私有仓库，**目前是个人 flake 唯一需要公司 SSH 凭证才能 eval 的地方**）
  + `oracle-arm-001` 里对应的注释块
- 顺带修既有 bug：`flake.nix:261,263` 的 `netbird` / `lubancat` 在 `nixos-configurations/` 下
  无对应目录（悬空条目）；`secrets/netbird-starslab.yaml` 是孤儿文件

**Success Criteria**: `nix flake check` + 全主机 build 通过；`git grep -i autolife` 的命中
只剩 huoshan-bj-001、oracle-arm-001、ansible/、secrets/ 四处。

---

## Stage 2: 目标仓库补基础设施
**Goal**: `AutoLifeRobot/nixos` 具备接收新主机的能力。**必须在 Stage 3 之前完成。**
**Status**: ✅ Complete（3 个 commit：`62a6cf6` / `9ef4dff` / `bbed28e`，均未 push）

### 相对计划的偏差

- **input 补齐的实际内容与预想不同**。计划以为要补的是 huoshan 的依赖
  （protect-carrot / bevy-open-rts / autolife-www），但那些属于 Stage 3、放到迁移时补更干净。
  Stage 2 真正必须补的是 **dotfiles 自身模块引用的 input** —— 目标仓库把 dotfiles 模块
  的 `inputs` 解析到**自己**的 input 集，所以 dotfiles 模块按名字引用的东西必须在这里存在：
  - `talon-nix` —— dotfiles 的 `nixos-modules/gui.nix` 无条件 import 它，缺了直接求值失败。
  - `noctalia` —— dotfiles 模块读 `inputs.noctalia`，而目标仓库叫 `noctalia-v5`
    （上游 `noctalia-shell`，是同一项目的新名字）。用 compatInputs 做别名，
    避免闭包里出现两份 noctalia。
  这是薄封装架构的固有代价：**每次 dotfiles 新增模块级 input，目标仓库都要跟着镜像。**
- **`disko` 不需要**：huoshan 用 `hardware-configuration.nix`，不用 disko。
- **`autolife-www` 已可改远程**：`AutoLifeRobot/www` 仓库存在且本地 HEAD == origin/main
  （`9419e04`），改远程 URL 是安全的。留到 Stage 3 随 huoshan 一起改。
- **sops 规则从"合并密钥"变成"收紧授权"**：原计划只说补 key，但执行时发现
  `updatekeys` 会按 creation_rules 把 **`game`（个人工作站）** 加成 `sg-office.yaml` 的
  收件人 —— 该文件原本只有 sg-office 一把密钥。这是公私分离的反向泄漏。
  因此把一条 `secrets/*.yaml` 通配规则拆成 per-file 规则，并把 `game` 整个移出
  （它在本仓库没有任何配置）。最终授权：
  `common.yaml` → sg-office + huoshan；`sg-office.yaml` → sg-office。
  **已验证重新加密前后两个文件明文 sha256 完全一致。**
- **格式化是必需的额外工作**：仓库从未跑过 hooks，一开就是红的。做了机械格式化
  （单独 commit `bbed28e`），并新增 `statix.toml` 关掉 `repeated_keys`(W20) ——
  它要求把按主题分组的 `services.*` 合并成一个大 attrset，与两个仓库的既有写法冲突。

### 验证

- `sg-office` 求值通过，且**跨越 dotfiles 23 个 commit + 更换 nixpkgs channel 后，
  格式化前后 drvPath 完全一致**（`yrqkdwpcll055bqz3mry4145crvd9hrk`），
  证明所有改动语义中性。
- `nix build .#checks.x86_64-linux.pre-commit` **全部 hook 通过**。
  （注：dotfiles 自己的同名门禁目前是失败的 —— shfmt 在一个游戏启动脚本上报错，
  属既有问题，与本次无关。）
- `flake.lock` 中 `dotfiles` pin 到 `1779017f`（Stage 1 的 commit），
  nixpkgs 为 `nixos-unstable@e2587cae`，与 dotfiles 同 rev；无 AutoLifeRobot 私有仓库引用。

### 遗留

- 三个 commit **都还没 push** 到 `AutoLifeRobot/nixos`。
- `services.logind.lidSwitch*` 系列弃用警告（来自 `server-power-management.nix`），
  nixos-unstable 引入，不影响求值，可后续单独修。
- 目标仓库的 `shares.toml` 现在是 dotfiles 的完整副本，携带了个人主机的 wireguard
  公钥。sg-office 确实需要 `oracle-amd-002` 那条（wg peer），但其余（game/office/digital）
  是多余的。**未来可收敛成公司仓库最小集** —— 本次未做，避免与迁移混在一起。

1. **修 dotfiles pin（阻塞级）**：目标仓库 pin 的是 `?ref=develop`，其 rev 落后本地 `main` 21 个
   commit，而 huoshan 的全部近期工作都在 main 上。改成 `?ref=main`，或先把 main 合回 develop。
2. **合并 `.sops.yaml` key 段**：目标仓库只有 3 个 key。**注意 `office` 这个别名在两个仓库指向
   不同的 age 公钥**（`age17qz63…` vs `age1ss6yryy…`）—— 同名不同 key，混用必然解密失败，
   合并时必须重命名区分。补齐要迁主机的 host age key，然后 `sops updatekeys`。
3. **补 flake input**（按迁哪台补）：迁 huoshan 需要 `protect-carrot`、`bevy-open-rts`、
   `autolife-www`；任何 oracle/tcloud 需要 `disko`。同时补 `dotfiles.inputs.*.follows`，
   否则会出现双份 nixpkgs。
4. **`autolife-www` 改远程**：现在是 `git+file:///home/freeman.xiong/Documents/github/autolife/www`
   本地绝对路径，换机器/CI 直接 eval 不了 → 改成 `git+ssh://git@github.com/AutoLifeRobot/www.git`。
5. **统一 nixpkgs channel**：目标仓库用 `nixpkgs-unstable`，dotfiles 用 `nixos-unstable`
   （后者有 NixOS 测试门禁）。统一到 `nixos-unstable`。
6. **开 pre-commit hooks**：目标仓库 input 里有 `pre-commit-hooks` 但没 import flakeModule，
   五个 hook（nixfmt/statix/deadnix/shellcheck/shfmt）一个都没跑。先开上，否则搬进去的代码格式立刻发散。
7. **同步 `shares.toml`**：目标仓库是旧快照 —— 有已删除的 tcloud 条目和 `[[tronlink]]` 段，
   `digital.auotlife.ai` 是 typo，freeman 的 yubikey cardno 少一位（`32_087_47` vs `32_087_478`，疑似 bug）。

**Success Criteria**: 目标仓库 `nix flake check` 通过；`sg-office` build 不回归；
pre-commit 在全仓库跑通。

---

## Stage 3: 迁移 huoshan-bj-001 与 ansible
**Goal**: huoshan-bj-001 落到目标仓库，dotfiles 侧移除。
**Status**: Not Started

1. huoshan-bj-001：在目标仓库建 `nixos-configurations/huoshan-bj-001/`，
   加进 `ezConfigs.nixos.hosts`。本地引用的 dotfiles 模块（`ezModules.cn`、`ezModules.sing-box`、
   `ezModules.root`、`ezModules."freeman.xiong"`、`ezModules.core`、`ezModules.server`、
   `ezModules.mixins-nginx`）改成 `inputs.dotfiles.nixosModules.*`。
   连同 `hardware-configuration.nix` 和 `rts-index.html` 一起搬。
2. ACME 维持原样 —— 只有 `autolife-robotics.com` 一张证书（volcengine DNS-01），
   `sops.secrets."acme/volcengine"` 需要同步到目标仓库的 `secrets/common.yaml`。
   **不要**引入 autolife.ai / cloudflare 那套。
   注意 huoshan 目前 `domain = "autolife.com"`（`default.nix:82`），这个域名在配置里
   没有任何 vhost 使用，但 Stage 0 把 `core.nix` 的 domain 默认值改掉之后要复核它
   是否会让 acme 模块去签一张多余的 `autolife.com` 证书。
3. ansible：整体搬 `parseable/`、`rustfs/`，以及 `netbird/`、`vaultwarden/`、`sub2api/` 的
   autolife profile（`inventory-autolife.ini` + `vars/autolife.yml`）。
   dotfiles 里保留 starslab / lubancat 的个人 profile。
   注意 `sub2api/deploy-sub2api.yml:10-14` 的**默认 profile 就是 `autolife`**，
   留在 dotfiles 的那份必须改默认值。
4. 对应 secrets 搬迁：`netbird-autolife.yaml`、`vaultwarden-autolife.yaml`、
   `sub2api-autolife.yaml`、`parseable.yaml`、`rustfs.yaml`、`versitygw.yaml`。

**Success Criteria**: 目标仓库能 `nixos-rebuild build --flake .#huoshan-bj-001`；
实机 switch 后官网 + 两个游戏站点均可访问、证书正常；dotfiles 侧对应目录已删除。

---

## Stage 3b: oracle-arm-001 去 autolife 化
**Goal**: arm-001 变成纯个人机，autolife.ai 退出 NixOS 管理。
**Status**: ✅ 配置侧已完成（并入 Stage 0）；DNS 清理与实机 switch 待做

autolife.ai 不迁北京，且实测这些 vhost 全是空壳（502 / nginx 默认页），
所以**全部删除，不搬迁任何东西**：

1. 删除 `nixos-configurations/oracle-arm-001/default.nix` 中的：
   - `:86` `domain = "autolife.ai"` → 改成个人域名或 null
   - `:283-353` 五个 `*.autolife.ai` vhost 全部
   - `:276-282` 的 CORS map（只服务 autolife.ai）
   - `:181-187` rustdesk-server（`relayHosts = ["rustdesk.autolife.ai"]`，已 disabled）
   - `:191-225` autolife-relay 注释块 + `:412-431` 对应的 sops 注释块
   - `:227-263` odoo 整段 + `:383-393` odoo logrotate + `:435-442` odoo sops secrets
   - `:129-132` postgresqlBackup 的 `odoo` 库、`:170-178` odoo 的 pg user/db
   - `:97` 防火墙的 8080 (Fleet) 端口
   - `:266-273` rust-web-server 注释块 + `:38-40` 空 overlay 块
2. 保留：postgresql 的 `freeman.xiong` 库（个人）、`:60-62` 的 ssh key 需要复核
   （`server-benjamintan`、`summer@summers-MacBook-Pro` 疑似公司/他人的 key，
   arm-001 洗成个人机后应当移除）。
3. DNS 侧（仓库外，手工做）：到 Cloudflare 摘掉 `api` / `odoo` / `rust-server`
   三条指向 138.2.95.174 的 A 记录。arm-001 清理后这些名字会从 502 变成连不上，
   留着只是对外挂着三个破站。
4. `secrets/common.yaml` 里 `odoo:` 和 `autolife-relay:` 两个键随之失去最后的消费者，
   在 Stage 4 一并删除。

**Success Criteria**: `git grep -i autolife nixos-configurations/oracle-arm-001/` 无输出；
build 通过；实机 switch 后个人服务（postgresql 的 freeman.xiong 库等）不受影响；
`curl https://api.autolife.ai` 不再解析。

---

## Stage 4: 拆分 secrets/common.yaml
**Goal**: 个人仓库的 secrets 里不再有公司凭证。**这是最脏的一步，放最后单独做。**
**Status**: Not Started

`secrets/common.yaml` 是 `shared-modules/sops.nix:1` 和 `home-modules/default.nix:47` 的
`defaultSopsFile`，被几乎所有主机 + home 配置共用，33 个顶层键里公司/个人混在一起。

- **公司键**：`autolife-relay`、`odoo`、`rust-web-server`、`openldap`、`fleet/mysql-password`、
  `openfortivpn`（公司 VPN）、`falcon`（公司 EDR）、`sub2api`、`casdoor`、`casibase`
- **个人键**：其余 23 个（`cc-gateway`、`api-keys`、`ib-gateway`、`hashtopolis`、`wireguard`、
  `restic`、`acme`、`cloudflared`… ）

做法：Stage 1 删完死模块后，公司键里大部分已经没有消费者，可直接删而非搬。
剩下真正在用的（`openfortivpn`、`falcon`，都只被 office/game 用）单独决定 —— 这两个是
"公司装在个人机上的合规软件"，凭证放哪都别扭，倾向于留在个人仓库但独立成
`secrets/corp-endpoint.yaml`。

最后从 `.sops.yaml` 移除公司 PGP key（`&autolife F2BAD15A…`、`&seanhxx`、`&freeman-sgoffice`）
并 `sops updatekeys` 重新加密全部个人 secrets。

**Success Criteria**: `sops -d secrets/common.yaml` 里无公司凭证；全主机 build + 抽样实机
switch 后 sops 挂载正常；公司 PGP key 已从个人仓库 recipient 列表移除。

---

## 风险与注意

- **不要跳过 Stage 0 直接搬家。** domain 污染不解除，个人机的 ACME 会在公司域名失效的
  那天集体崩掉，而且是静默地在证书续期时才爆。
- 每个 Stage 独立 commit，`nixos-rebuild build`（不是 switch）作为门禁；实机 switch 逐台做，
  远程机器先 `--target-host` build 再 switch，保留回滚代。
- `nixos-modules/ldap.toml` 是 git-crypt 加密的，删除前确认 git-crypt key 的处置。
- 目标仓库当前工作区不干净（`M flake.lock`、`M nixos-modules/nvidia-dual-4090.nix`），
  动工前先清理。
- 腾讯云 `43.139.62.96` 同时跑 netbird + vaultwarden + sub2api 三个公司服务，
  但**不在 NixOS 管理下**，只有 ansible。它是一台完整的公司主机，值得单列记录。
