---
name: proxy-nodes
description: 个人跨境代理节点的惯例:统一 hysteria2、服务端用 sing-box NixOS 模块、每机自持证书、客户端(sing-box/mihomo/karing)配置与订阅链接、多节点负载均衡。用于加/改节点、换协议、生成扫码配置、订阅链接、代理不通或带宽打满排查时。
---

# 跨境代理节点惯例

## 协议只留 hysteria2

- **裸 VLESS / shadowsocks 全部下线**,新增节点一律 hysteria2(UDP/QUIC + Brutal)。
  原因写在 `nixos-modules/sing-box.nix` 的 option 描述里:跨境段专掐 TCP 隧道,
  同链路 ss 只有 16 KB/s,hy2 能跑 3 MB/s
- 服务端配置只在 `nixos-modules/sing-box.nix` + 各主机 `default.nix` 里改,
  用 `nixos-rebuild` 下发。**NixOS 机器不要用 ansible playbook 改代理**
- 密码走 sops(`sing-box/HYSTERIA2_PASSWORD`)

## 证书

- **一机一个证书管家**,不追求集中化(可用性要求不高,简单优先)。
  acme 定时器要确认还活着——过期没续的域名是历史常见坑
- 证书目录属组决定谁读得到:sing-box 要能读 nginx 组的 acme 目录
- 反代要么全 caddy 要么全 nginx,按主机上已有的来,不混搭

## 防火墙

- 云厂商安全组放行 UDP 通常没问题,真正挡住的是主机自己的防火墙
- 这些机器是 **nftables 后端**,用 iptables 临时开洞会静默失败,别在这上面浪费时间

## 客户端

- Linux 桌面:sing-box(systemd 单元 + polkit 允许免密 toggle)或 mihomo;
  节点多了要负载均衡就换 clash 系客户端
- 手机 / iOS:karing。**karing 要的是配置文件格式,不是把机场全展开的裸 json**
  (展开的没法自动更新)
- 分享给自己或朋友:做订阅链接挂在自有域名下,域名走 terraform 仓库添加,
  提交即自动 apply
- 每次改完协议要**给出可扫的二维码/配置**,并实测点一遍系统代理生效

## 排查

- 先分段测:本机 → 节点(直连带宽)、节点 → 目标。两端都快而中间慢就是链路 QoS
- 带宽打满/单点撑不住:多节点 + 负载均衡是彻底方案;套餐限速无法升级时,
  用现有几台按权重分流(小流量导向备用线路)
- 上行带宽要实测,不要按套餐标称值下结论
