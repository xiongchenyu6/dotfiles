# Sing-box Server Documentation (hysteria2-only)

## Protocol

自 2026-08 起服务端只跑 **hysteria2**(shadowsocks 已下线):

- **Servers:** oracle-amd-001 (213.35.97.233)、oracle-amd-002
  (213.35.117.232)、jtti-sg (45.194.18.75)、lubancat (203.116.95.146)、
  sg-office (101.78.126.6)
- **Port:** 8443/udp (QUIC)
- **TLS:** Let's Encrypt `*.panda.qzz.io` 通配证书;客户端正常校验证书

## 🔐 Accessing Credentials

Credentials are stored with SOPS:

```bash
sops -d secrets/servers.yaml
```

`hysteria2.password` / `hysteria2.port` 就是客户端所需的全部参数。

## Clients

- **game 主机**:声明式 sing-box TUN 客户端(`nixos-configurations/game/default.nix`,
  services.sing-box),通过 `pon`/`poff` 按需启停,国内流量走
  geoip-cn/geosite-cn 规则直连。5 个节点由 URLTest 每 15 秒检查 Anthropic
  链路并自动故障转移;lubancat 优先,jtti-sg 因带宽和稳定性仅作末位应急。
  密码经 sops-nix 注入,不落明文
- 其他客户端:任何支持 hysteria2 的内核(sing-box / clash.meta),
  server_name 填 `hy2-<host>.panda.qzz.io`,保持证书校验开启

## Service Management

```bash
ssh jtti-sg "systemctl status sing-box"
ssh jtti-sg "systemctl restart sing-box"
ssh jtti-sg "journalctl -u sing-box -f"
```

## Firewall

- 8443/udp — hysteria2 (ufw,由 playbook 管理)
- 22/tcp — SSH

## Installation Details

- **Installation Path:** `/usr/local/bin/sing-box`
- **Config Location:** `/etc/sing-box/config.json`
- **System User:** sing-box (unprivileged)
- **Deploy:** `ansible-playbook -i inventory.ini deploy-singbox-secure.yml`
  (jtti-sg 单独部署加 `-l jtti-sg`)

## Troubleshooting

1. 服务状态:`ssh jtti-sg "systemctl status sing-box"`
2. 防火墙(hysteria2 是 UDP,漏 ufw 规则时服务端看着一切正常但客户端不通):
   `ssh jtti-sg "ufw status"`
3. 日志:`ssh jtti-sg "journalctl -u sing-box -n 50"`
4. 本机客户端:`pon`,测试 `curl https://api.anthropic.com`,完成后 `poff`
