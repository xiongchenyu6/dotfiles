# Sing-box Server Documentation (hysteria2-only)

## Protocol

自 2026-08 起服务端只跑 **hysteria2**(shadowsocks 已下线):

- **Servers:** jtti-sg (45.194.18.75)、lubancat (203.116.95.146)
- **Port:** 8443/udp (QUIC)
- **TLS:** ansible 生成的自签证书,CN `hy2-<host>.panda.qzz.io`;客户端跳过校验(hysteria2 有密码认证)

## 🔐 Accessing Credentials

Credentials are stored with SOPS:

```bash
sops -d secrets/servers.yaml
```

`hysteria2.password` / `hysteria2.port` 就是客户端所需的全部参数。

## Clients

- **game 主机**:声明式 sing-box 客户端(`nixos-configurations/game/default.nix`,
  services.sing-box),本地 mixed 代理端口 127.0.0.1:20170 / 20171,
  国内流量走 geoip-cn/geosite-cn 规则直连。密码经 sops-nix 注入,不落明文
- 其他客户端:任何支持 hysteria2 的内核(sing-box / clash.meta),
  server_name 填 `hy2-<host>.panda.qzz.io`,allow-insecure 开启

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
4. 本机客户端:`systemctl status sing-box`,测试
   `curl -x socks5h://127.0.0.1:20170 https://www.google.com`
