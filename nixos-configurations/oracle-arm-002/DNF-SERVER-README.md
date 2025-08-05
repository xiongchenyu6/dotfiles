# DNF Server on Oracle ARM-002

Simple deployment using NixOS's built-in `virtualisation.oci-containers`.

## Configuration

The DNF server is configured in `dnf-server.nix` and automatically imported in `default.nix`.

## Deployment

1. Deploy the configuration:
```bash
nixos-rebuild switch --flake .#oracle-arm-002 --target-host root@oracle-arm-002 --build-host root@oracle-arm-002
```

2. Check services status:
```bash
ssh root@oracle-arm-002
systemctl status docker-dnfmysql
systemctl status docker-dnfserver
```

## Management Commands

After SSH into oracle-arm-002:

```bash
# Check status
dnf-status

# View logs
dnf-logs-mysql
dnf-logs-server

# Access MySQL
dnf-mysql

# Access server shell
dnf-shell

# Restart services
dnf-restart
```

Or use systemctl directly:
```bash
systemctl restart docker-dnfmysql
systemctl restart docker-dnfserver
```

## Server Information

- **Public IP**: 203.116.47.202
- **MySQL Port**: 3000
- **Game Ports**: 7600, 881 (and many others)
- **MySQL Password**: 88888888
- **GM Account**: gm_user / 123456

## Data Location

- MySQL data: `/var/lib/dnf-server/mysql/`
- Server data: `/var/lib/dnf-server/data/`
- Server logs: `/var/lib/dnf-server/log/`

## Note on ARM

The containers are x86_64 images running on ARM through QEMU emulation, which may impact performance.