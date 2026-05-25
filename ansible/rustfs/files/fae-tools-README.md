# fae-tools

机器人侧训练数据上传工具的发布桶。所有机器人都可以从这里拉到最新的 `trainingdata-cli`。

## 下载安装（在机器人上跑一次）

```bash
sudo curl -fsSL https://s3.gz.autolife.ai:8444/fae-tools/trainingdata-cli \
  -o /usr/local/bin/trainingdata-cli
sudo chmod +x /usr/local/bin/trainingdata-cli

# 安装 systemd timer + 写入 /etc/training-upload/upload.env
sudo trainingdata-cli agent install
```

机器人主机名要符合 `autolife-<bucket>` 格式（例如 `autolife-robot-234`），
`agent install` 会按主机名自动选 bucket（`robot-234`）。

## 升级到最新版本

```bash
sudo trainingdata-cli agent upgrade
```

`agent upgrade` 默认从本桶拉脚本（`UPGRADE_URL` 已经指向
`https://s3.gz.autolife.ai:8444/fae-tools/trainingdata-cli`），同步完成后
会自动重启 timer/service。

## 常用命令

```bash
trainingdata-cli agent status       # 看 timer 和最近上传日志
sudo trainingdata-cli agent test    # 手动跑一次（绕过 20:00–06:00 时间窗）
sudo trainingdata-cli agent stop    # 临时停掉 timer
sudo trainingdata-cli agent start   # 重新启用
```

## 桶里有什么

- `trainingdata-cli` — 主脚本本身（anonymous read 已开，能直接 curl）
- `README.md` — 本文件

## 维护：怎么发布新版本

桶里的 `trainingdata-cli` 由 dotfiles 里的 ansible 脚本同步：

```bash
cd ~/dotfiles/ansible/rustfs
./update-fae-tools.sh
```

会把 `~/Documents/github/autolife/TrainingDataUploader/trainingdata-cli`
SCP 到 rustfs 主机然后 `mc cp` 进桶，sha256 校验。
