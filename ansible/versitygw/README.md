# VersityGW S3 网关部署

本项目使用 Ansible 部署 VersityGW S3 网关，支持多用户访问管理。

## 用户账户设计

| 账户 | 用途 | 权限 |
|------|------|------|
| **root (myaccess)** | AI 工程师 | 完全控制，可读取/下载/删除所有桶的数据 |
| **fae-uploader** | FAE 上传数据 | 拥有 `training-backup` 桶，可上传数据 |

## 文件结构

```
ansible/versitygw/
├── deploy-versitygw.yml    # 主部署 playbook
├── setup-iam-users.yml     # 创建 IAM 用户
├── debug-versitygw.yml     # 调试工具
├── inventory.ini           # 服务器清单
├── ansible.cfg             # Ansible 配置
└── README.md

secrets/
└── versitygw.yaml          # SOPS 加密的凭证文件
```

## 部署步骤

### 1. 部署 VersityGW

```bash
cd ansible/versitygw
ansible-playbook -i inventory.ini deploy-versitygw.yml -K -k
```

### 2. 创建 IAM 用户

```bash
ansible-playbook -i inventory.ini setup-iam-users.yml -K -k
```

这将：
- 创建 `training-backup` 桶
- 创建 FAE 用户
- 将 `training-backup` 桶分配给 FAE 用户

## 凭证管理

所有凭证存储在 SOPS 加密文件 `secrets/versitygw.yaml` 中。

### 查看凭证
```bash
cd /path/to/dotfiles
sops -d secrets/versitygw.yaml
```

### 修改凭证
```bash
sops secrets/versitygw.yaml
```

## 使用示例

### FAE 上传数据

```bash
# 设置环境变量
export AWS_ACCESS_KEY_ID=fae-uploader
export AWS_SECRET_ACCESS_KEY=fae-secret-uploader-2026
export AWS_ENDPOINT_URL=http://<versitygw-server>:7070

# 上传文件
aws s3 cp training_data.tar.gz s3://training-backup/

# 上传目录
aws s3 sync ./training_data/ s3://training-backup/training_data/

# 列出文件
aws s3 ls s3://training-backup/
```

### AI 工程师下载数据（使用 root）

```bash
# 设置环境变量
export AWS_ACCESS_KEY_ID=myaccess
export AWS_SECRET_ACCESS_KEY=mysecret
export AWS_ENDPOINT_URL=http://<versitygw-server>:7070

# 列出所有桶
aws s3 ls

# 下载文件
aws s3 cp s3://training-backup/training_data.tar.gz ./

# 同步目录
aws s3 sync s3://training-backup/training_data/ ./training_data/

# 删除文件（仅 root 可以）
aws s3 rm s3://training-backup/old_file.txt
```

### 使用 s3cmd

创建配置文件：

```bash
# FAE 配置 (/tmp/s3cfg-fae)
cat > /tmp/s3cfg-fae << 'EOF'
[default]
access_key = fae-uploader
secret_key = fae-secret-uploader-2026
host_base = <server>:7070
host_bucket = <server>:7070/%(bucket)
use_https = False
signature_v2 = False
EOF

# Root 配置 (/tmp/s3cfg-root)
cat > /tmp/s3cfg-root << 'EOF'
[default]
access_key = myaccess
secret_key = mysecret
host_base = <server>:7070
host_bucket = <server>:7070/%(bucket)
use_https = False
signature_v2 = False
EOF

# 使用
s3cmd -c /tmp/s3cfg-fae --region us-east-1 put file.txt s3://training-backup/
s3cmd -c /tmp/s3cfg-root --region us-east-1 ls s3://training-backup/
```

## 调试

如果遇到问题，运行调试 playbook：

```bash
ansible-playbook -i inventory.ini debug-versitygw.yml -K -k
```

## 安全注意事项

1. **密钥安全**：生产环境应使用更强的随机密钥
2. **网络隔离**：建议在内网部署，不直接暴露到公网
3. **数据保护**：建议配合 restic 进行增量备份，防止数据丢失

## 参考链接

- [VersityGW Wiki - Multi-Tenant](https://github.com/versity/versitygw/wiki/Multi-Tenant)
- [VersityGW Wiki - Quickstart](https://github.com/versity/versitygw/wiki/Quickstart)
