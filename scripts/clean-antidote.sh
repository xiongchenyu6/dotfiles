#!/usr/bin/env bash
set -euo pipefail

# Cleans Antidote temp files and regenerates plugins.
echo "🚀 清理 Antidote 临时文件..."
rm -f /tmp/tmp_hm_zsh_plugins.zsh-*

echo "📂 确保 ~/.cache/antidote 存在并设置权限..."
mkdir -p ~/.cache/antidote
chmod -R u+rwX ~/.cache/antidote

echo "🔄 重新生成 Antidote 插件..."
if command -v home-manager >/dev/null 2>&1; then
	home-manager switch
else
	echo "⚠️ 未检测到 home-manager，请先安装并配置 home-manager"
	exit 1
fi

echo "✅ 完成！建议现在重新启动 Zsh 会话："
echo "   exec zsh"
