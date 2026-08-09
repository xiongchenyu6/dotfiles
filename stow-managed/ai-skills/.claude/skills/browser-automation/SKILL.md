---
name: browser-automation
description: 用 agent-browser CLI 做 headless 浏览器自动化。用于打开网页、点击、填表、截图、读取页面内容、调试 web 应用时(无 GUI 或不想动真实 Chrome 的场景)。
---

# agent-browser 浏览器自动化

纯 CLI 驱动 headless 浏览器,适合调试 web 应用和网页自动化。

## 标准工作流

1. 打开页面 → 2. 快照拿元素引用 → 3. 用引用交互 → 4. 页面变化后重新快照

```bash
agent-browser open <url>          # 打开页面
agent-browser snapshot -i         # 列出可交互元素(@e1、@e2 …)
agent-browser click @e1           # 点击
agent-browser fill @e2 "text"     # 填输入框
agent-browser screenshot          # 截图
agent-browser read [url]          # 读取页面文本(agent 友好格式)
agent-browser get text @e1        # 取元素文本
agent-browser get url             # 当前 URL
agent-browser wait <selector>     # 等元素出现
agent-browser is visible @e1      # 可见性检查
```

## 注意

- 机器可读输出加 `--json`(如 `snapshot --json`)
- 元素引用(@eN)在页面变化后会失效,操作后重新 `snapshot`
- 不确定的子命令用 `agent-browser --help` 查
