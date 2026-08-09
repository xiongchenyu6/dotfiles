---
name: tui-automation
description: 用 terminal-use (tu) 驱动交互式 TUI/终端程序。用于需要操作 htop、交互式安装器、菜单式 CLI 等无法一次性执行的交互式终端应用时。
---

# terminal-use (tu) 交互式终端自动化

headless 虚拟终端(后台 daemon + PTY),让 agent 能操作交互式 TUI 程序。

## 标准工作流

```bash
tu run <app>                       # 启动交互式应用
tu screenshot                      # 读屏(文本;也可输出 PNG)
tu type 'hello world'              # 输入文本
tu press F2                        # 按键
tu mouse click --on-text 'OK'      # 按屏幕文字点击
tu wait --text 'Complete' --timeout 30   # 等待某文字出现
```

## 注意

- daemon 自动启动,每次 CLI 调用都是无状态的,支持多会话并行
- 循环:发输入 → `tu screenshot` 确认状态 → 再决定下一步,别盲发按键
- `tu monitor` 是给人实时观察用的,agent 不要调
