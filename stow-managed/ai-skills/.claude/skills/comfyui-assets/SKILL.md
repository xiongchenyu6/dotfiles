---
name: comfyui-assets
description: 用 ComfyUI / Hunyuan3D-2 批量生成游戏素材(icon、精灵图、贴图、3D 模型)并用截图 harness 验收。用于游戏缺素材、"生成图标/模型/素材"、UI 文字要换成 icon 时。
---

# ComfyUI 素材生成管线

服务跑在 sg-office(双 GPU);endpoint 和 bearer token 不写在这里——token 在
dotfiles 仓库 secrets 的 comfyui 条目里,本地 `sops` 解开取用。

## 原则

- **批量,不要一张张抠**:先列完整素材清单(整套 icon、全部兵种、全套技能特效),
  一次排队生成,再统一筛选。素材数量直接决定游戏耐玩度,宁多勿少
- **UI 一律 icon 不用文字**:文字标签都是待替换项,缺哪个 icon 就生成哪个
- **精灵图必须带动画帧**(移动/攻击/受击),静态单帧不合格

## 3D 模型(Hunyuan3D-2)

1. 生成后必须用 harness 截图检查质量(比例、贴图、破面)
2. 质量不够就调 prompt 重新生成,不要将就着用
3. 合格后再进游戏资产目录,记录生成参数便于补做同风格素材

## 验收闭环

生成 → 导入游戏 → harness 截图(含不同交互状态)→ 不合格重生成。
没截图验收过的素材不算完成。
