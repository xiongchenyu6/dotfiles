---
name: asset-gen
description: 批量生成视觉/音频素材:精灵图与动画帧、UI icon、立绘与过场、贴图、3D 模型、音效、头像与视频。后端有 ComfyUI(Flux/Kontext/Wan)、Hunyuan3D-2、PixelLab、Retro Diffusion、ElevenLabs、codex CLI 生图。用于游戏缺素材、"生成图标/精灵图/模型/音效/立绘"、UI 文字要换 icon 时。
---

# 素材生成管线

## 后端选型

| 需求 | 后端 | 说明 |
|---|---|---|
| 通用图/立绘/背景 | ComfyUI Flux(`comfy_gen.py`) | 跑在 sg-office 双 4090;endpoint/token 见 dotfiles secrets 的 comfyui 条目 |
| 同一角色换姿势/动作帧 | ComfyUI Flux **Kontext** | 走路帧、攻击帧靠它保持角色一致 |
| 像素风精灵 | PixelLab(`PIXELLAB_SECRET`) | 免费额度用完自动停 |
| 像素风背景/战场 | Retro Diffusion(`RD_API_KEY`) | 同上,余额耗尽干净退出 |
| 音效 | ElevenLabs sound-generation(`ELEVENLABS_API_KEY`) | 输出 mp3,ffmpeg 转 wav |
| 3D 模型 | Hunyuan3D-2 | 同 ComfyUI 主机 |
| 视频/图生视频 | ComfyUI Wan 2.2 | 过场动画、剧情演出 |
| 临时补一张图 | codex CLI 生图 | 手上有两个 200 刀 pro 账号,额度用不完,别省 |

**key 一律从环境变量/sops 读,绝不写进脚本或文档。**付费额度的原则是
「先把免费/已有额度用光」,脚本必须在余额耗尽时干净退出而不是报错崩掉。

## 原则

- **批量,不要一张张抠**:先列完整素材清单(整套 icon、全部兵种、全套技能特效、
  各职业×种族立绘组合),一次排队生成,再统一筛选。素材数量直接决定耐玩度,宁多勿少
- **UI 一律 icon 不用文字**:文字标签都是待替换项,缺哪个 icon 就生成哪个
- **动画帧是硬指标**:一个动作 ≥8–10 帧(idle / walk×6 / attack / hit)。
  "静态图平移""只有残影""走路手脚不动""旋转着位移"一律不合格。近战职业要
  劈砍动作,不能生成成远程投射姿势
- **一致性**:同角色的走路/攻击/背包图必须是同一形象——用 Kontext 从同一张
  基准图派生,不要各自独立生成
- **风格统一**:塔和敌人不能一个像素风一个写实。换风格就整套重生成

## 生成后处理(不做就是半成品)

1. 去背 + trim + 对齐(脚底贴底、统一 128px 方图),Kontext 偶尔出近黑帧要自动重试
2. **contact sheet 拼图人工/截图速查**——批量结果一张张看不过来,拼成带标签的
   大图找坏帧、缺件、撞脸
3. **verify 脚本**:从代码里推导出「需要哪些素材」,校验文件存在且非空白,
   输出 manifest + report。发布前必跑
4. **转 webp**(q90 保 alpha)并删掉 PNG,首屏体积能降 60–70%;加载路径同步改扩展名

## 验收闭环

生成 → 后处理 → 导入游戏 → harness 截图(含不同交互状态)→ 不合格重生成。
**没截图验收过的素材不算完成。**3D 模型额外查比例、贴图、破面。

合格后记录生成参数(prompt / seed / 尺寸 / 后端),便于以后补做同风格素材。
