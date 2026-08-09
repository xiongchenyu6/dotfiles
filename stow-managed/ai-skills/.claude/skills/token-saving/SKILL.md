---
name: token-saving
description: 用 rtk 前缀跑命令、用 ck 语义搜代码,压缩喂给模型的输出省 token。用于跑 git/测试/构建/docker 等输出冗长的命令,或在大代码库里按含义找代码时。
---

# 省 token:rtk 命令代理 + ck 语义搜索

## rtk — 命令输出精简(可省 60–90% bash 输出)

输出冗长的命令一律加 `rtk` 前缀,由它转发并精简输出:

```bash
rtk git status / rtk git diff / rtk git log
rtk cargo test / rtk cargo build / rtk pytest / rtk jest
rtk tsc / rtk lint
rtk docker ps / rtk kubectl logs …
rtk ls / rtk read file.rs / rtk grep "pattern" .
```

- 覆盖 100+ 常见开发命令;不认识的命令 rtk 会原样透传,加前缀无副作用
- 输出被精简过;需要完整原始输出排查问题时才去掉前缀重跑

## ck — 语义/混合代码搜索(替代盲目 grep 全库)

按含义找代码,命中率高、返回少:

```bash
ck --sem "error handling" src/        # 语义搜索
ck --hybrid "async timeout" src/      # 关键词+语义混合
ck -n "TODO" *.rs                     # 兼容 grep 用法
ck --sem --topk 5 --full-section "auth logic" src/   # 限量+整函数返回
```

- 首次使用先 `ck --index .`(之后增量);`ck --status .` 查索引状态
- 结构化输出:`ck --jsonl --sem "…" src/`
- 明确知道字符串/标识符时仍用普通 grep/rg;不知道叫什么、只知道干什么时用 ck
