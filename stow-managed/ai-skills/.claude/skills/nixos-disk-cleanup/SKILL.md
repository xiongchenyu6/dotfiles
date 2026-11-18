---
name: nixos-disk-cleanup
description: 排查和清理 NixOS 磁盘空间不足，包括 --keep-failed 构建目录、Nix GC、Python 包缓存和系统日志。
---

- 用 `df -B1 /` 记录可用空间。先按已知目录量体积，避免一开始反复扫描整个 store。检查 `nix-gc.timer` 和最近 GC 日志，区分自动回收是否正常。
- 优先处理可重建缓存，使用 `uv cache clean` 和 `pip cache purge`。项目、下载文件、模型、会话记录需要按用户指定范围处理。
- `--keep-failed` 的调试目录不由 store GC 清除。先确认没有运行中的构建，再核对待删目录确为已失败且无需调试的构建。只删除确认过的目录。
- 系统日志用 `journalctl --vacuum-size` 清理归档日志。Nix store 由 Nix 回收，保留所需的回滚代际。管理员权限不可用时，先完成能执行的清理并说明受阻项。
- 清理完重跑 `df -B1 /`，用可用空间差值汇报实际释放量。目录统计和工具的删除字节数可能包含硬链接，不能直接相加作为实测结果。
