## Stage 1: Shared profile
**Goal**: Add the dotfiles workstation composition and shared desktop behavior.
**Success Criteria**: Module evaluation exposes Rio, Rime, and Claude Desktop without Unity.
**Tests**: Evaluate the Home Manager options and package paths.
**Status**: Complete

## Stage 2: Consumer cleanup
**Goal**: Point autolife at the shared module and keep Isaac Sim host-specific.
**Success Criteria**: Both hosts evaluate with the intended package split.
**Tests**: Evaluate both NixOS configurations and inspect Home Manager output.
**Status**: In Progress

## Stage 3: Deployment
**Goal**: Push and activate both hosts.
**Success Criteria**: Both system profiles use the new closures and runtime checks pass.
**Tests**: SSH runtime command, Fcitx, Niri, systemd, and kernel-log checks.
**Status**: Not Started
