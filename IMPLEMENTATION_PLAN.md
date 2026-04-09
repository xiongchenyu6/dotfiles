## Stage 1: Replace openclaw flake input with zeroclaw
**Goal**: Point the flake input from `github:openclaw/nix-openclaw` to `github:zeroclaw-labs/zeroclaw`.
**Success Criteria**: `flake.nix` references `zeroclaw` input; openclaw input removed; shared-modules overlay list updated.
**Tests**: `nix flake show` resolves the new input.
**Status**: Complete

## Stage 2: Rewrite oracle-arm-002 for zeroclaw
**Goal**: Replace the openclaw-gateway NixOS module with a custom zeroclaw Rust build + systemd service.
**Success Criteria**: Host config builds zeroclaw from source via `rustPlatform.buildRustPackage`, generates TOML config, runs `zeroclaw daemon` via systemd, and includes a one-shot migration service.
**Tests**: `nix flake check`; `nixos-rebuild build --flake .#oracle-arm-002`.
**Status**: Complete

## Stage 3: Validate build and deploy
**Goal**: Verify the configuration evaluates and builds cleanly; deploy to oracle-arm-002.
**Success Criteria**: Host builds without errors; `zeroclaw-migrate.service` runs successfully on first boot; `zeroclaw.service` starts and connects to Telegram.
**Tests**: `nixos-rebuild switch --flake .#oracle-arm-002 --target-host root@<ip>`.
**Status**: Not Started
