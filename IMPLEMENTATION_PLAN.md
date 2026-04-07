## Stage 1: Add cc-gateway package and module
**Goal**: Import the cc-gateway NixOS module and add the cc-gateway package to oracle-arm-002.
**Success Criteria**: `oracle-arm-002` imports `inputs.xiongchenyu6.nixosModules.cc-gateway` and includes `inputs.xiongchenyu6.packages.aarch64-linux.cc-gateway` in `environment.systemPackages`.
**Tests**: `nix flake check` or targeted host evaluation succeeds.
**Status**: Complete

## Stage 2: Configure SOPS-backed cc-gateway service
**Goal**: Declare `cc-gateway` secrets and enable `services.cc-gateway` using those files.
**Success Criteria**: Host config declares the needed SOPS secrets and a complete `services.cc-gateway` stanza.
**Tests**: Nix evaluation resolves all `config.sops.secrets.*.path` references without assertion failures.
**Status**: Complete

## Stage 3: Add placeholder secrets to SOPS
**Goal**: Add encrypted placeholder/random `cc-gateway` values to `secrets/common.yaml`.
**Success Criteria**: `cc-gateway` keys exist in the encrypted secrets file and decrypt correctly.
**Tests**: `sops -d secrets/common.yaml` shows the new keys.
**Status**: Complete

## Stage 4: Validate the host configuration
**Goal**: Verify the updated oracle-arm-002 configuration evaluates cleanly.
**Success Criteria**: Evaluation/build checks complete without config errors.
**Tests**: `nix flake check`; if needed, `nixos-rebuild build --flake .#oracle-arm-002`.
**Status**: Complete

## Stage 5: Stabilize openclaw-gateway dependency fetch
**Goal**: Add a host-local workaround for flaky `openclaw-gateway` pnpm dependency prefetching.
**Success Criteria**: `oracle-arm-002` overrides the upstream `pnpmDeps` fetch with retry tuning while preserving the existing runtime/install patches.
**Tests**: `nixos-rebuild build --flake .#oracle-arm-002` progresses past `openclaw-gateway-pnpm-deps`.
**Status**: In Progress

## Stage 6: Revalidate oracle-arm-002 build
**Goal**: Rebuild the host after the OpenClaw workaround.
**Success Criteria**: The previous `ECONNRESET` failure is gone or moved past the prior failing derivation.
**Tests**: `nixos-rebuild build --flake .#oracle-arm-002`.
**Status**: Not Started
