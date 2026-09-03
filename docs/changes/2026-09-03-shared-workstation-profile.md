# Shared workstation profile

## Goal

Make `dotfiles` the single source of truth for the workstation tools used by
`sg-office` and `gz-pc`, with Rio and Rime active on both machines and Isaac Sim
remaining an explicit `sg-office`-only workload.

## In scope

- Add one thin `homeModules.workstation` module in `dotfiles`.
- Compose the existing `zsh`, `cli-development`, `gui`, `stow-config`, `niri`,
  and `tmux` modules from that profile.
- Include the dotfiles NVIDIA Home Manager module only on NVIDIA hosts that do
  not use PRIME offload.
- Manage the Fcitx5 profile in dotfiles and make Rime the default input method.
- Keep Rio as the only terminal supplied by the shared profile.
- Remove autolife's now-unused Ghostty flake input.
- Install Claude Desktop from `llm-agents.nix` on Linux workstations and remove
  the unused standalone Claude Desktop flake input.
- Remove Unity from the shared workstation tool set.
- Delete autolife's duplicate `home-modules/nixos-desktop.nix`.
- Make autolife's `freeman.xiong` GUI configuration consume
  `inputs.dotfiles.homeModules.workstation` as one flake input module.
- Keep `gui-omniverse.nix` in autolife and enable it only for hosts tagged
  `isaac-sim`.
- Add the `isaac-sim` tag to `sg-office`; do not add it to `gz-pc`.
- Push directly to each repository's `main` branch and deploy both workstations.

## Explicitly out of scope

- No compatibility module for the deleted autolife desktop module.
- No updater daemon, unlocked input, or automatic dotfiles tracking.
- No Isaac Sim package, command, or desktop entry on `gz-pc`.
- No changes to the unrelated local Caps Lock mapping or disk recovery module
  in dotfiles.
- No changes to unrelated autolife services, secrets, or host onboarding work.

## Assumptions

- Relative imports inside `workstation.nix` resolve dotfiles modules even when
  the module is consumed by autolife; consumer-provided `ezModules` are not used.
- The existing dotfiles GUI module remains the owner of Rio, desktop packages,
  and Fcitx5.
- `sg-office` continues to deploy from `main` through comin; it is not switched
  concurrently with a manual rebuild.
- `gz-pc` evaluates and builds its final closure on `gz-pc` itself.
- Existing unrelated dirty worktree changes are preserved and excluded from
  commits created for this change.

## Behavior scenarios

### Shared terminal and input method

Requirement: both workstations receive the same terminal and input method from
dotfiles.

WHEN the new Home Manager generation is activated on `sg-office` or `gz-pc`,
THEN Rio is installed, Alacritty and Ghostty are absent, and the active Fcitx5
input method is Rime.

### Host-specific Isaac Sim

Requirement: Isaac Sim is available only where explicitly requested.

WHEN the profile is evaluated for `sg-office`, THEN the Isaac Sim package,
command, and desktop entry are present.

WHEN the profile is evaluated for `gz-pc`, THEN none of those Isaac Sim outputs
are present.

### Shared desktop applications

Requirement: the shared Linux workstation profile includes Claude Desktop but
does not include Unity.

WHEN either workstation activates the profile, THEN Claude Desktop comes from
the `llm-agents.nix` package set and Unity Hub/CLI are absent.

## Acceptance criteria

- `nixosConfigurations.gz-pc.config.system.build.toplevel` evaluates.
- `nixosConfigurations.sg-office.config.system.build.toplevel` evaluates.
- Evaluated Home Manager configuration for both hosts reports Rio enabled,
  Alacritty disabled, Ghostty disabled, and `DefaultIM=rime`.
- Evaluated Home Manager configuration reports Claude Desktop present and Unity
  absent on both hosts.
- `gz-pc` has no Isaac package, command, or desktop entry.
- `sg-office` retains the Isaac command and desktop entry.
- autolife's `flake.lock` references the pushed dotfiles commit.
- Both deployments exit successfully and their current system profiles point to
  the new closures.
- At runtime on both hosts, `command -v rio` and `command -v claude-desktop`
  succeed while the old terminal and Unity commands fail.
- At runtime, `fcitx5-remote -n` reports `rime`.
- Rio can be launched and is visible in `niri msg windows`.
- Neither host gains new failed systemd units or relevant kernel hardware errors.

## Risk

The main risk is evaluating dotfiles Home Manager modules through autolife's
module argument set. The shared module therefore uses only relative imports and
is evaluated for both hosts before any deployment.

## Acceptance evidence

Pending implementation and deployment.
