{
  lib,
  pkgs,
  ...
}:
let
  version = "1.0.5";

  sources = {
    aarch64-darwin = {
      arch = "macos-arm64";
      sha256 = "5b8b7f7e46a35bc3b20046b8a5a72d92aae7f388cdc15194cc6f0eb1fbc0aa78";
    };
    x86_64-darwin = {
      arch = "macos-x64";
      sha256 = "e74559e737a6a23aef1f8d6581fe9cc164dac03fa139844c90712cd6c636d7d9";
    };
  };

  platformSource = sources.${pkgs.stdenv.hostPlatform.system} or null;

  archive = pkgs.fetchurl {
    url = "https://github.com/shayann07/antigravity-computer-use/releases/download/v${version}/AntigravityComputerUse-${version}-${platformSource.arch}.tar.gz";
    inherit (platformSource) sha256;
  };

  setupPermissions = pkgs.writeShellScriptBin "antigravity-computer-use-setup-permissions" ''
    set -euo pipefail
    deployments="$HOME/Library/Application Support/AntigravityComputerUse/deployments.json"
    release=""
    if [[ -f "$deployments" ]]; then
      release=$(${pkgs.jq}/bin/jq -r '.global.release // empty' "$deployments" 2>/dev/null || true)
    fi
    if [[ -z "$release" || ! -d "$release/bin/AxonHost.app" ]]; then
      echo "antigravity-computer-use is not installed yet. Run darwin-rebuild switch first." >&2
      exit 1
    fi
    exec /usr/bin/open "$release/bin/AxonHost.app" --args --setup
  '';
in
lib.mkIf (pkgs.stdenv.hostPlatform.isDarwin && platformSource != null) {
  home.packages = [ setupPermissions ];

  # Upstream only publishes macOS and Windows bundles; install.mjs validates
  # bundle-manifest.json and requires writable payload files during staging, so
  # extract the Nix-fetched tarball into a temp dir and invoke install.command.
  home.activation.antigravityComputerUse = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    install_dir="$HOME/Library/Application Support/AntigravityComputerUse"
    deployments_file="$install_dir/deployments.json"
    skill_file="$HOME/.gemini/config/skills/computer-use/SKILL.md"
    sidecar_dir="$HOME/.gemini/config/sidecars/antigravity-computer-use"

    current_release=""
    current_version=""
    if [[ -f "$deployments_file" ]]; then
      current_release=$(${pkgs.jq}/bin/jq -r '.global.release // empty' "$deployments_file" 2>/dev/null || true)
    fi
    if [[ -n "$current_release" && -f "$current_release/bundle-manifest.json" ]]; then
      current_version=$(${pkgs.jq}/bin/jq -r '.version // empty' "$current_release/bundle-manifest.json" 2>/dev/null || true)
    fi

    if [[ "$current_version" != "${version}" || ! -f "$skill_file" || ! -d "$sidecar_dir" ]]; then
      session_file="$install_dir/data/desktop-session.json"
      running_pid=""
      if [[ -f "$session_file" ]]; then
        running_pid=$(${pkgs.jq}/bin/jq -r '.pid // empty' "$session_file" 2>/dev/null || true)
      fi

      if [[ -n "$running_pid" ]] && kill -0 "$running_pid" 2>/dev/null; then
        echo "antigravity-computer-use: desktop service (pid $running_pid) is running; skipping update to ${version}." >&2
      else
        tmp_dir=$(${pkgs.coreutils}/bin/mktemp -d)
        trap 'rm -rf "$tmp_dir"' EXIT
        $DRY_RUN_CMD ${pkgs.gnutar}/bin/tar -xzf "${archive}" -C "$tmp_dir"
        $DRY_RUN_CMD "$tmp_dir/macos-bundle/install.command"
        rm -rf "$tmp_dir"
        trap - EXIT
      fi
    fi
  '';
}
