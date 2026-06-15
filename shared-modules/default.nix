# Shared configuration factory for both Darwin and NixOS modules
# This can be imported and used to generate consistent configuration
{
  inputs,
  lib,
}:
let
  masterPkgsFor =
    system:
    import inputs.nixpkgs-master {
      inherit system;
      config.allowUnfree = true;
    };

  # Common overlays shared between Darwin and NixOS
  baseOverlays =
    with inputs;
    map (x: x.overlays.default or x.overlay) [
      xiongchenyu6
      nix-alien
      sops-nix
      nix-topology
    ];

  # Additional overlays for NixOS — applied to ALL nixos hosts.
  # rust-web-server intentionally NOT in this list: it's a private SSH
  # input and only one host (huoshan-bj-001) actually deploys the service.
  # Adding it globally would force every host's eval to touch the private
  # repo, which fails for users without the deploy key. The overlay is
  # added locally in nixos-configurations/huoshan-bj-001/default.nix.
  nixosAdditionalOverlays =
    with inputs;
    map (x: x.overlays.default or x.overlay) [
      nur
    ];
in
{
  inherit baseOverlays nixosAdditionalOverlays;

  # Combined overlays for NixOS
  nixosOverlays =
    baseOverlays
    ++ nixosAdditionalOverlays
    ++ [
      (
        _: prev:
        let
          xorgCompat = {
            libX11 = prev.libx11;
            libXScrnSaver = prev.libxscrnsaver;
            libXcomposite = prev.libxcomposite;
            libXcursor = prev.libxcursor;
            libXdamage = prev.libxdamage;
            libXext = prev.libxext;
            libXfixes = prev.libxfixes;
            libXi = prev.libxi;
            libXmu = prev.libxmu;
            libXrandr = prev.libxrandr;
            libXrender = prev.libxrender;
            libXtst = prev.libxtst;
            libXv = prev.libxv;
            libxcb = prev.libxcb;
            libxkbfile = prev.libxkbfile;
            libxshmfence = prev.libxshmfence;
          };
          addPerf = kernelPackages: kernelPackages.extend (_: _: { perf = prev.perf; });
        in
        {
          # gnupg240 = nixpkgs-stable.legacyPackages.x86_64-linux.gnupg;
          # telegram-desktop =
          #   nixpkgs-stable.legacyPackages.x86_64-linux.telegram-desktop;
          # waybar = nixpkgs-master.legacyPackages.x86_64-linux.waybar;
          claude-code = (masterPkgsFor prev.stdenv.hostPlatform.system).claude-code;
          # cloudflared 2026.6.0's proxy SSE test can panic during checkPhase.
          # Keep the package buildable until nixpkgs carries an upstream fix.
          cloudflared = prev.cloudflared.overrideAttrs (_: {
            doCheck = false;
          });
          feishu-lark =
            if prev.stdenv.hostPlatform.isLinux then
              prev.callPackage "${inputs.xiongchenyu6}/pkgs/feishu-lark/package.nix" {
                xorg = prev.xorg // xorgCompat;
              }
            else
              throw "feishu-lark is only available on Linux";
          linuxPackages = addPerf prev.linuxPackages;
          linuxPackages_latest = addPerf prev.linuxPackages_latest;
          lowPrio = lib.lowPrio;
          netbird = prev.netbird.override {
            buildGoModule = prev.buildGo125Module;
          };
          record_screen =
            if prev.stdenv.hostPlatform.isLinux then
              prev.callPackage "${inputs.xiongchenyu6}/pkgs/record_screen/package.nix" { }
            else
              throw "record_screen is only available on Linux";
          sui =
            if prev.stdenv.hostPlatform.isLinux then
              prev.callPackage "${inputs.xiongchenyu6}/pkgs/sui/package.nix" { }
            else
              throw "sui is only available on Linux";
          xorg = prev.xorg // xorgCompat;
        }
        // lib.optionalAttrs (prev.stdenv.hostPlatform.system == "x86_64-linux") {
          microsoft-edge = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.microsoft-edge;
        }
      )
      # nixpkgs-unstable as of 2026-04-25 ships python3.13-cli-helpers 2.10.0
      # whose `tests/tabular_output/test_preprocessors.py::test_style_output*`
      # tests assert ANSI escapes equal `\x1b[39m` but newer Pygments emits
      # the joined `\x1b[39;49m` form. Cosmetic test breakage; output is
      # still functional. Drop doCheck until upstream fixes the assertion.
      # Affects pgcli + litecli (both depend on cli-helpers).
      (_: prev: {
        python313Packages = prev.python313Packages.overrideScope (
          _: pyprev: {
            cli-helpers = pyprev.cli-helpers.overridePythonAttrs (_: {
              doCheck = false;
            });
          }
        );
      })
      # nixpkgs pins wireshark 4.6.5 via fetchFromGitLab using a tag
      # ref. GitLab regenerated the v4.6.5 archive, so the recorded
      # hash drifted (`U30OJ8m+L/...` → `Zvrwxjp4LK2J3...`) and every
      # build that pulls wireshark/wireshark-cli/termshark fails. Pin
      # the new hash here until nixpkgs' update lands. Both wireshark
      # (qt) and wireshark-cli need patching: wireshark-cli is
      # `wireshark.override { withQt = false; }`, which re-invokes the
      # package with the original src, bypassing an override on
      # wireshark alone.
      (
        _: prev:
        let
          wiresharkSrc = prev.fetchFromGitLab {
            repo = "wireshark";
            owner = "wireshark";
            tag = "v4.6.5";
            hash = "sha256-Zvrwxjp4LK2J3QnxmPxKKrU01YHQvPyp54UWzeGNCjA=";
          };
          fixSrc =
            drv:
            drv.overrideAttrs (_: {
              src = wiresharkSrc;
            });
        in
        {
          wireshark = fixSrc prev.wireshark;
          wireshark-cli = fixSrc prev.wireshark-cli;
        }
      )
      # nixpkgs' activitywatch let-binds `aw-webui` inside aw-server-rust's
      # default.nix and exposes it only via `env.AW_WEBUI_DIR`. The current
      # unstable revision crashes jest with `Cannot find module
      # 'vue-template-compiler'` (transitive dep missing from the package
      # closure); the runtime bundle is fine. Reach the let-bound derivation
      # through aw-server-rust.env.AW_WEBUI_DIR and skip its checks.
      (_: prev: {
        aw-server-rust = prev.aw-server-rust.overrideAttrs (old: {
          env = old.env // {
            AW_WEBUI_DIR = old.env.AW_WEBUI_DIR.overrideAttrs (_: {
              doCheck = false;
            });
          };
        });
      })
      # aw-watcher-window-wayland emits the raw Wayland app_id (e.g.
      # "google-chrome", "firefox", "chromium"), but aw-webui's hardcoded
      # browser whitelist is in CamelCase ("Google-chrome", "Firefox", ...).
      # Without this remap, the Browser dashboard view stays empty even when
      # both the window watcher and the aw-watcher-web-* extension are
      # collecting data.
      (_: prev: {
        aw-watcher-window-wayland = prev.aw-watcher-window-wayland.overrideAttrs (old: {
          postPatch = (old.postPatch or "") + ''
            substituteInPlace src/main.rs \
              --replace-fail \
                'data.insert("app".to_string(), Value::String(window.appid.clone()));' \
                'data.insert("app".to_string(), Value::String(normalize_appid(&window.appid)));'
            cat >> src/main.rs <<'RUST_EOF'

            fn normalize_appid(appid: &str) -> String {
                match appid {
                    "google-chrome" => "Google-chrome".to_string(),
                    "google-chrome-stable" => "Google-chrome-stable".to_string(),
                    "google-chrome-beta" => "Google-chrome-beta".to_string(),
                    "google-chrome-unstable" => "Google-chrome-unstable".to_string(),
                    "chromium" => "Chromium".to_string(),
                    "chromium-browser" => "Chromium-browser".to_string(),
                    "brave-browser" => "Brave-browser".to_string(),
                    "firefox" => "Firefox".to_string(),
                    "firefox-esr" => "Firefox-esr".to_string(),
                    _ => appid.to_string(),
                }
            }
            RUST_EOF
          '';
        });
      })
    ];

  # Home Manager configuration shared between Darwin and NixOS
  homeManagerConfig = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "backup";
  };

  # Generate nixpkgs configuration with overlays
  mkNixpkgsConfig =
    overlays:
    (_: {
      nixpkgs = {
        inherit overlays;
      };
    });

  # Generate nixpkgs configuration with overlays and hostPlatform for NixOS
  mkNixosNixpkgsConfig =
    overlays:
    (_: {
      nixpkgs = {
        hostPlatform = lib.mkDefault "x86_64-linux";
        inherit overlays;
      };
    });
}
