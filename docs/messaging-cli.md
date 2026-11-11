# WhatsApp and Telegram CLI

The `cli-server` Home Manager tier (also imported by `cli-development`) installs
`wacli` 0.26.0 and `tg` 0.11.0 on Apple Silicon macOS and x86_64/aarch64 Linux.
Intel macOS archives are mapped too, but the current nixpkgs input has dropped
x86_64-darwin support; that platform needs a compatible nixpkgs revision.
Update and activate the device's usual dotfiles configuration to install them.
The packages are also exposed independently:

```sh
nix run .#wacli -- --help
nix run .#tg -- --help
```

These are third-party personal-account clients, not the desktop apps or bot-only APIs.
Pinned upstream release archives and SHA-256 hashes live in
`packages/messaging-cli.nix`. Release builds of gotd/cli embed public application
credentials; building it from source instead requires your own Telegram API app.
The upstream wacli macOS binaries require macOS 15 or newer.

## Authenticate each device

```sh
wacli auth --history-days 1 --history-max-per-chat 20
tg init
tg login
```

Scan WhatsApp's QR from Settings → Linked devices → Link a device.
Scan Telegram's QR from Settings → Devices → Link Desktop Device; enter the
Telegram two-step verification password locally if prompted. If a QR expires,
restart the corresponding login command. Do not run `tg init` over an existing
custom configuration unnecessarily.

Verify without sending messages:

```sh
wacli auth status --json
wacli --read-only --json chats list --limit 3
wacli --read-only --json messages list --limit 3
tg whoami -o json
tg chats list --limit 3 -o json
tg history me --limit 1 -o json
```

WhatsApp search reads its local index. Refresh with `wacli sync`, or keep a
foreground sync running with `wacli sync --follow`. Initial history is bounded
by the login flags above and what the phone supplies.

## Local state

Do not commit or copy account sessions into this repository. Pair devices separately.
`wacli` defaults to `~/.wacli` on macOS and `~/.local/state/wacli` on Linux
(subject to XDG settings). `tg` uses the `gotd` directory inside the OS config
directory, and stores macOS sessions in the login Keychain by default.

If an older manually installed `~/.local/bin/wacli` or `~/.local/bin/tg` shadows
the managed command, inspect `type -a wacli tg` after activation and remove only
the obsolete manually installed executable. Keep all session data.

## WeChat: agent-wechat (`wx`)

The CLI tier also installs `agent-wechat` 2.3.1 on Apple Silicon macOS and
x86_64/aarch64 Linux. Run it separately with `nix run .#agent-wechat -- --help`.
This is an unofficial independent client, not a wrapper around the Mac app.
The package installs commands only; it does not start a service or log in during activation.

```sh
wx daemon start
wx accounts create --alias personal
wx --account personal auth login
wx --account personal chats list
wx --account personal messages list filehelper --limit 1
# Stop when not needed:
wx daemon stop
```

Phone approval is required. A new profile has no complete old history; it collects
messages received after login. Existing official Mac/Windows profile reuse is
unverified; do not use `--official` on those platforms. The separately documented
Linux takeover mode needs the official client stopped and a backup, and is not
part of this setup. Do not re-sign, downgrade, or modify the main WeChat app.

State, API credentials and login QR files stay in the per-device data directory
(default `~/.local/share/agent-wechat` on this Mac), outside dotfiles. The local
service is not exposed to other machines by this configuration. Other devices
can install the same CLI and authenticate separately. Login acceptance and
simultaneous-client behavior depend on WeChat; installation alone does not
establish account connectivity. FFmpeg/FFprobe are needed for some media features.

Sources: https://github.com/thisnick/agent-wechat and
https://thisnick.github.io/agent-wechat/setup/connect/ .
