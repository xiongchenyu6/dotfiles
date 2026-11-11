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
