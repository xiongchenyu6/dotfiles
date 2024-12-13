# Dotfiles Settings [![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)
This repository contains my personal settings using Nix.
# Features
- flakes
- deploy with nixos-rebuild switch --flake .#mail --use-substitutes --target-host root@mail --use-remote-sudo
- use the latest Linux kernel
- use [nixos-hardware](https://github.com/NixOS/nixos-hardware)
- use [srvos](https://github.com/nix-community/srvos)
- Full bcachefs disk encryption on laptop
- wayland hyprland & x11 xmonad
- dn42 bgp & babel
- digital ocean & aws cloud host
- programmer dvorak layout
- impermanence
- sops

- overlays
  - emacs-overlay
  - nur
  - vscode-plugins-overlay

- split-dns with systemd-resolved
- netbird
- vmess
- openvpn

![Terminal Previews](./previews/terminal.png)

## Usage

### For macOS

To use this, simply run the `brew.bash` installation shell script.

### For NixOS

To use the `apt.bash` installation shell script.

### For Arch Linux

``` shell
pacman -S --needed - < pkglist.txt
```

## My daily application list

| Usage              | Application                     |
|--------------------|---------------------------------|
| Email              | <imap:mbsync> <smtp:msmtp> gnus |
| Crypt              | gnu2 sops                       |
| Editor             | Emacs, neovim                   |
| Shell              | Zsh                             |
| Shell manager      | Oh-my-zsh                       |
| Searcher           | rg, atuin                       |
| Termial            | alacritty                       |
| Chat               | QQ, Wechat, Erc                 |
| Presentation       | Reveal.js                       |
| GTD                | Org Agenda                      |
| Notes              | Org Roam                        |
| Graph Draw         | Plantuml, Ditaa, Gnuplot        |
| Documentation View | Dash                            |
| Book Writing       | Gitbook                         |
| Finance            | Ledger                          |
| Auto Deploy        | Systemd, Docker                 |
| Academic Writing   | Pandadoc, Org mode              |
| Desktop            | Xmonad -\> hyprland             |

## Tips for the Chinese settings problem on Ubuntu

This is how to set up Chinese input in Ubuntu

``` shell
gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us'), ('ibus', 'Unikey')]"
```

## My roadmap

I did love and enjoy the key-bindings of vim, from the day I switch from
the emacs, but I switch back shortly because of the 2 main reasons.

1.  Emacs plugins works on my windows but the my vim got some problem
    and I have to dirty my configuration files so much to fixed.
2.  I need to use playframework for development and the emacs support
    ensime very well.

I move from emacs to vim then to neovim and now I am using the
spacemacs, I think the configuration for everyone will become the same
after long period of learning and copying from others, so it is good to
give the community driven software a try.

## Gist to immigrate my private keys

``` shell
cp /path/to/backups/*.gpg ~/.gnupg/
#Inport gpg
gpg --import-ownertrust chrisroos-ownertrust-gpg.txt
```


## Windows Wsl2

## network topology
![Main Previews](./previews/main.svg)
![Network Previews](./previews/network.svg)

```bash
# Windows WSL2
wsl --set-version Ubuntu-20.04 2
wsl --set-default-version 2
wsl --list --verbose
wsl --set-version Ubuntu-20.04 2

# Windows Terminal
# https://docs.microsoft.com/en-us/windows/terminal/tutorials/powerline-setup
#
sudo nix-env --list-generations --profile /nix/var/nix/profiles/system
```

```bash
nixos-rebuild switch --option substituters "https://mirror.sjtu.edu.cn/nix-channels/store" --use-substitutes --flake .#huawei-bj-001 --target-host root@1.94.246.7
```
