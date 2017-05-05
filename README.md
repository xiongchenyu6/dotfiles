# Freeman Settings

## Usage
To use this simple run the brew installation shell script.
## My application list
| Usage              | Application              |
|--------------------|--------------------------|
| Email              | imap:mbsync smtp:msmtp   |
| Crypt              | gnu2                     |
| Editor             | Emacs, neovim            |
| Ide                | Intellij                 |
| Termial            | Iterm2                   |
| Chat               | QQ, Wechat, Erc          |
| Presentation       | Reveal.js                |
| GTD                | Org Agenda               |
| Notes              | Onenote -> Org Mode      |
| Graph Draw         | Plantuml, Ditaa, Gnuplot |
| Documentation View | Dash                     |
| Book Writing       | Gitbook                  |
| Finance            | Ledger                   |
| Auto Deploy        | Pm2, Docker              |
| Academic Writing   | Pandadoc, Org mode       |

```bash
gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us'), ('ibus', 'Unikey')]"
```
## Update log
I did love and enjoy the key-bindings of vim, from the day I switch from the emacs, but I switch back shortly because of the 2 main reasons.
1. Emacs plugins works on my windows but the my vim got some problem and I have to dirty my configuration files so much to fixed.
2. I need to use playframework for development and the emacs support ensime very well.

I move from emacs to vim then to neovim and now I am using the spacemacs, I think the configuration for everyone will become the same after long period of learning and copying from others, so it is good to give the community driven software a try.

cp /path/to/backups/*.gpg ~/.gnupg/
# or, if you exported the ownertrust
gpg --import-ownertrust chrisroos-ownertrust-gpg.txt
