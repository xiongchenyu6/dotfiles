sudo -v

# Keep-alive: update existing `sudo` time stamp until the script has finished.
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Check for Homebrew,
# Install if we don't have it
if test ! $(which brew); then
  echo "Installing homebrew..."
  yes "" | ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew update


brew tap caskroom/fonts
brew tap d12frosted/emacs-plus


apps=(
    dropbox
    google-chrome
    sequel-pro
    java
    iterm2
    qq
    dropbox
    intellij-idea
    font-inconsolata-nerd-font-mono
    font-source-code-pro
)
brew cask install "${apps[@]}"

services=(
    wakatime-cli
    hub
    fasd
    openssl
    git-crypt
    haskell-stack
    ispell
    scala
    neovim
    watchman
    w3m
    fasd
    poppler
    automake
    msmtp
    isync
    node
    tmux
    mysql
    gnupg
    git
    git-crypt
    gnuplot
    python3
    sbt
    the_platinum_searcher
    reattach-to-user-namespace
    ditaa
    plantuml
    terminal-notifier
    emacs-plus --HEAD --with-natural-title-bars
    mu --with-emacs
    global --with-pygments --with-ctags
)
# Install and Setup MySQL
brew install "${services[@]}"
brew cleanup

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
chsh -s /usr/local/bin/zsh

git clone git://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k

cd ~/.oh-my-zsh/custom/plugins && git clone https://github.com/wbingli/zsh-wakatime.git

brew services start mysql
brew services start emacs-plus
brew linkapps emacs-plus

mkdir ~/Mail
mkdir ~/Mail/office
mkdir ~/Mail/gmail