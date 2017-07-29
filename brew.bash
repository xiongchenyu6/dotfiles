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

taps=(
    d12frosted/emacs-plus
    caskroom/fonts
    caskroom/cask
    neovim/neovim
)

brew tap "${taps[@]}"

apps=(
    dropbox
    google-chrome
    sequel-pro
    java
    iterm2
    qq
    robomongo
    obs
    flux
    react-native-debugger
    dropbox
    neteasemusic
    xquartz
    font-inconsolata-nerd-font-mono 
    font-source-code-pro
)
brew cask install "${apps[@]}"

services=(
    fasd
    openssl
    git-crypt
    terminal-notifier
    haskell-stack
    ispell
    scala
    neovim/neovim/neovim
    w3m
    fasd
    'mu --with-emacs'
    cmake
    ledger
    emacs-plus
    poppler
    automake
    msmtp
    isync
    node
    tmux
    mariadb
    gnupg
    git
    git-crypt
    imagemagick
    gnuplot
    python3
    sbt
    ctags
    the_platinum_searcher
    ditaa
    plantuml
    watchman
    yarn
    reattach-to-user-namespace
)
# Install and Setup MySQL
brew install "${services[@]}"
brew cleanup

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
chsh -s /usr/local/bin/zsh

# git clone git://github.com/zsh-users/zsh-autosuggestions $ZSH_CUSTOM/plugins/zsh-autosuggestions

# git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

# git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k

brew cask install font-Inconsolata-nerd-font-mono
brew services start mariadb
brew services start emacs-plus
brew linkapps emacs-plus
