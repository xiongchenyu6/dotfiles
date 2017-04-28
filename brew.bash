sudo -v

# Keep-alive: update existing `sudo` time stamp until the script has finished.
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Check for Homebrew,
# Install if we don't have it
if test ! $(which brew); then
  echo "Installing homebrew..."
  yes "" | ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew tap homebrew/versions
brew tap homebrew/dupes
brew tap Goles/battery

# Make sure we’re using the latest Homebrew.
brew update
apps=(
    google-chrome
    sequel-pro
    java
    iterm2
)
brew cask install "${apps[@]}"

services=(
    node
    tmux
    vim
    mariadb
    git
)
# Install and Setup MySQL
brew install "${services[@]}"
brew cleanup

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
chsh -s /usr/local/bin/zsh

git clone git://github.com/zsh-users/zsh-autosuggestions $ZSH_CUSTOM/plugins/zsh-autosuggestions

git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k

cd ~/.oh-my-zsh/custom/themes/powerlevel9k/
git remote add dritter https://github.com/dritter/powerlevel9k.git
git fetch dritter
git checkout -t dritter/async_all_the_segments

brew tap caskroom/fonts
brew cask install font-Inconsolata-nerd-font-mono
brew services start mariadb
