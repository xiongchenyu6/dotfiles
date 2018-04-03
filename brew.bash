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
    font-inconsolata-nerd-font
    font-source-code-pro
    amethyst
)

brew install wakatime-cli
brew install hub
brew install fasd
brew install openssl
brew install git-crypt
brew install haskell-stack
brew install aspell
brew install scala
brew install neovim
brew install watchman
brew install w3m
brew install fasd
brew install poppler
brew install automake
brew install msmtp
brew install isync
brew install node
brew install tmux
brew install mysql
brew install gnupg
brew install git
brew install git-crypt
brew install gnuplot
brew install python3
brew install sbt
brew install the_platinum_searcher
brew install reattach-to-user-namespace
brew install ditaa
brew install plantuml
brew install terminal-notifier
brew install emacs-plus --HEAD --with-natural-title-bars
brew install mu --with-emacs
brew install global --with-pygments --with-ctags

brew cask install "${apps[@]}"
brew cleanup

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
chsh -s /usr/local/bin/zsh

git clone git://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k

cd ~/.oh-my-zsh/custom/plugins && git clone https://github.com/wbingli/zsh-wakatime.git

curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim


brew services start mysql
brew services start emacs-plus
brew linkapps emacs-plus

mkdir ~/Maildir
mkdir ~/Maildir/office
mkdir ~/Maildir/gmail
