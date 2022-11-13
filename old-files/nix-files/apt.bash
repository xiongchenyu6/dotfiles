# sudo apt-get install git exuberant-ctags ncurses-term curl

git clone git://github.com/zsh-users/zsh-autosuggestions "$ZSH_CUSTOM/plugins/zsh-autosuggestions"

git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting"

git clone https://github.com/bhilburn/powerlevel9k.git ~/.oh-my-zsh/custom/themes/powerlevel9k

cd ~/.oh-my-zsh/custom/themes/powerlevel9k/ || exit

curl -fsSL https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf mv PowerlineSymbols.otf ~/.fonts/ fc-cache -vf ~/.fonts/
