#!/usr/bin/env bash

############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
# If symlinks already exists, they are skipped, otherwise destiny files are moved to dotfiles
# folder and respective symlinks created.
# Note: A backup folder is created with all .dotfiles (not symlinks) desired
############################
########## Functions

confirmMessage() {
	read -r -p "$1 ([m]ac or [l]inux): " REPLY
	case $(echo "$REPLY" | tr '[:upper:]' '[:lower:]') in
	m | mac) echo "mac" ;;
	*) echo "linux" ;;
	esac
}

ANSWER=$(confirmMessage "Witch version do you want to install")

if [ "$ANSWER" = "mac" ]; then
	echo "mac"
else
	echo "linux"
fi

########## Variables
TIME=$(date "+%Y-%m-%d_%H-%M-%S")
DIR=${1:-$(
	cd "$(dirname "$1")" || exit
	pwd -P
)}                            # dotfiles directory
OLDDIR=~/dotfiles_old_"$TIME" # old dotfiles backup directory

#Use neovim rather than vim 8
FILES=(zshenv authinfo.gpg curlrc ctags mbsyncrc msmtprc tmux.conf aria2.conf zshrc gitconfig global_ignore ideavimrc ensime-server.conf editorconfig) # list of symlink

DIRECTORYS=(config oh-my-zsh spacemacs.d password monad)

##########
# create dotfiles_old in homedir
echo "Creating $OLDDIR for backup of any existing dotfiles in ~"
mkdir -p "$OLDDIR"
echo "...done"

# change to the dotfiles directory
echo "Changing to the $DIR directory"
cd "$DIR" || exit
echo "...done"

# copy any existing dotfiles (follow symlinks) in homedir to dotfiles_old_$time directory
for FILE in "${FILES[@]}"; do
	echo "Backup dotfile $FILE from ~ to $OLDDIR"
	cp -L "$HOME/.$FILE" "$OLDDIR"
done

for DIRECTORY in "${DIRECTORYS[@]}"; do
	echo "Backup dotfile $DIRECTORY from ~ to $OLDDIR"
	cp -R "$HOME/.$DIRECTORY" "$OLDDIR"
done

# move any existing dotfiles (not symlinks) in homedir to dotfiles directory and create respective symlinks
for FILE in "${FILES[@]}"; do
	echo "Moving and creating symlink to $FILE from ~ to $DIR."
	ln -sf "$DIR/$FILE" "$HOME/.$FILE"
done

for DIRECTORY in "${DIRECTORYS[@]}"; do
	echo "Moving and creating symlink to $DIRECTORY from ~ to $DIR."
	ln -sf "$DIR/$DIRECTORY" "$HOME/.$DIRECTORY"
done
