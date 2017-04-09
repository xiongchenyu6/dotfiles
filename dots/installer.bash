#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
# If symlinks already exists, they are skipped, otherwise destiny files are moved to dotfiles
# folder and respective symlinks created.
# Note: A backup folder is created with all .dotfiles (not symlinks) desired
############################

########## Variables
TIME=$(date "+%Y-%m-%d_%H-%M-%S")
DIR=${1:-$(cd "$(dirname "$1")"; pwd -P)} # dotfiles directory
OLDDIR=~/dotfiles_old_"$TIME" # old dotfiles backup directory
FILES=(bash_profile curlrc gvimrc emacs ctags muttrc tmux.conf vimrc zshrc gitconfig global_ignore) # list of files/folders to symlink in homedir

DIRECTORYS=(config oh-my-zsh local)
##########
# create dotfiles_old in homedir
echo "Creating $OLDDIR for backup of any existing dotfiles in ~"
mkdir -p "$OLDDIR"
echo "...done"

# change to the dotfiles directory
echo "Changing to the $DIR directory"
cd $DIR
echo "...done"

# copy any existing dotfiles (follow symlinks) in homedir to dotfiles_old_$time directory
for FILE in "${FILES[@]}"; do
echo "Backup dotfile $FILE from ~ to $OLDDIR"
cp -L ~/.$FILE "$OLDDIR"
done

for DIRECTORY in "${DIRECTORYS[@]}"; do
echo "Backup dotfile $DIRECTORY from ~ to $OLDDIR"
cp -R ~/.$DIRECTORY "$OLDDIR"
done

# move any existing dotfiles (not symlinks) in homedir to dotfiles directory and create respective symlinks
for FILE in "${FILES[@]}"; do
echo "Moving and creating symlink to $FILE from ~ to $DIR."
ln -sf $DIR/$FILE ~/.$FILE
done

for DIRECTORY in "${DIRECTORYS[@]}"; do
echo "Moving and creating symlink to $DIRECTORY from ~ to $DIR."
ln -sf $DIR/$DIRECTORY ~/.$DIRECTORY
done
