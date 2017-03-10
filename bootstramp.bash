confirmMessage() {
        read -p "$1 ([y]es or [N]o): " REPLY
    case $(echo $REPLY | tr '[A-Z]' '[a-z]') in
        y|yes) echo "yes" ;;
        *)     echo "no" ;;
    esac
}

ROOT=$(cd "$(dirname "$1")"; pwd -P) # dotfiles directory
echo "Moving and creating symlink to bin from /usr/local/bin/ to $ROOT/bin"
sudo ln -sfn $ROOT/bin /usr/local/bin;
./dots/installer.bash "$ROOT/dots";

ANSWER=$(confirmMessage "Do you wanna delete backup dot files")

if [ "yes" = $ANSWER ]; then
    echo "Delete all back up files"
    rm -rf ~/dotfiles_old*;
else
    echo "Skipped.";
    exit 0;
fi
