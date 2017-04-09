confirmMessage() {
        read -p "$1 ([y]es or [N]o): " REPLY
    case $(echo $REPLY | tr '[A-Z]' '[a-z]') in
        y|yes) echo "yes" ;;
        *)     echo "no" ;;
    esac
}

ROOT=$(cd "$(dirname "$1")"; pwd -P) # dotfiles directory

./dots/installer.bash "$ROOT/dots";

ANSWER=$(confirmMessage "Do you wanna delete backup dot files")

if [ "yes" = $ANSWER ]; then
    echo "Delete all back up files"
    rm -rf ~/dotfiles_old*;
else
    echo "Skipped.";
    exit 0;
fi
