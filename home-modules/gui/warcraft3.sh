#!/usr/bin/env bash

# Warcraft 3 launcher
export WINEPREFIX="$HOME/.wine-warcraft3"
export LANG=zh_CN.UTF-8

# Wine compatibility settings for Warcraft 3
export WINEDEBUG=-all
export WINE_FORCE_LARGE_ADDRESS_AWARE=1

# Use virtual desktop (commented out for now)
# wine reg add "HKEY_CURRENT_USER\\Software\\Wine\\Explorer\\Desktops" /v Default /d "1024x768" /f >/dev/null 2>&1

# Warcraft 3 installation path
WC3_PATH="$WINEPREFIX/drive_c/Program Files (x86)/3DMGAME/Warcraft3"

if [ ! -d "$WC3_PATH" ]; then
    echo "Error: Warcraft 3 installation not found at $WC3_PATH"
    exit 1
fi

# Launch options
case "${1:-launcher}" in
    launcher)
        echo "Starting Warcraft III Launcher..."
        cd "$WC3_PATH"
        wine "PCgames_1.26CN.exe" "${@:2}"
        ;;
    reign|roc)
        echo "Starting Warcraft III: Reign of Chaos..."
        cd "$WC3_PATH"
        wine "War3.exe" -opengl -window "${@:2}"
        ;;
    frozen|tft)
        echo "Starting Warcraft III: The Frozen Throne..."
        cd "$WC3_PATH"
        wine "Frozen Throne.exe" -opengl -window "${@:2}"
        ;;
    editor)
        echo "Starting World Editor..."
        cd "$WC3_PATH"
        wine "World Editor.exe" "${@:2}"
        ;;
    *)
        echo "Usage: warcraft3 [launcher|reign|frozen|editor] [options]"
        echo "  launcher - Launch game launcher (default)"
        echo "  reign    - Launch Reign of Chaos directly"
        echo "  frozen   - Launch The Frozen Throne directly"
        echo "  editor   - Launch World Editor"
        exit 1
        ;;
esac