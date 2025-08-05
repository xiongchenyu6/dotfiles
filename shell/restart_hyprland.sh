#!/usr/bin/env bash

echo "Restarting Hyprland..."

# Save current workspace
CURRENT_WS=$(hyprctl activewindow -j | jq -r '.workspace.id' 2>/dev/null || echo "1")

# Method 1: Try graceful exit first
hyprctl dispatch exit

# If that doesn't work after 3 seconds, force kill
sleep 3
if pgrep -x Hyprland > /dev/null; then
    echo "Graceful exit failed, force killing..."
    pkill -9 Hyprland
fi

echo "Hyprland stopped. You should be returned to your display manager."
echo "Last workspace was: $CURRENT_WS"