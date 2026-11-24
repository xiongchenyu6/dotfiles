{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  computerUsePackage =
    inputs.xiongchenyu6.packages.${pkgs.stdenv.hostPlatform.system}.computer-use-linux;
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [ computerUsePackage ];

  home.sessionVariables = {
    YDOTOOL_SOCKET = "/run/ydotoold/socket";
  };

  dconf.settings."org/gnome/desktop/interface" = {
    toolkit-accessibility = true;
  };

  home.activation.configureComputerUseLinuxMcp = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    mcp_dir="$HOME/.gemini/config"
    mcp_config="$mcp_dir/mcp_config.json"
    $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir -p "$mcp_dir"

    if [[ -s "$mcp_config" ]] && ${pkgs.jq}/bin/jq -e 'type == "object"' "$mcp_config" >/dev/null 2>&1; then
      source="$mcp_config"
    else
      source=""
    fi

    entry=$(${pkgs.jq}/bin/jq -n \
      --arg cmd "${computerUsePackage}/bin/computer-use-linux" \
      '{
        command: $cmd,
        args: ["mcp"],
        env: {
          YDOTOOL_SOCKET: "/run/ydotoold/socket"
        }
      }')

    if [[ -z "$source" ]]; then
      ${pkgs.jq}/bin/jq -n \
        --argjson entry "$entry" \
        '{ mcpServers: { "computer-use-linux": $entry } }' \
        > "$mcp_config.tmp"
    else
      ${pkgs.jq}/bin/jq \
        --argjson entry "$entry" \
        '.mcpServers = ((.mcpServers // {}) + { "computer-use-linux": $entry })' \
        "$source" > "$mcp_config.tmp"
    fi

    $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod 600 "$mcp_config.tmp"
    $DRY_RUN_CMD ${pkgs.coreutils}/bin/mv -f "$mcp_config.tmp" "$mcp_config"
  '';
}
