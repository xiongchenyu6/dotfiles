{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.unit-status-telegram;
in
{
  options.services.unit-status-telegram = {
    enable = mkEnableOption "Enables unit-status-telegram service";

    telegramKey = mkOption {
      description = "telegram key";
      type = types.str;
    };
    chatId = mkOption {
      description = "chat id";
      type = types.int;
    };
  };
  config = {
    systemd.services = mkIf cfg.enable {
      "unit-status.telegram@" = {
        description = "Bttc Service Daemon";
        path = [ pkgs.curl ];
        wantedBy = [ "multi-user.target" ];
        after = [ "networking.target" ];
        script = ''
          UNIT=$1
          UNITSTATUS=$(systemctl status $UNIT)
          ALERT=$(echo -e "\u26A0")
          MESSAGE="$ALERT Unit failed $UNIT $ALERT
          Status:
          $UNITSTATUS"
          curl -s -X POST https://api.telegram.org/bot${cfg.telegramKey}/sendMessage -d chat_id=${toString cfg.chatId} -d text=$MESSAGE
        '';
      };
    };
  };
}





