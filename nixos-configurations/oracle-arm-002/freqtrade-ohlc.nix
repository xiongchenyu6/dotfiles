{ config, ... }:
{
  # The `quant` role secret already exists for postgres.nix; we reuse it to
  # build a TIMESCALE_URL pointing at the local TimescaleDB hypertable.
  sops.templates."ohlc-sync.env" = {
    content = ''
      TIMESCALE_URL=postgres://quant:${
        config.sops.placeholder."oracle-arm-002/quant-password"
      }@127.0.0.1:5432/api
    '';
    owner = "freqtrade-ohlc";
  };

  services.freqtrade-ohlc-sync = {
    enable = true;
    pairs = [
      "BTC/USDT"
      "ETH/USDT"
      "BNB/USDT"
      "SOL/USDT"
    ];
    timeframes = [ "1m" ];
    schema = "quant";
    backfillDays = 7;
    onCalendar = "*:0/15";
    refreshViews = [
      "ohlc_15m"
      "ohlc_1h"
      "ohlc_1d"
    ];
    environmentFile = config.sops.templates."ohlc-sync.env".path;
  };
}
