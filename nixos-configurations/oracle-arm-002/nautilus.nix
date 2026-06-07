{
  config,
  inputs,
  pkgs,
  ...
}:
{
  # Binance Ed25519 credentials. The api_secret is the Ed25519 private-key PEM
  # (multi-line), so it is materialized as its own secret file and referenced by
  # path; the api_key is a single-line value carried in the env template.
  sops.secrets."oracle-arm-002/binance-api-key" = { };
  sops.secrets."oracle-arm-002/binance-api-secret" = {
    owner = "nautilus";
  };

  sops.templates."nautilus-accumulator.env" = {
    content = ''
      BINANCE_API_KEY=${config.sops.placeholder."oracle-arm-002/binance-api-key"}
      BINANCE_API_SECRET_FILE=${config.sops.secrets."oracle-arm-002/binance-api-secret".path}
    '';
    owner = "nautilus";
  };

  services.nautilus-accumulator = {
    enable = true;
    # nur overlay isn't applied globally on this host — reference the package directly.
    package = inputs.xiongchenyu6.packages.${pkgs.stdenv.hostPlatform.system}.nautilus-trader;

    testnet = true; # SAFETY: stays testnet until a long clean soak is proven.
    instrument = "BTCUSDT.BINANCE";
    barSpec = "1-HOUR-LAST-EXTERNAL"; # hourly: exercises the order/reconcile path during soak
    intervalBars = 1;
    baseBuyUsd = 50.0;
    mode = "smart";
    environmentFile = config.sops.templates."nautilus-accumulator.env".path;
  };
}
