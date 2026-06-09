{
  inputs,
  pkgs,
  config,
  ...
}:
# US-equity HonestTrend live node (NautilusTrader via Interactive Brokers), PAPER only.
# Counterpart to oracle-arm-002/nautilus.nix, but for the equity engine. Co-located with
# the IB Gateway container (ib-gateway.nix) on this x86_64 box.
#
# No sops secrets here: this node connects to the LOCAL Gateway over the mesh and places
# only simulated paper orders. The IB account id (DUQ654554) is not a secret, and there are
# no exchange API keys (the Gateway holds the IBKR login; see ib-gateway.nix). The node's
# own PAPER-ONLY guardrail refuses any non-DU* account / non-paper mode.
#
# CONNECTION: ib-gateway.nix publishes the API on the WireGuard mesh address only
# (`${wgAddr}:4002:4004`), NOT on 127.0.0.1 — so the host-local client must dial the mesh
# IP, not loopback. Hence host = "172.22.240.97" (overriding the module's 127.0.0.1 default).
{
  services.nautilus-equity-trend = {
    enable = true;
    # The nur overlay isn't applied globally on this host — reference the packages directly,
    # exactly like oracle-arm-002/nautilus.nix does for the crypto services. The equity node
    # additionally needs the IB adapter's `ibapi` module (nautilus-ibapi).
    package = inputs.xiongchenyu6.packages.${pkgs.stdenv.hostPlatform.system}.nautilus-trader;
    ibapiPackage = inputs.xiongchenyu6.packages.${pkgs.stdenv.hostPlatform.system}.nautilus-ibapi;

    # The Gateway API is published on the mesh IP only (see header). 127.0.0.1 won't connect.
    host = "172.22.240.97";
    port = 4002; # Gateway paper (mapped to the container's socat relay).
    clientId = 8; # dedicated live-node client id (5 = download_ib, 7 = order test).
    account = "DUQ654554"; # PAPER account (DU*). The node refuses anything else.

    # Recommended live config (STRATEGY_LEADERBOARD US Equity): 1h bars, EMA 50/100.
    barSpec = "1-HOUR-LAST-EXTERNAL";
    emaFast = 50;
    emaSlow = 100;

    # The IB paper account base currency is SGD; the strategy sizes in USD (quote ccy),
    # so convert (~0.74 USD per SGD). Without this it sizes off raw SGD and over-buys ~1.35x.
    quotePerBaseFx = 0.74;

    # Persist fills / closed positions to quant.nautilus_trades with asset_class='equity'.
    environment = "testnet"; # NAUTILUS_ENV row tag — paper, non-real money.
    environmentFile = config.sops.templates."nautilus-equity.env".path;
  };

  # TIMESCALE_URL for the trade ledger. quant-password is in common.yaml (encrypted to all
  # hosts incl. amd-002). DB reached over the public Internet (db.panda.qzz.io), not the mesh.
  sops.secrets."oracle-arm-002/quant-password" = { };
  sops.templates."nautilus-equity.env" = {
    content = ''
      TIMESCALE_URL=postgres://quant:${config.sops.placeholder."oracle-arm-002/quant-password"}@db.panda.qzz.io:5432/api?sslmode=require
    '';
    owner = "nautilus";
  };
}
