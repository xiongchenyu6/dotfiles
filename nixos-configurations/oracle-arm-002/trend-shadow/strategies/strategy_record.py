"""House trend strategy (趋势突破策略) — pure signal math for the public track record.

Pure: no DB, no network, stdlib only. signal_evaluator.py feeds it Binance 1h klines (live
sweep + --backfill) and persists the events into quant.strategy_signals / strategy_assets
(migration 032).

The rule is the one nautilus_crypto/donchian.py trades (1h, 168/72, long-only, spot):
  entry: flat, and a closed bar's close > max(HIGH of the prior 168 bars)   (~7-day high)
  exit:  long, and a closed bar's close < min(LOW of the prior 72 bars)     (~3-day low)
The current bar is never part of its own lookback, and one bar can't both enter and exit
(exit is only checked when already long before the bar) — mirrors DonchianBreakout.on_bar.

Stats (win rate, compounded returns, buy&hold) are NOT computed here: the SQL view
quant.strategy_record is the single source of truth. net_return() only mirrors that view's
fee math for logs and --backfill --dry-run summaries.
"""

from __future__ import annotations

STRATEGY = "donchian_1h"
# Screened 2026-09-27 from Binance's top-30 USDT pairs using ONLY 2024-2025 data
# (pre-registered: 2 full years, net return > 0, max drawdown shallower than buy-and-hold,
# return / |max drawdown| >= 0.5), so 2026 is out-of-sample. See STRATEGY_LEADERBOARD.md.
ASSETS = ("BTC", "ETH", "SOL", "XRP", "DOGE", "ADA", "AVAX", "SUI", "NEAR", "UNI", "ZEC",
          "PEPE", "WLD")
ENTRY_LB = 168   # 7 days of 1h bars — breakout lookback
EXIT_LB = 72     # 3 days of 1h bars — trailing-low exit lookback
FEE = 0.001      # Binance spot taker, per side
HIGH_30D_LB = 720  # 30 days of 1h bars — dip reference for the opportunity scan
RECORD_START = "2026-01-01"  # UTC — start of the published record (buy&hold baseline)

def price_decimals(x: float) -> int:
    """Decimals that keep ~4 significant digits below $1 (PEPE trades near $0.00001);
    2 at or above $1. Shared by the Telegram texts and the share cards."""
    if x <= 0 or x >= 1:
        return 2
    d = 0
    while x < 0.1 and d < 12:
        x *= 10
        d += 1
    return d + 4


def assets_label(assets: list[str]) -> str:
    """'BTC/ETH/SOL' for a few assets, '13 个币种' once a list would not fit a line."""
    return "/".join(assets) if len(assets) <= 4 else f"{len(assets)} 个币种"


# (close_ts_ms, high, low, close) — CLOSED bars only, oldest → newest.
Bar = tuple[int, float, float, float]


def step(bars: list[Bar], position: dict | None, after_ms: int) -> list[dict]:
    """Replay the rule over bars closing after `after_ms`, starting from `position`.

    position: None (flat) or {"entry_ts": ms, "entry_price": float} (long).
    Only bars with a full ENTRY_LB lookback (index >= ENTRY_LB) are evaluated. Returns
    events in order: {"type": "entry"|"exit", "ts": close_ts_ms, "price": close,
    "level": the channel edge that was broken}.
    """
    events: list[dict] = []
    long = position is not None
    for i in range(ENTRY_LB, len(bars)):
        ts, _high, _low, close = bars[i]
        if ts <= after_ms:
            continue
        if not long:
            level = max(b[1] for b in bars[i - ENTRY_LB:i])
            if close > level:
                events.append({"type": "entry", "ts": ts, "price": close, "level": level})
                long = True
        else:
            level = min(b[2] for b in bars[i - EXIT_LB:i])
            if close < level:
                events.append({"type": "exit", "ts": ts, "price": close, "level": level})
                long = False
    return events


def channels(bars: list[Bar]) -> dict:
    """Channel edges for the NEXT bar's decision (the last closed bar is inside them).

    channel_high = max HIGH of the last ENTRY_LB bars (a close above it = entry),
    channel_low  = min LOW of the last EXIT_LB bars  (a close below it = exit).
    high_30d     = max HIGH of the last 30 days (720 bars) — the opportunity scan's dip test.
    An edge is None when there aren't enough bars for its full lookback.
    """
    if not bars:
        raise ValueError("channels() needs at least one closed bar")
    last_ts, _high, _low, last_close = bars[-1]
    return {
        "last_ts": last_ts,
        "last_close": last_close,
        "channel_high": max(b[1] for b in bars[-ENTRY_LB:]) if len(bars) >= ENTRY_LB else None,
        "channel_low": min(b[2] for b in bars[-EXIT_LB:]) if len(bars) >= EXIT_LB else None,
        "high_30d": max(b[1] for b in bars[-HIGH_30D_LB:]) if len(bars) >= HIGH_30D_LB else None,
    }


def net_return(entry: float, exit: float) -> float:
    """Round-trip return net of FEE on both sides. Mirrors quant.strategy_trades.net_ret:
    (exit_price / entry_price) * power(0.999, 2) - 1."""
    return (exit / entry) * (1 - FEE) ** 2 - 1
