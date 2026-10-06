"""Timestamped owner journal valuation; deposits and withdrawals are not returns."""

import math
from datetime import datetime, timezone


def record_snapshot(store, sequence, observed_at, price_as_of, equity, cash, funded, fees):
    observed = datetime.fromisoformat(observed_at.replace('Z','+00:00'))
    price_time = datetime.fromisoformat(price_as_of.replace('Z','+00:00'))
    if observed.tzinfo is None or price_time.tzinfo is None or not 0<=(observed-price_time).total_seconds()<=10830:
        raise ValueError('Valuation timestamp is invalid or stale')
    values = (equity,cash,funded,fees)
    if any(type(v) not in (int,float) or not math.isfinite(v) or v<0 for v in values):
        raise ValueError('Invalid historical account amount')
    if type(sequence) is not int or sequence<1:
        raise ValueError('Invalid snapshot sequence')
    with store.transaction():
        store.db.execute('INSERT INTO equity_snapshots VALUES (?,?,?,?,?,?,?)',
            (sequence,observed.astimezone(timezone.utc).isoformat(),
             price_time.astimezone(timezone.utc).isoformat(),*values))


def history(store, limit=168):
    if type(limit) is not int or not 1<=limit<=1000:
        raise ValueError('Invalid history limit')
    # Latest observation in each UTC hour. Every minute remains in the private journal.
    rows = store.db.execute('''SELECT s.* FROM equity_snapshots s JOIN
        (SELECT max(sequence) sequence FROM equity_snapshots GROUP BY substr(observed_at,1,13)) h
        ON s.sequence=h.sequence ORDER BY s.observed_at DESC LIMIT ?''',(limit,)).fetchall()
    return {'valuation_source':'hourly_research_close','points':[
        {'observed_at':row['observed_at'],'price_as_of':row['price_as_of'],
         'equity_usdt':row['equity'],'cash_usdt':row['cash'],
         'net_contributions_usdt':row['net_funding'],'fees_usdt':row['fees'],
         'net_pnl_usdt':row['equity']-row['net_funding']}
        for row in reversed(rows)]}
