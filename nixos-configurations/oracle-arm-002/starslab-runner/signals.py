"""Validate public research data before a locally configured strategy uses it."""

from datetime import datetime, timezone
import math


def timestamp(value):
    if not isinstance(value, str):
        raise ValueError('Missing signal timestamp')
    parsed = datetime.fromisoformat(value.replace('Z', '+00:00'))
    if parsed.tzinfo is None:
        raise ValueError('Signal timestamp needs a timezone')
    return parsed.astimezone(timezone.utc)


def recent(value, now, max_age):
    age = (now-timestamp(value)).total_seconds()
    if not 0 <= age <= max_age:
        raise ValueError('Signal data is stale or from the future')


def fields(value, expected):
    if not isinstance(value, dict) or set(value) != set(expected):
        raise ValueError('Unexpected signal fields')


def positive(value):
    if isinstance(value, bool) or not isinstance(value, (float, int)):
        raise ValueError('Invalid signal number')
    if not math.isfinite(value) or value <= 0:
        raise ValueError('Invalid signal number')
    return value


def validate_snapshot(snapshot, assets, now=None):
    """A feed supplies targets, never keys, budgets, order instructions or code.

    The caller supplies its locally selected universe. Missing market rows prevent
    all execution, especially sales which could otherwise mistake missing data for
    an exit. An absent/currently unavailable DCA day skips DCA without fabricating it.
    """
    now = now or datetime.now(timezone.utc)
    fields(snapshot, ('version', 'server_time', 'assets', 'targets', 'dca'))
    if type(snapshot['version']) is not int or snapshot['version'] != 1:
        raise ValueError('Unsupported signal format')
    recent(snapshot['server_time'], now, 120)
    universe = set(assets)
    if not universe or not isinstance(snapshot['assets'], list) or len(snapshot['assets']) > 100:
        raise ValueError('Invalid signal universe')
    prices = {}
    for row in snapshot['assets']:
        fields(row, ('asset', 'last_close', 'last_ts', 'updated_at'))
        asset = row['asset']
        if not isinstance(asset, str) or asset in prices:
            raise ValueError('Duplicate or invalid market asset')
        recent(row['last_ts'], now, 10800)
        recent(row['updated_at'], now, 10800)
        prices[asset] = positive(row['last_close'])
    if not universe.issubset(prices):
        raise ValueError('Signal market data is incomplete')
    if not isinstance(snapshot['targets'], list) or len(snapshot['targets']) > 100:
        raise ValueError('Invalid target list')
    targets, ids = {}, set()
    for row in snapshot['targets']:
        fields(row, ('asset', 'id'))
        asset, signal_id = row['asset'], row['id']
        if not isinstance(asset, str) or asset not in prices or asset in targets:
            raise ValueError('Invalid or duplicate target asset')
        if type(signal_id) is not int or signal_id <= 0 or signal_id in ids:
            raise ValueError('Invalid or duplicate signal ID')
        targets[asset] = signal_id
        ids.add(signal_id)
    dca = snapshot['dca']
    if dca is not None:
        fields(dca, ('day', 'units', 'computed_at'))
        units = positive(dca['units'])
        if not 1 <= units <= 8:
            raise ValueError('DCA multiple exceeds local safety limit')
        recent(dca['computed_at'], now, 86400)
        if dca['day'] != now.date().isoformat():
            dca = None
    return {'prices': {a: prices[a] for a in universe},
            'targets': {a: i for a, i in targets.items() if a in universe}, 'dca': dca}
