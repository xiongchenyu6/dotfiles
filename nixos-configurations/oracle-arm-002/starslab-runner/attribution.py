"""Net movement attribution. Fees are included in basis and proceeds, never charged twice."""

from .fills import fee_value


def attribution(store, prices):
    groups = {}
    def group(strategy, asset):
        return groups.setdefault((strategy,asset),{'strategy':strategy,'asset':asset,
            'realized_pnl_usdt':0.0,'unrealized_pnl_usdt':0.0,'net_pnl_usdt':0.0,'fees_usdt':0.0})
    for row in store.holdings():
        if row['bought']<=0:
            continue
        quantity = max(0,row['quantity'])
        basis = row['cost']/row['bought']
        result = group(row['kind'],row['asset'])
        result['realized_pnl_usdt'] += row['proceeds']-(row['bought']-quantity)*basis
        if quantity>1e-12:
            result['unrealized_pnl_usdt'] += quantity*(prices[row['asset']]-basis)
    for row in store.db.execute("SELECT * FROM orders WHERE status='done'"):
        if row['amount']:
            group(row['kind'],row['asset'])['fees_usdt'] += fee_value(row)[0]
    for row in groups.values():
        row['net_pnl_usdt'] = row['realized_pnl_usdt']+row['unrealized_pnl_usdt']
    return [groups[key] for key in sorted(groups)]
