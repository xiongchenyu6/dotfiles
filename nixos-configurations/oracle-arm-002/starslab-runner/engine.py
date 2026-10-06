"""Apply public research targets under settings chosen locally by the account owner."""

import calendar
from datetime import datetime, timezone
import uuid
from .signals import validate_snapshot
from .config import validate, machine_identity


def tick(store, venue, config, snapshot, now=None, decisions=None):
    decisions = [] if decisions is None else decisions
    def note(strategy, asset, reason):
        decisions.append({'strategy':strategy,'asset':asset,'reason':reason})
    validate(config)
    if config['mode']=='live' and config['account_lock_owner']['machine_id']!=machine_identity():
        raise RuntimeError('Live execution belongs to the designated owner server')
    if venue.mode != config['mode']:
        raise ValueError('Exchange mode differs from local authorization')
    now = now or datetime.now(timezone.utc)
    if not venue.reconcile(store):
        note('account',None,'pending_reconciliation')
        return 'pending'
    venue.check(store)
    held_assets = {row['asset'] for row in store.holdings() if row['quantity']>1e-12}
    data = validate_snapshot(snapshot,set(config['assets']) | held_assets,now)
    month = now.strftime('%Y-%m-01')
    targets = data['targets']
    # Existing trend holdings still get exits when new entries are locally disabled.
    for row in store.holdings():
        current_position = f"signal:{targets[row['asset']]}" if row['asset'] in targets else None
        if row['kind']=='trend' and row['quantity']>1e-12 and row['position']!=current_position:
            submitted = venue.submit(store,'trend',row['asset'],'sell',f"exit:{row['position']}:{uuid.uuid4().hex}",
                row['position'],row['quantity'])
            note('trend',row['asset'],'exit_submitted' if submitted else 'below_exchange_minimum')
            if store.pending():
                return 'pending'
    if config['trend']:
        for asset in config['assets']:
            if asset not in targets:
                note('trend',asset,'no_entry_signal')
                continue
            if store.exists(f'entry:{targets[asset]}'):
                note('trend',asset,'target_already_processed')
                continue
            allocation = min(config['order_usdt'],store.budget('trend',month),store.cash())
            if allocation>0:
                submitted = venue.submit(store,'trend',asset,'buy',f'entry:{targets[asset]}',
                    f'signal:{targets[asset]}',allocation)
                note('trend',asset,'entry_submitted' if submitted else 'below_exchange_minimum')
                if store.pending():
                    return 'pending'
            else:
                note('trend',asset,'confirmed_budget_unavailable')
    else:
        note('trend',None,'entries_disabled')
    if config['dca'] and data['dca'] is not None and not store.exists('dca:'+now.date().isoformat()):
        allocation = min(config['monthly_dca_usdt']/calendar.monthrange(now.year,now.month)[1]*data['dca']['units'],
            store.budget('dca',month),store.cash())
        if allocation>0:
            submitted = venue.submit(store,'dca','BTC','buy','dca:'+now.date().isoformat(),'DCA-BTC',allocation)
            note('dca','BTC','entry_submitted' if submitted else 'below_exchange_minimum')
            if store.pending():
                return 'pending'
        else:
            note('dca','BTC','confirmed_budget_unavailable')
    else:
        reason = 'entries_disabled' if not config['dca'] else ('no_dca_signal' if data['dca'] is None else 'today_already_processed')
        note('dca','BTC',reason)
    return 'healthy'
