"""Account-owner HTX adapter. Exchange methods are called only on this machine."""

import math
from .fills import deltas


def number(value):
    result = float(value)
    if not math.isfinite(result) or result <= 0:
        raise ValueError('Invalid exchange price or quantity')
    return result


class HTX:
    def __init__(self, exchange, mode, uid=None, account_id=None):
        if mode not in ('live','dry_run'):
            raise ValueError('Unsupported execution mode')
        self.ex, self.mode, self.uid, self.account_id = exchange, mode, uid, account_id
        if mode=='live':
            identity = self.ex.spot_private_get_v2_user_uid()
            accounts = self.ex.spot_private_get_v1_account_accounts()
            if identity.get('code') != 200 or str(identity.get('data')) != uid:
                raise ValueError('HTX account UID does not match local configuration')
            if accounts.get('status') != 'ok' or not any(str(row['id'])==account_id
                and row['type']=='spot' and row['state']=='working' for row in accounts.get('data', [])):
                raise ValueError('HTX spot account does not match local configuration')

    def balance(self):
        return self.ex.fetch_balance({'type':'spot','accountId':self.account_id})

    def check(self, store):
        from .account_lock import check
        check(store)
        if self.mode!='live':
            return
        balance = self.balance()['total']
        if any(not math.isfinite(float(value or 0)) or float(value or 0)<0 for value in balance.values()):
            raise ValueError('Invalid exchange balance')
        if float(balance.get('USDT') or 0)+.01 < store.cash():
            raise ValueError('Exchange cash is below journal cash')
        expected = {}
        for row in store.holdings():
            expected[row['asset']] = expected.get(row['asset'],0) + row['quantity']
        for asset in set(expected) | {a for a,q in balance.items() if a!='USDT' and q}:
            if not math.isclose(float(balance.get(asset) or 0),expected.get(asset,0),rel_tol=1e-7,abs_tol=1e-12):
                raise ValueError('Exchange holdings differ from the local journal')

    def reconcile(self, store):
        from .account_lock import check
        check(store)
        for row in store.pending():
            if self.mode != 'live':
                # Simulations never leave a remote ambiguous order. A crash after
                # reserving but before applying a simulated fill cancels that intent.
                store.finish(row['client_id'],0,0,0,0)
                continue
            cid, oid, symbol = row['client_id'], row['exchange_id'], row['asset']+'/USDT'
            order = self.ex.fetch_order(oid or cid,symbol,{} if oid else {'clientOrderId':cid})
            if not order.get('id') or order.get('symbol')!=symbol or order.get('side')!=row['side']:
                raise ValueError('Recovered order identity does not match')
            if order.get('clientOrderId') and order['clientOrderId']!=cid:
                raise ValueError('Recovered client order ID does not match')
            if oid and str(order['id'])!=oid:
                raise ValueError('Recovered exchange order ID does not match')
            store.set_exchange_id(cid,str(order['id']))
            if order.get('status') not in ('closed','canceled','expired','rejected'):
                continue
            trades = self.ex.fetch_order_trades(order['id'],symbol) if order.get('filled') else []
            qty,cash = deltas(order,trades)
            store.finish(cid,qty,cash,float(order.get('filled') or 0),
                float(order.get('cost') or 0) if order.get('filled') else 0)
        return not store.pending()

    def submit(self, store, kind, asset, side, action, position, allocation):
        import uuid
        from .account_lock import check
        check(store)
        symbol = asset+'/USDT'
        market = self.ex.market(symbol)
        if not market.get('spot') or market.get('active') is False:
            raise ValueError('Inactive spot market')
        ticker = self.ex.fetch_ticker(symbol)
        price = number(ticker.get('ask') if side=='buy' else ticker.get('bid'))
        minimum = float((market.get('limits',{}).get('cost') or {}).get('min') or 0)
        min_qty = float((market.get('limits',{}).get('amount') or {}).get('min') or 0)
        if any(not math.isfinite(v) or v<0 for v in (minimum,min_qty)):
            raise ValueError('Invalid exchange order limits')
        if self.mode=='live':
            fee = self.ex.fetch_trading_fee(symbol)
            rates = (fee.get('taker'),(fee.get('info') or {}).get('takerFeeRate',fee.get('taker')))
            if fee.get('symbol')!=symbol or any(v is None or not math.isfinite(float(v))
                or not 0<=float(v)<=.003 for v in rates):
                raise ValueError('Authenticated fee unavailable or above safety ceiling')
            free = self.balance()['free']
        else:
            free = {'USDT':store.cash(),asset:allocation}
            rates = (.002,.002)
        if any(not math.isfinite(float(v or 0)) or float(v or 0)<0 for v in free.values()):
            raise ValueError('Invalid available exchange balance')
        if side=='buy':
            allocation = min(allocation,float(free.get('USDT') or 0))
            cost = float(self.ex.cost_to_precision(symbol,allocation/1.003))
            qty = float(self.ex.amount_to_precision(symbol,cost/price))
        else:
            qty = float(self.ex.amount_to_precision(symbol,min(allocation,float(free.get(asset) or 0))))
            cost = qty*price
        if cost<minimum or qty<min_qty or qty<=0 or cost<=0:
            return False
        number(cost)
        number(qty)
        cid = 'q'+uuid.uuid4().hex[:30]
        if not store.reserve(cid,action,position,kind,asset,side,allocation if side=='buy' else qty,
                             float(rates[0]),float(rates[1])):
            return False
        if self.mode=='dry_run':
            # A simulation charges the conservative HTX basic taker rate locally.
            if side=='buy':
                cost = qty*price
                store.finish(cid,qty*.998,-cost,qty,cost)
            else:
                store.finish(cid,-qty,cost*.998,qty,cost)
            return True
        check(store)
        params = {'clientOrderId':cid,'account-id':self.account_id}
        order = self.ex.create_market_buy_order_with_cost(symbol,cost,params) if side=='buy' else self.ex.create_market_sell_order(symbol,qty,params)
        if order.get('id'):
            store.set_exchange_id(cid,str(order['id']))
        self.reconcile(store)
        return True
