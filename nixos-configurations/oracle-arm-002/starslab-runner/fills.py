"""Normalize final HTX spot matches, rejecting unaccounted fee currencies."""

import math


def deltas(order, trades):
    amount = float(order.get('filled') or 0)
    if order.get('filled') is None or not math.isfinite(amount) or amount < 0:
        raise ValueError('Invalid filled amount')
    if amount == 0:
        return 0.0, 0.0
    if order.get('side') not in ('buy','sell'):
        raise ValueError('Invalid order side')
    base, quote = order['symbol'].split('/')
    if not trades or not math.isclose(sum(float(t['amount']) for t in trades), amount,
                                     rel_tol=1e-8, abs_tol=1e-12):
        raise ValueError('Incomplete order trade details')
    base_fee = quote_fee = cost = 0.0
    for trade in trades:
        raw = trade.get('info') or {}
        points = float(raw.get('filled-points') or 0)
        if not math.isfinite(points) or points < 0 or raw.get('fee-deduct-state') == 'ongoing':
            raise ValueError('Fee deduction is not final')
        if points and (raw.get('fee-deduct-state') != 'done' or
            str(raw.get('fee-deduct-currency') or '').upper() not in (base, quote)):
            raise ValueError('Unsupported fee deduction')
        if not math.isfinite(float(trade['amount'])) or float(trade['amount']) <= 0:
            raise ValueError('Invalid match quantity')
        quote_cost = float(trade['cost'])
        if not math.isfinite(quote_cost) or quote_cost <= 0:
            raise ValueError('Invalid match cost')
        cost += quote_cost
        fees = trade.get('fees') or ([trade['fee']] if trade.get('fee') is not None else None)
        if not fees:
            raise ValueError('Missing actual fee information')
        for fee in fees:
            value = float(fee['cost'])
            if not math.isfinite(value) or value < 0:
                raise ValueError('Invalid fee')
            if fee.get('currency') == base:
                base_fee += value
            elif fee.get('currency') == quote:
                quote_fee += value
            elif value != 0:
                raise ValueError('Unsupported third-currency fee')
    if not math.isfinite(cost) or cost <= 0 or order.get('cost') is None or not math.isclose(
        cost, float(order['cost']), rel_tol=1e-8, abs_tol=1e-8):
        raise ValueError('Order and match costs disagree')
    if base_fee >= amount or quote_fee >= cost:
        raise ValueError('Invalid fee totals')
    return (amount-base_fee, -(cost+quote_fee)) if order['side']=='buy' else (-(amount+base_fee),cost-quote_fee)


def fee_value(row):
    amount, cost = row['amount'], row['cost']
    if not amount or not cost:
        return 0.0, 0.0
    base = amount-row['asset_delta'] if row['side']=='buy' else -row['asset_delta']-amount
    quote = -row['cash_delta']-cost if row['side']=='buy' else cost-row['cash_delta']
    value = max(0, base)*cost/amount + max(0, quote)
    return value, value/cost
