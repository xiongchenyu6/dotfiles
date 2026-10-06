"""Estimated observed-period performance from local valuations and timed cash flows.

Modified Dietz: (end - start - flows) / (start + time-weighted flows).
The first recorded valuation is the baseline, never an invented inception value.
Equity already reflects incurred fees; cumulative fees must not be deducted again.
"""

from datetime import datetime, timezone
import math


def _timestamp(value):
    stamp = datetime.fromisoformat(value)
    if stamp.tzinfo is None:
        raise ValueError('Timestamp needs a timezone')
    return stamp.astimezone(timezone.utc)


def observed_period_return(store):
    """Return a nullable, non-annualized percentage; perform no journal writes.

    Confirmation timestamps weight flows by the remaining seconds in the period.
    This approximates performance, not an exact time-weighted return or a claim of
    GIPS compliance. Untimed immutable nonnegative legacy funding may establish
    opening capital only when it fully reconciles to the first recorded funding
    balance. It receives no synthetic timestamp and never enters period flows.
    """
    result = dict(method='modified_dietz', estimated=True, start_at=None,
                  end_at=None, return_pct=None, unavailable_reason=None)

    def unavailable(reason):
        result['unavailable_reason'] = reason
        return result

    snapshots = list(store.db.execute(
        'SELECT observed_at,equity,net_funding FROM equity_snapshots ORDER BY sequence'))
    if snapshots:
        result.update(start_at=snapshots[0][0], end_at=snapshots[-1][0])
    if len(snapshots) < 2:
        return unavailable('insufficient_observations')
    try:
        valuations = [(_timestamp(row[0]), float(row[1]), float(row[2]))
                      for row in snapshots]
        result.update(start_at=valuations[0][0].isoformat(),end_at=valuations[-1][0].isoformat())
        if any(not math.isfinite(equity) or not math.isfinite(funding)
               for _, equity, funding in valuations):
            return unavailable('invalid_valuation')
        if any(b[0] <= a[0] for a, b in zip(valuations, valuations[1:])):
            result.update(start_at=None,end_at=None)
            return unavailable('invalid_observation_times')
        flows = []
        legacy = []
        for month, trend, dca in store.db.execute('SELECT month,trend,dca FROM funding'):
            amount = float(trend) + float(dca)
            if amount == 0:
                continue
            row = store.db.execute('SELECT value FROM metadata WHERE key=?',
                                   ('funded-at:' + month,)).fetchone()
            if row is None:
                if not math.isfinite(amount) or min(float(trend), float(dca)) < 0:
                    return unavailable('invalid_cash_flow')
                legacy.append(amount)
            else:
                flows.append((_timestamp(row[0]), amount))
        for amount, created_at in store.db.execute(
                'SELECT cash_delta,created_at FROM cash_flows WHERE cash_delta != 0'):
            flows.append((_timestamp(created_at), float(amount)))
        if any(not math.isfinite(amount) for _, amount in flows):
            return unavailable('invalid_cash_flow')
    except (ValueError, TypeError, OverflowError):
        result.update(start_at=None,end_at=None)
        return unavailable('invalid_timestamps_or_amounts')

    start, beginning, opening_funding = valuations[0]
    opening_known = math.fsum(amount for time, amount in flows if time <= start)
    opening_total = math.fsum(legacy) + opening_known
    if not math.isclose(opening_total, opening_funding, rel_tol=1e-9, abs_tol=1e-7):
        return unavailable('unknown_flow_timing' if legacy else 'unreconciled_cash_flows')
    # Check every recorded funding balance: an unexplained or mistimed movement
    # must not turn into apparent investment profit (even if later offset).
    for stamp, _, funding in valuations:
        known = opening_total + math.fsum(
            amount for time, amount in flows if start < time <= stamp)
        if not math.isclose(known, funding, rel_tol=1e-9, abs_tol=1e-7):
            return unavailable('unreconciled_cash_flows')
    end, ending, _ = valuations[-1]
    period = (end - start).total_seconds()
    during = [(time, amount) for time, amount in flows if start < time <= end]
    denominator = beginning + math.fsum(
        amount * (end - time).total_seconds() / period for time, amount in during)
    if not math.isfinite(denominator) or denominator <= 0:
        return unavailable('nonpositive_capital')
    percentage = (ending - beginning - math.fsum(amount for _, amount in during)) / denominator * 100
    if not math.isfinite(percentage):
        return unavailable('invalid_return')
    result['return_pct'] = percentage
    return result
